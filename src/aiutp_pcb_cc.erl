%%------------------------------------------------------------------------------
%% @doc Congestion Control for uTP Protocol Control Block
%%
%% This module implements LEDBAT (Low Extra Delay Background Transport)
%% congestion control algorithm as specified in BEP-29.
%%
%% Key functions:
%% - cc_control/4: Main LEDBAT congestion control logic
%% - maybe_decay_win/1: Window decay on timeout
%% - ack_packet/3: Process acknowledged packet for RTT calculation
%% - caculate_acked_bytes/4: Calculate total acknowledged bytes and min RTT
%% - selective_ack_packet/3: Handle selective ACK (SACK) processing
%% @end
%%------------------------------------------------------------------------------
-module(aiutp_pcb_cc).
-include("aiutp.hrl").

-export([cc_control/4,
         maybe_decay_win/1,
         ack_packet/3,
         caculate_acked_bytes/4,
         selective_ack_packet/3]).

%%------------------------------------------------------------------------------
%% @doc LEDBAT congestion control algorithm
%%
%% Adjusts the congestion window based on measured delay relative to target.
%% Implements slow start and congestion avoidance phases.
%%
%% @param Now Current timestamp in milliseconds
%% @param AckedBytes Number of bytes acknowledged
%% @param RTT Measured round-trip time
%% @param PCB Protocol control block
%% @returns Updated PCB with new window size
%% @end
%%------------------------------------------------------------------------------
-spec cc_control(integer(), integer(), integer(), #aiutp_pcb{}) -> #aiutp_pcb{}.
cc_control(Now, AckedBytes, RTT,
           #aiutp_pcb{our_hist = OurHist, target_delay = TargetDelay,
                      clock_drift = ClockDrift, max_window = MaxWindow,
                      last_maxed_out_window = LastMaxedOutWindow,
                      slow_start = SlowStart, ssthresh = SSThresh} = PCB) ->
    OurHistValue = aiutp_delay:value(OurHist),
    OurDelay = erlang:min(aiutp_util:bit32(RTT), OurHistValue),

    %% Target delay defaults to 100ms if not set
    Target =
        if TargetDelay =< 0 -> 100000;
           true -> TargetDelay
        end,

    %% Clock drift compensation penalty
    Penalty =
        if ClockDrift < -200000 -> (200000 + ClockDrift) div 7;
           true -> 0
        end,

    OurDelay0 = OurDelay + Penalty,
    OffTarget = Target - OurDelay0,

    %% Calculate window and delay factors
    Win0 = erlang:min(AckedBytes, MaxWindow),
    Win1 = erlang:max(AckedBytes, MaxWindow),
    WindowFactor = Win0 / Win1,
    DelayFactor = OffTarget / Target,

    %% Calculate scaled gain for window adjustment
    ScaledGain = ?MAX_CWND_INCREASE_BYTES_PER_RTT * WindowFactor * DelayFactor,
    ScaledGain0 =
        if (ScaledGain > 0) and (Now - LastMaxedOutWindow > 3000) -> 0;
           true -> erlang:trunc(ScaledGain)
        end,

    %% LEDBAT congestion window
    LedbetCwnd = erlang:max(?MIN_WINDOW_SIZE, (MaxWindow + ScaledGain0)),

    %% Slow start or congestion avoidance
    {SlowStart0, SSThresh0, MaxWindow0} =
        if SlowStart ->
            SSCwnd = MaxWindow + erlang:trunc(WindowFactor * ?MIN_WINDOW_SIZE),
            if SSCwnd > SSThresh -> {false, SSThresh, MaxWindow};
               OurDelay0 > Target * 0.9 -> {false, MaxWindow, MaxWindow};
               true -> {SlowStart, SSThresh, erlang:max(SSCwnd, LedbetCwnd)}
            end;
           true -> {SlowStart, SSThresh, LedbetCwnd}
        end,

    PCB#aiutp_pcb{
        slow_start = SlowStart0,
        ssthresh = SSThresh0,
        target_delay = (Target * 3 + OurDelay0) div 4,
        max_window = aiutp_util:clamp(MaxWindow0, ?MIN_WINDOW_SIZE,
                                      ?OUTGOING_BUFFER_MAX_SIZE * ?PACKET_SIZE)
    }.

%%------------------------------------------------------------------------------
%% @doc Decay window on inactivity
%%
%% Reduces the congestion window by 20% if enough time has passed since
%% the last decay. Used to prevent stale window sizes.
%%
%% @param PCB Protocol control block
%% @returns Updated PCB with potentially decayed window
%% @end
%%------------------------------------------------------------------------------
-spec maybe_decay_win(#aiutp_pcb{}) -> #aiutp_pcb{}.
maybe_decay_win(#aiutp_pcb{time = Now,
                           max_window = MaxWindow,
                           last_rwin_decay = LastRWinDecay} = PCB) ->
    if (Now - LastRWinDecay) < ?MAX_WINDOW_DECAY -> PCB;
       true ->
           MaxWindow0 = erlang:trunc(MaxWindow * 0.8),
           MaxWindow1 =
               if MaxWindow0 < ?MIN_WINDOW_SIZE -> ?MIN_WINDOW_SIZE;
                  true -> MaxWindow0
               end,
           PCB#aiutp_pcb{
               slow_start = false,
               ssthresh = MaxWindow1,
               max_window = MaxWindow1,
               last_rwin_decay = Now
           }
    end.

%%------------------------------------------------------------------------------
%% @doc Process acknowledged packet for RTT calculation
%%
%% Updates RTT estimates and current window based on acknowledged packet.
%% Only uses first transmission for RTT calculation (Karn's algorithm).
%%
%% @param MicroNow Current time in microseconds
%% @param WrapPacket The acknowledged packet wrapper
%% @param Acc Accumulator tuple {Now, CurWindow, RTT, RTO, RTTVar, RTTHist}
%% @returns Updated accumulator
%% @end
%%------------------------------------------------------------------------------
-spec ack_packet(integer(), #aiutp_packet_wrap{}, tuple()) -> tuple().
ack_packet(MicroNow,
           #aiutp_packet_wrap{transmissions = Transmissions,
                              time_sent = TimeSent,
                              need_resend = NeedResend,
                              payload = Payload},
           {Now, CurWindow, RTT, RTO, RTTVar, RTTHist}) ->
    {RTT1, RTTVar1, RTO0, RTTHist1} =
        if Transmissions == 1 ->
            %% Only calculate RTT for first transmission (Karn's algorithm)
            {RTT0, RTTVar0, ERTT} = aiutp_rtt:caculate_rtt(RTT, RTTVar, TimeSent, MicroNow),
            RTTHist0 =
                if RTT /= 0 -> aiutp_delay:add_sample(ERTT, Now, RTTHist);
                   true -> RTTHist
                end,
            {RTT0, RTTVar0, aiutp_util:clamp((RTT0 + RTTVar0 * 4), 600, 6000), RTTHist0};
           true -> {RTT, RTTVar, RTO, RTTHist}
        end,

    %% Adjust current window (don't count resent packets)
    CurWindow0 =
        if NeedResend == false -> CurWindow - Payload;
           true -> CurWindow
        end,

    {Now, CurWindow0, RTT1, RTO0, RTTVar1, RTTHist1}.

%%------------------------------------------------------------------------------
%% @doc Calculate total acknowledged bytes and minimum RTT
%%
%% Processes lists of acknowledged and selectively acknowledged packets
%% to compute total bytes and minimum RTT for congestion control.
%%
%% @param Acc Initial accumulator {Bytes, RTT}
%% @param Now Current timestamp
%% @param AckedPackets List of normally acknowledged packets
%% @param SAckedPackets List of selectively acknowledged packets
%% @returns {TotalBytes, MinRTT}
%% @end
%%------------------------------------------------------------------------------
-spec caculate_acked_bytes(tuple(), integer(), list(), list()) -> tuple().
caculate_acked_bytes(Acc, Now, AckedPackets, SAckedPackets) ->
    Fun = fun(WrapPacket, {Bytes, RTT}) ->
              TimeSent = WrapPacket#aiutp_packet_wrap.time_sent,
              RTT0 =
                  if TimeSent < Now -> erlang:min(RTT, (Now - TimeSent));
                     true -> erlang:min(RTT, 50000)
                  end,
              {Bytes + WrapPacket#aiutp_packet_wrap.payload, RTT0}
          end,
    Acc0 = lists:foldl(Fun, Acc, AckedPackets),
    lists:foldl(Fun, Acc0, SAckedPackets).

%%------------------------------------------------------------------------------
%% @doc Process selective ACK (SACK) packets
%%
%% Handles selectively acknowledged packets, updating RTT estimates and
%% triggering fast retransmit if needed.
%%
%% Note: Current implementation may waste bandwidth compared to C++ version.
%% This is a known limitation for future optimization.
%%
%% @param SAckedPackets List of selectively acknowledged packets
%% @param MicroNow Current time in microseconds
%% @param PCB Protocol control block
%% @returns Updated PCB
%% @end
%%------------------------------------------------------------------------------
-spec selective_ack_packet(list(), integer(), #aiutp_pcb{}) -> #aiutp_pcb{}.
selective_ack_packet(_, _, #aiutp_pcb{cur_window_packets = CurWindowPackets} = PCB)
  when CurWindowPackets == 0 -> PCB;
selective_ack_packet([], _, PCB) -> PCB;
selective_ack_packet(SAckedPackets,
                     MicroNow,
                     #aiutp_pcb{seq_nr = SeqNR,
                                cur_window_packets = CurWindowPackets} = PCB) ->
    Now0 = aiutp_util:millisecond(),

    %% Update RTT and window from SACK'd packets
    {_, CurWindow0, RTT0, RTO0, RTTVar0, RTTHist0} =
        lists:foldr(
            fun(I, AccPCB) -> ack_packet(MicroNow, I, AccPCB) end,
            {Now0, PCB#aiutp_pcb.cur_window, PCB#aiutp_pcb.rtt, PCB#aiutp_pcb.rto,
             PCB#aiutp_pcb.rtt_var, PCB#aiutp_pcb.rtt_hist},
            SAckedPackets),

    PCB0 = PCB#aiutp_pcb{
        cur_window = CurWindow0,
        rtt = RTT0,
        rtt_var = RTTVar0,
        rtt_hist = RTTHist0,
        rto = RTO0,
        retransmit_count = 0,
        retransmit_timeout = RTO0,
        rto_timeout = RTO0 + Now0
    },

    %% Calculate sequence range for fast retransmit
    [El | _] = SAckedPackets,
    MinSeq = aiutp_util:bit16(SeqNR - CurWindowPackets),
    Packet = El#aiutp_packet_wrap.packet,
    MaxSeq = aiutp_util:bit16(Packet#aiutp_packet.seq_nr - 1),

    if ?WRAPPING_DIFF_16(MaxSeq, MinSeq) > ?DUPLICATE_ACKS_BEFORE_RESEND ->
        %% Trigger fast retransmit
        {Sent, LastSeq, PCB1} = aiutp_net:send_n_packets(MinSeq, MaxSeq, 4, PCB0),
        PCB2 = PCB1#aiutp_pcb{
            fast_resend_seq_nr = aiutp_util:bit16(LastSeq + 1),
            duplicate_ack = erlang:length(SAckedPackets)
        },
        if Sent > 0 -> maybe_decay_win(PCB2);
           true -> PCB2
        end;
       true -> PCB0
    end.
