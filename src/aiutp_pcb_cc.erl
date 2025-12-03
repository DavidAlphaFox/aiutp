%%------------------------------------------------------------------------------
%% @doc uTP 协议控制块的拥塞控制
%%
%% 本模块实现 BEP-29 规范中的 LEDBAT（低额外延迟后台传输）拥塞控制算法。
%%
%% 主要函数：
%% - cc_control/4: LEDBAT 拥塞控制主逻辑
%% - maybe_decay_win/1: 超时时窗口衰减
%% - ack_packet/3: 处理已确认的包以计算 RTT
%% - caculate_acked_bytes/4: 计算已确认字节总数和最小 RTT
%% - selective_ack_packet/3: 处理选择性确认（SACK）
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
%% @doc LEDBAT 拥塞控制算法
%%
%% 根据相对于目标的测量延迟调整拥塞窗口。
%% 实现慢启动和拥塞避免两个阶段。
%%
%% @param Now 当前时间戳（毫秒）
%% @param AckedBytes 已确认字节数
%% @param RTT 测量的往返时间
%% @param PCB 协议控制块
%% @returns 更新窗口大小后的 PCB
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

    %% 目标延迟默认为 100ms（如未设置）
    Target =
        if TargetDelay =< 0 -> 100000;
           true -> TargetDelay
        end,

    %% 时钟漂移补偿惩罚
    Penalty =
        if ClockDrift < -200000 -> (200000 + ClockDrift) div 7;
           true -> 0
        end,

    OurDelay0 = OurDelay + Penalty,
    OffTarget = Target - OurDelay0,

    %% 计算窗口因子和延迟因子
    Win0 = erlang:min(AckedBytes, MaxWindow),
    Win1 = erlang:max(AckedBytes, MaxWindow),
    WindowFactor = Win0 / Win1,
    DelayFactor = OffTarget / Target,

    %% 计算窗口调整的缩放增益
    ScaledGain = ?MAX_CWND_INCREASE_BYTES_PER_RTT * WindowFactor * DelayFactor,
    ScaledGain0 =
        if (ScaledGain > 0) and (Now - LastMaxedOutWindow > 3000) -> 0;
           true -> erlang:trunc(ScaledGain)
        end,

    %% LEDBAT 拥塞窗口
    LedbetCwnd = erlang:max(?MIN_WINDOW_SIZE, (MaxWindow + ScaledGain0)),

    %% 慢启动或拥塞避免
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
%% @doc 不活跃时窗口衰减
%%
%% 如果自上次衰减以来已过去足够时间，则将拥塞窗口减少 20%。
%% 用于防止陈旧的窗口大小。
%%
%% @param PCB 协议控制块
%% @returns 可能衰减窗口后的更新 PCB
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
%% @doc 处理已确认的包以计算 RTT
%%
%% 根据已确认的包更新 RTT 估计值和当前窗口。
%% 仅使用首次传输来计算 RTT（Karn 算法）。
%%
%% @param MicroNow 当前时间（微秒）
%% @param WrapPacket 已确认的包装包
%% @param Acc 累加器元组 {Now, CurWindow, RTT, RTO, RTTVar, RTTHist}
%% @returns 更新后的累加器
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
            %% 仅对首次传输计算 RTT（Karn 算法）
            {RTT0, RTTVar0, ERTT} = aiutp_rtt:caculate_rtt(RTT, RTTVar, TimeSent, MicroNow),
            RTTHist0 =
                if RTT /= 0 -> aiutp_delay:add_sample(ERTT, Now, RTTHist);
                   true -> RTTHist
                end,
            {RTT0, RTTVar0, aiutp_util:clamp((RTT0 + RTTVar0 * 4), 600, 6000), RTTHist0};
           true -> {RTT, RTTVar, RTO, RTTHist}
        end,

    %% 调整当前窗口（不计算重发的包）
    CurWindow0 =
        if NeedResend == false -> CurWindow - Payload;
           true -> CurWindow
        end,

    {Now, CurWindow0, RTT1, RTO0, RTTVar1, RTTHist1}.

%%------------------------------------------------------------------------------
%% @doc 计算已确认字节总数和最小 RTT
%%
%% 处理已确认和选择性确认的包列表，
%% 计算拥塞控制所需的总字节数和最小 RTT。
%%
%% @param Acc 初始累加器 {Bytes, RTT}
%% @param Now 当前时间戳
%% @param AckedPackets 正常确认的包列表
%% @param SAckedPackets 选择性确认的包列表
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
%% @doc 处理选择性确认（SACK）包
%%
%% 处理选择性确认的包，更新 RTT 估计值，
%% 并在需要时触发快速重传。
%%
%% 注意：当前实现与 C++ 版本相比可能浪费带宽。
%% 这是未来优化的已知限制。
%%
%% @param SAckedPackets 选择性确认的包列表
%% @param MicroNow 当前时间（微秒）
%% @param PCB 协议控制块
%% @returns 更新后的 PCB
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

    %% 从 SACK 的包更新 RTT 和窗口
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

    %% 计算快速重传的序列号范围
    [El | _] = SAckedPackets,
    MinSeq = aiutp_util:bit16(SeqNR - CurWindowPackets),
    Packet = El#aiutp_packet_wrap.packet,
    MaxSeq = aiutp_util:bit16(Packet#aiutp_packet.seq_nr - 1),

    if ?WRAPPING_DIFF_16(MaxSeq, MinSeq) > ?DUPLICATE_ACKS_BEFORE_RESEND ->
        %% 触发快速重传
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
