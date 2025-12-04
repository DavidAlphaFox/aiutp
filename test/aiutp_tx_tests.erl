%%------------------------------------------------------------------------------
%% @doc Unit tests for aiutp_tx module
%%
%% Tests SACK processing and packet transmission logic.
%% @end
%%------------------------------------------------------------------------------
-module(aiutp_tx_tests).

-include_lib("eunit/include/eunit.hrl").
-include("aiutp.hrl").

%%==============================================================================
%% Test: map_sack_to_seq
%%==============================================================================

map_sack_to_seq_empty_test() ->
    %% Empty extension list should return empty
    Result = aiutp_tx:map_sack_to_seq([], 100),
    ?assertEqual([], Result).

map_sack_to_seq_single_bit_test() ->
    %% SACK with single bit set at position 0
    %% Base = 100, bit 0 set means seq 100 is acknowledged
    SAck = <<1>>,
    Result = aiutp_tx:map_sack_to_seq([{sack, SAck}], 100),
    ?assert(lists:member(100, Result)).

map_sack_to_seq_multiple_bits_test() ->
    %% SACK with bits 0, 2, 4 set (binary: 00010101 = 21)
    %% Base = 100 means seq 100, 102, 104 are acknowledged
    SAck = <<21>>,
    Result = aiutp_tx:map_sack_to_seq([{sack, SAck}], 100),
    ?assert(lists:member(100, Result)),
    ?assert(lists:member(102, Result)),
    ?assert(lists:member(104, Result)),
    ?assertNot(lists:member(101, Result)),
    ?assertNot(lists:member(103, Result)).

map_sack_to_seq_multi_byte_test() ->
    %% SACK with multiple bytes
    %% Byte 0: bit 0 set (seq 100)
    %% Byte 1: bit 0 set (seq 108)
    SAck = <<1, 1>>,
    Result = aiutp_tx:map_sack_to_seq([{sack, SAck}], 100),
    ?assert(lists:member(100, Result)),
    ?assert(lists:member(108, Result)).

%%==============================================================================
%% Test: update_skip_counts
%%==============================================================================

update_skip_counts_empty_sack_test() ->
    %% Empty SACK list should not change anything
    PCB = create_test_pcb_with_outbuf([]),
    {SkippedCount, PCB1} = aiutp_tx:update_skip_counts([], PCB),
    ?assertEqual(0, SkippedCount),
    ?assertEqual(PCB, PCB1).

update_skip_counts_no_gaps_test() ->
    %% All packets are SACK'd, no gaps
    PCB = create_test_pcb_with_packets([100, 101, 102]),
    {SkippedCount, _PCB1} = aiutp_tx:update_skip_counts([100, 101, 102], PCB),
    ?assertEqual(0, SkippedCount).

update_skip_counts_increments_skip_count_test() ->
    %% Packet 100 is not SACK'd but 101 is, so 100 should have skip_count incremented
    PCB = create_test_pcb_with_packets([100, 101]),
    {SkippedCount, PCB1} = aiutp_tx:update_skip_counts([101], PCB),
    %% First skip, should not trigger resend yet
    ?assertEqual(0, SkippedCount),
    %% Verify skip_count was incremented
    OutBuf = PCB1#aiutp_pcb.outbuf,
    Iter = aiutp_buffer:head(OutBuf),
    WrapPacket = aiutp_buffer:data(Iter, OutBuf),
    ?assertEqual(1, WrapPacket#aiutp_packet_wrap.skip_count).

update_skip_counts_triggers_resend_test() ->
    %% After 3 skips, packet should be marked for resend
    PCB0 = create_test_pcb_with_packets_and_skip_count([100, 101], 2),
    %% This is the 3rd skip, should trigger resend
    {SkippedCount, PCB1} = aiutp_tx:update_skip_counts([101], PCB0),
    ?assertEqual(1, SkippedCount),
    %% Verify need_resend was set
    OutBuf = PCB1#aiutp_pcb.outbuf,
    Iter = aiutp_buffer:head(OutBuf),
    WrapPacket = aiutp_buffer:data(Iter, OutBuf),
    ?assertEqual(true, WrapPacket#aiutp_packet_wrap.need_resend),
    ?assertEqual(3, WrapPacket#aiutp_packet_wrap.skip_count).

update_skip_counts_exports_test() ->
    %% Verify update_skip_counts is exported
    Exports = aiutp_tx:module_info(exports),
    ?assert(lists:member({update_skip_counts, 2}, Exports)).

%%==============================================================================
%% Helper functions
%%==============================================================================

create_test_pcb_with_outbuf(OutBuf) ->
    Now = aiutp_util:millisecond(),
    #aiutp_pcb{
        time = Now,
        state = ?CS_CONNECTED,
        conn_id_recv = 12345,
        conn_id_send = 12346,
        max_window = ?PACKET_SIZE * 10,
        max_window_user = ?PACKET_SIZE * 255,
        cur_window = 0,
        cur_window_packets = 0,
        inbuf = aiutp_buffer:new(?OUTGOING_BUFFER_MAX_SIZE),
        outbuf = OutBuf,
        inque = aiutp_queue:new(),
        outque = aiutp_queue:new(),
        our_hist = aiutp_delay:new(Now),
        their_hist = aiutp_delay:new(Now),
        rtt_hist = aiutp_delay:new(Now)
    }.

create_test_pcb_with_packets(SeqNRs) ->
    create_test_pcb_with_packets_and_skip_count(SeqNRs, 0).

create_test_pcb_with_packets_and_skip_count(SeqNRs, SkipCount) ->
    OutBuf0 = aiutp_buffer:new(?OUTGOING_BUFFER_MAX_SIZE),
    OutBuf = lists:foldl(
        fun(SeqNR, Buf) ->
            Packet = #aiutp_packet{seq_nr = SeqNR, type = ?ST_DATA},
            WrapPacket = #aiutp_packet_wrap{
                packet = Packet,
                transmissions = 1,  %% Already sent once
                payload = 100,
                skip_count = SkipCount
            },
            aiutp_buffer:append(WrapPacket, Buf)
        end,
        OutBuf0,
        SeqNRs),
    create_test_pcb_with_outbuf(OutBuf).

%%==============================================================================
%% Test: MTU 探测包丢失检测
%%==============================================================================

mtu_probe_lost_test_() ->
    {"MTU 探测包丢失检测测试",
     [{"探测包被 SACK 跳过时调用 on_probe_lost",
       fun() ->
           %% 创建带有 MTU 探测包的 PCB
           %% 探测包 seq_nr=100，后续包 seq_nr=101 被 SACK 确认
           PCB = create_test_pcb_with_mtu_probe(100, 101),
           %% SACK 确认 101，跳过 100 三次（触发丢包）
           PCB1 = set_skip_count(100, ?DUPLICATE_ACKS_BEFORE_RESEND - 1, PCB),
           %% 这次 SACK 会触发探测包丢失
           {_SkippedCount, PCB2} = aiutp_tx:update_skip_counts([101], PCB1),
           %% MTU 探测应该被处理
           ?assertEqual(0, PCB2#aiutp_pcb.mtu_probe_seq),
           ?assertEqual(999, PCB2#aiutp_pcb.mtu_ceiling)  %% probe_size - 1
       end},
      {"非探测包不影响 MTU 状态",
       fun() ->
           %% 创建普通包（非探测）
           PCB = create_test_pcb_with_packets([100, 101]),
           PCB1 = PCB#aiutp_pcb{
               mtu_probe_seq = 200,  %% 探测包是 200，不是 100
               mtu_probe_size = 1000,
               mtu_ceiling = ?MTU_CEILING_DEFAULT
           },
           PCB2 = set_skip_count(100, ?DUPLICATE_ACKS_BEFORE_RESEND - 1, PCB1),
           {_SkippedCount, PCB3} = aiutp_tx:update_skip_counts([101], PCB2),
           %% MTU 状态不应改变
           ?assertEqual(200, PCB3#aiutp_pcb.mtu_probe_seq),
           ?assertEqual(?MTU_CEILING_DEFAULT, PCB3#aiutp_pcb.mtu_ceiling)
       end},
      {"未达到阈值时不触发丢失",
       fun() ->
           PCB = create_test_pcb_with_mtu_probe(100, 101),
           %% 只跳过一次，不触发丢失
           {_SkippedCount, PCB1} = aiutp_tx:update_skip_counts([101], PCB),
           %% MTU 探测状态应该保持
           ?assertEqual(100, PCB1#aiutp_pcb.mtu_probe_seq),
           ?assertEqual(?MTU_CEILING_DEFAULT, PCB1#aiutp_pcb.mtu_ceiling)
       end}
     ]}.

%% 创建带 MTU 探测包的 PCB
create_test_pcb_with_mtu_probe(ProbeSeq, OtherSeq) ->
    OutBuf0 = aiutp_buffer:new(?OUTGOING_BUFFER_MAX_SIZE),
    %% 探测包
    ProbePacket = #aiutp_packet{seq_nr = ProbeSeq, type = ?ST_DATA},
    ProbeWrap = #aiutp_packet_wrap{
        packet = ProbePacket,
        transmissions = 1,
        payload = 1000,
        skip_count = 0,
        is_mtu_probe = true
    },
    OutBuf1 = aiutp_buffer:append(ProbeWrap, OutBuf0),
    %% 普通包
    OtherPacket = #aiutp_packet{seq_nr = OtherSeq, type = ?ST_DATA},
    OtherWrap = #aiutp_packet_wrap{
        packet = OtherPacket,
        transmissions = 1,
        payload = 100,
        skip_count = 0,
        is_mtu_probe = false
    },
    OutBuf2 = aiutp_buffer:append(OtherWrap, OutBuf1),
    PCB = create_test_pcb_with_outbuf(OutBuf2),
    PCB#aiutp_pcb{
        mtu_probe_seq = ProbeSeq,
        mtu_probe_size = 1000,
        mtu_floor = ?MTU_FLOOR_DEFAULT,
        mtu_ceiling = ?MTU_CEILING_DEFAULT,
        mtu_probe_failures = 0
    }.

%% 设置指定序列号包的 skip_count
set_skip_count(SeqNR, SkipCount, #aiutp_pcb{outbuf = OutBuf} = PCB) ->
    OutBuf1 = set_skip_count_in_buffer(SeqNR, SkipCount, aiutp_buffer:head(OutBuf), OutBuf),
    PCB#aiutp_pcb{outbuf = OutBuf1}.

set_skip_count_in_buffer(_SeqNR, _SkipCount, -1, OutBuf) ->
    OutBuf;
set_skip_count_in_buffer(SeqNR, SkipCount, Iter, OutBuf) ->
    WrapPacket = aiutp_buffer:data(Iter, OutBuf),
    Packet = WrapPacket#aiutp_packet_wrap.packet,
    case Packet#aiutp_packet.seq_nr of
        SeqNR ->
            WrapPacket1 = WrapPacket#aiutp_packet_wrap{skip_count = SkipCount},
            aiutp_buffer:replace(Iter, WrapPacket1, OutBuf);
        _ ->
            Next = aiutp_buffer:next(Iter, OutBuf),
            set_skip_count_in_buffer(SeqNR, SkipCount, Next, OutBuf)
    end.
