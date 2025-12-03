-module(aiutp_packet_tests).

-include_lib("eunit/include/eunit.hrl").
-include("aiutp.hrl").

%%====================================================================
%% Test: Packet creation functions
%%====================================================================

reset_packet_test() ->
    Pkt = aiutp_packet:reset(12345, 100),
    ?assertEqual(?ST_RESET, Pkt#aiutp_packet.type),
    ?assertEqual(12345, Pkt#aiutp_packet.conn_id),
    ?assertEqual(100, Pkt#aiutp_packet.ack_nr),
    ?assertEqual(0, Pkt#aiutp_packet.seq_nr),
    ?assertEqual(0, Pkt#aiutp_packet.wnd),
    ?assertEqual([], Pkt#aiutp_packet.extension).

syn_packet_test() ->
    Pkt = aiutp_packet:syn(50),
    ?assertEqual(?ST_SYN, Pkt#aiutp_packet.type),
    ?assertEqual(50, Pkt#aiutp_packet.seq_nr),
    ?assertEqual(0, Pkt#aiutp_packet.ack_nr),
    ?assertEqual([], Pkt#aiutp_packet.extension).

fin_packet_test() ->
    Pkt = aiutp_packet:fin(200, 150),
    ?assertEqual(?ST_FIN, Pkt#aiutp_packet.type),
    ?assertEqual(200, Pkt#aiutp_packet.seq_nr),
    ?assertEqual(150, Pkt#aiutp_packet.ack_nr),
    ?assertEqual([], Pkt#aiutp_packet.extension).

ack_packet_without_extension_test() ->
    Pkt = aiutp_packet:ack(10, 20),
    ?assertEqual(?ST_STATE, Pkt#aiutp_packet.type),
    ?assertEqual(10, Pkt#aiutp_packet.seq_nr),
    ?assertEqual(20, Pkt#aiutp_packet.ack_nr),
    ?assertEqual([], Pkt#aiutp_packet.extension).

ack_packet_with_extension_test() ->
    Ext = [{sack, <<1, 2, 3, 4>>}],
    Pkt = aiutp_packet:ack(10, 20, Ext),
    ?assertEqual(?ST_STATE, Pkt#aiutp_packet.type),
    ?assertEqual(10, Pkt#aiutp_packet.seq_nr),
    ?assertEqual(20, Pkt#aiutp_packet.ack_nr),
    ?assertEqual(Ext, Pkt#aiutp_packet.extension).

data_packet_test() ->
    Pkt = aiutp_packet:data(100, 99),
    ?assertEqual(?ST_DATA, Pkt#aiutp_packet.type),
    ?assertEqual(100, Pkt#aiutp_packet.seq_nr),
    ?assertEqual(99, Pkt#aiutp_packet.ack_nr),
    ?assertEqual([], Pkt#aiutp_packet.extension).

%%====================================================================
%% Test: encode/1 and decode/1 - Basic packets
%%====================================================================

encode_decode_syn_test() ->
    Original = #aiutp_packet{
        type = ?ST_SYN,
        conn_id = 1000,
        tv_usec = 123456,
        reply_micro = 0,
        wnd = 65535,
        seq_nr = 1,
        ack_nr = 0,
        extension = [],
        payload = <<>>
    },
    Encoded = aiutp_packet:encode(Original),
    {ok, Decoded} = aiutp_packet:decode(Encoded),
    ?assertEqual(Original#aiutp_packet.type, Decoded#aiutp_packet.type),
    ?assertEqual(Original#aiutp_packet.conn_id, Decoded#aiutp_packet.conn_id),
    ?assertEqual(Original#aiutp_packet.seq_nr, Decoded#aiutp_packet.seq_nr),
    ?assertEqual(Original#aiutp_packet.ack_nr, Decoded#aiutp_packet.ack_nr),
    ?assertEqual(Original#aiutp_packet.wnd, Decoded#aiutp_packet.wnd).

encode_decode_state_test() ->
    Original = #aiutp_packet{
        type = ?ST_STATE,
        conn_id = 2000,
        tv_usec = 999999,
        reply_micro = 100,
        wnd = 32768,
        seq_nr = 50,
        ack_nr = 49,
        extension = [],
        payload = <<>>
    },
    Encoded = aiutp_packet:encode(Original),
    {ok, Decoded} = aiutp_packet:decode(Encoded),
    ?assertEqual(Original#aiutp_packet.type, Decoded#aiutp_packet.type),
    ?assertEqual(Original#aiutp_packet.conn_id, Decoded#aiutp_packet.conn_id),
    ?assertEqual(Original#aiutp_packet.seq_nr, Decoded#aiutp_packet.seq_nr),
    ?assertEqual(Original#aiutp_packet.ack_nr, Decoded#aiutp_packet.ack_nr).

encode_decode_data_with_payload_test() ->
    Payload = <<"Hello, uTP World!">>,
    Original = #aiutp_packet{
        type = ?ST_DATA,
        conn_id = 3000,
        tv_usec = 555555,
        reply_micro = 200,
        wnd = 16384,
        seq_nr = 100,
        ack_nr = 99,
        extension = [],
        payload = Payload
    },
    Encoded = aiutp_packet:encode(Original),
    {ok, Decoded} = aiutp_packet:decode(Encoded),
    ?assertEqual(Original#aiutp_packet.type, Decoded#aiutp_packet.type),
    ?assertEqual(Payload, Decoded#aiutp_packet.payload).

encode_decode_fin_test() ->
    Original = #aiutp_packet{
        type = ?ST_FIN,
        conn_id = 4000,
        tv_usec = 111111,
        reply_micro = 50,
        wnd = 8192,
        seq_nr = 200,
        ack_nr = 199,
        extension = [],
        payload = <<>>
    },
    Encoded = aiutp_packet:encode(Original),
    {ok, Decoded} = aiutp_packet:decode(Encoded),
    ?assertEqual(?ST_FIN, Decoded#aiutp_packet.type).

encode_decode_reset_test() ->
    Original = #aiutp_packet{
        type = ?ST_RESET,
        conn_id = 5000,
        tv_usec = 0,
        reply_micro = 0,
        wnd = 0,
        seq_nr = 0,
        ack_nr = 50,
        extension = [],
        payload = <<>>
    },
    Encoded = aiutp_packet:encode(Original),
    {ok, Decoded} = aiutp_packet:decode(Encoded),
    ?assertEqual(?ST_RESET, Decoded#aiutp_packet.type).

%%====================================================================
%% Test: Extensions (SACK)
%%====================================================================

encode_decode_with_sack_extension_test() ->
    SackBits = <<16#FF, 16#00, 16#FF, 16#00>>,
    Original = #aiutp_packet{
        type = ?ST_STATE,
        conn_id = 6000,
        tv_usec = 777777,
        reply_micro = 300,
        wnd = 65535,
        seq_nr = 10,
        ack_nr = 9,
        extension = [{sack, SackBits}],
        payload = <<>>
    },
    Encoded = aiutp_packet:encode(Original),
    {ok, Decoded} = aiutp_packet:decode(Encoded),
    ?assertEqual([{sack, SackBits}], Decoded#aiutp_packet.extension).

encode_decode_with_ext_bits_extension_test() ->
    ExtBits = <<16#AB, 16#CD>>,
    Original = #aiutp_packet{
        type = ?ST_STATE,
        conn_id = 7000,
        tv_usec = 888888,
        reply_micro = 400,
        wnd = 32768,
        seq_nr = 20,
        ack_nr = 19,
        extension = [{ext_bits, ExtBits}],
        payload = <<>>
    },
    Encoded = aiutp_packet:encode(Original),
    {ok, Decoded} = aiutp_packet:decode(Encoded),
    ?assertEqual([{ext_bits, ExtBits}], Decoded#aiutp_packet.extension).

encode_decode_multiple_extensions_test() ->
    SackBits = <<16#FF>>,
    ExtBits = <<16#12, 16#34>>,
    Original = #aiutp_packet{
        type = ?ST_STATE,
        conn_id = 8000,
        tv_usec = 999999,
        reply_micro = 500,
        wnd = 16384,
        seq_nr = 30,
        ack_nr = 29,
        extension = [{sack, SackBits}, {ext_bits, ExtBits}],
        payload = <<>>
    },
    Encoded = aiutp_packet:encode(Original),
    {ok, Decoded} = aiutp_packet:decode(Encoded),
    %% 扩展可能以不同顺序返回，验证两个都存在
    ?assert(lists:member({sack, SackBits}, Decoded#aiutp_packet.extension)),
    ?assert(lists:member({ext_bits, ExtBits}, Decoded#aiutp_packet.extension)).

encode_undefined_ext_bits_test() ->
    %% undefined ext_bits 应该被跳过
    Original = #aiutp_packet{
        type = ?ST_STATE,
        conn_id = 9000,
        tv_usec = 123456,
        reply_micro = 100,
        wnd = 65535,
        seq_nr = 40,
        ack_nr = 39,
        extension = [{ext_bits, undefined}],
        payload = <<>>
    },
    Encoded = aiutp_packet:encode(Original),
    {ok, Decoded} = aiutp_packet:decode(Encoded),
    ?assertEqual([], Decoded#aiutp_packet.extension).

%%====================================================================
%% Test: Boundary values
%%====================================================================

max_seq_nr_test() ->
    Original = #aiutp_packet{
        type = ?ST_DATA,
        conn_id = 16#FFFF,
        tv_usec = 16#FFFFFFFF,
        reply_micro = 16#FFFFFFFF,
        wnd = 16#FFFFFFFF,
        seq_nr = 16#FFFF,
        ack_nr = 16#FFFF,
        extension = [],
        payload = <<"test">>
    },
    Encoded = aiutp_packet:encode(Original),
    {ok, Decoded} = aiutp_packet:decode(Encoded),
    ?assertEqual(16#FFFF, Decoded#aiutp_packet.seq_nr),
    ?assertEqual(16#FFFF, Decoded#aiutp_packet.ack_nr),
    ?assertEqual(16#FFFF, Decoded#aiutp_packet.conn_id).

zero_values_test() ->
    Original = #aiutp_packet{
        type = ?ST_SYN,
        conn_id = 0,
        tv_usec = 0,
        reply_micro = 0,
        wnd = 0,
        seq_nr = 0,
        ack_nr = 0,
        extension = [],
        payload = <<>>
    },
    Encoded = aiutp_packet:encode(Original),
    {ok, Decoded} = aiutp_packet:decode(Encoded),
    ?assertEqual(0, Decoded#aiutp_packet.conn_id),
    ?assertEqual(0, Decoded#aiutp_packet.seq_nr).

%%====================================================================
%% Test: Invalid packets
%%====================================================================

decode_invalid_packet_test() ->
    %% 太短的数据包
    ?assertEqual({error, drop}, aiutp_packet:decode(<<1, 2, 3>>)).

decode_invalid_version_test() ->
    %% 版本号不是 1
    InvalidPacket = <<0:4, 2:4, 0:8, 0:16, 0:32, 0:32, 0:32, 0:16, 0:16>>,
    ?assertEqual({error, drop}, aiutp_packet:decode(InvalidPacket)).

decode_state_with_payload_test() ->
    %% ST_STATE 类型不应该有 payload
    InvalidPacket = <<(?ST_STATE):4, 1:4, 0:8, 1000:16, 0:32, 0:32, 65535:32, 1:16, 0:16, "payload">>,
    ?assertEqual({error, drop}, aiutp_packet:decode(InvalidPacket)).

decode_data_without_payload_test() ->
    %% ST_DATA 类型必须有 payload
    InvalidPacket = <<(?ST_DATA):4, 1:4, 0:8, 1000:16, 0:32, 0:32, 65535:32, 1:16, 0:16>>,
    ?assertEqual({error, drop}, aiutp_packet:decode(InvalidPacket)).

%%====================================================================
%% Test: Large payload
%%====================================================================

large_payload_test() ->
    LargePayload = crypto:strong_rand_bytes(1296), %% PACKET_SIZE
    Original = #aiutp_packet{
        type = ?ST_DATA,
        conn_id = 10000,
        tv_usec = 123456,
        reply_micro = 100,
        wnd = 65535,
        seq_nr = 500,
        ack_nr = 499,
        extension = [],
        payload = LargePayload
    },
    Encoded = aiutp_packet:encode(Original),
    {ok, Decoded} = aiutp_packet:decode(Encoded),
    ?assertEqual(LargePayload, Decoded#aiutp_packet.payload).

%%====================================================================
%% Test: Header size
%%====================================================================

header_size_no_extension_test() ->
    Pkt = #aiutp_packet{
        type = ?ST_SYN,
        conn_id = 1,
        tv_usec = 0,
        reply_micro = 0,
        wnd = 0,
        seq_nr = 0,
        ack_nr = 0,
        extension = [],
        payload = <<>>
    },
    Encoded = aiutp_packet:encode(Pkt),
    %% uTP 头部大小是 20 字节
    ?assertEqual(20, byte_size(Encoded)).
