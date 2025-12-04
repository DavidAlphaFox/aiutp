-module(aiutp_packet).
-include("aiutp.hrl").

-export([decode/1,encode/1]).
-export([reset/2,syn/1,ack/3,ack/2,data/2]).

%% 仅测试使用的导出
-ifdef(TEST).
-export([fin/2]).
-endif.

%% 类型定义
-type packet_type() :: ?ST_DATA | ?ST_FIN | ?ST_STATE | ?ST_RESET | ?ST_SYN.
-type extension() :: {sack, binary()} | {ext_bits, binary() | undefined}.
-type extension_list() :: [extension()].

%% 扩展类型现在定义在 aiutp.hrl 中
%% EXT_SACK = 1, EXT_EXT_BITS = 2

-spec reset(non_neg_integer(), non_neg_integer()) -> #aiutp_packet{}.
reset(ConnId,AckNR)->
  #aiutp_packet{type = ?ST_RESET,
                ack_nr = AckNR,
                seq_nr = 0,
                wnd = 0,
                extension = [],
                conn_id = ConnId}.

-spec syn(non_neg_integer()) -> #aiutp_packet{}.
syn(SeqNR) ->
  #aiutp_packet {type = ?ST_SYN,
                 seq_nr = SeqNR,
                 ack_nr = 0,
                 extension = []}.

-spec fin(non_neg_integer(), non_neg_integer()) -> #aiutp_packet{}.
fin(SeqNR,AckNR)->
  #aiutp_packet{type = ?ST_FIN,
                seq_nr = SeqNR,
                ack_nr = AckNR,
                extension = []}.

-spec ack(non_neg_integer(), non_neg_integer(), extension_list()) -> #aiutp_packet{}.
ack(SeqNR,AckNR,Ext)->
  #aiutp_packet {type = ?ST_STATE,
                 seq_nr = SeqNR,
                 ack_nr = AckNR,
                 extension = Ext}.

-spec ack(non_neg_integer(), non_neg_integer()) -> #aiutp_packet{}.
ack(SeqNR, AckNR) ->
  #aiutp_packet {type = ?ST_STATE,
                 seq_nr = SeqNR,
                 ack_nr = AckNR,
                 extension = []}.

-spec data(non_neg_integer(), non_neg_integer()) -> #aiutp_packet{}.
data(SeqNR,AckNR)->
  #aiutp_packet{type = ?ST_DATA,
                seq_nr = SeqNR,
                ack_nr = AckNR,
                extension = []}.

-spec decode(binary()) -> {ok, #aiutp_packet{}} | {error, drop}.
decode(Packet) ->
  try
    case decode_packet(Packet) of
      {error,_} = Error-> Error;
      R -> {ok,R}
    end
  catch
    _:_ -> {error,drop}
  end.

-spec decode_packet(binary()) -> #aiutp_packet{} | {error, drop}.
decode_packet(Packet) ->
  <<Ty:4/big-integer,1:4/big-integer,
    Extension:8/big-integer, ConnectionId:16/big-integer,
    TS:32/big-integer,
    TSDiff:32/big-integer,
    WindowSize:32/big-integer,SeqNR:16/big-integer,
    AckNR:16/big-integer,ExtPayload/binary>> = Packet,
  {Extensions, Payload} = decode_extensions(Extension, ExtPayload, []),
  Verified = validate_packet_type(Ty, Payload),
  if Verified /= ok -> {error,drop};
     true ->#aiutp_packet{type = Ty,
                           conn_id = ConnectionId,
                           tv_usec = TS,
                           reply_micro = TSDiff,
                           wnd = WindowSize,
                           seq_nr = SeqNR,
                           ack_nr = AckNR,
                           extension = Extensions,
                           payload = Payload}
  end.

-spec decode_extensions(non_neg_integer(), binary(), extension_list()) -> {extension_list(), binary()}.
decode_extensions(?EXT_NONE, Payload, Exts) -> {lists:reverse(Exts), Payload};
decode_extensions(?EXT_SACK, <<Next:8/big-integer,Len:8/big-integer, R/binary>>, Acc) ->
  <<Bits:Len/binary, Rest/binary>> = R,
  decode_extensions(Next, Rest, [{sack, Bits} | Acc]);
decode_extensions(?EXT_EXT_BITS, <<Next:8/big-integer,
                                   Len:8/big-integer, R/binary>>, Acc) ->
  <<ExtBits:Len/binary, Rest/binary>> = R,
  decode_extensions(Next, Rest, [{ext_bits, ExtBits} | Acc]).

-spec validate_packet_type(packet_type(), binary()) -> ok.
validate_packet_type(Ty, Payload) ->
  case Ty of
    ?ST_STATE when Payload == <<>> -> ok;
    ?ST_DATA when Payload =/= <<>> -> ok;
    ?ST_FIN -> ok;
    ?ST_SYN -> ok;
    ?ST_RESET -> ok
  end.


-spec encode(#aiutp_packet{}) -> binary().
encode(#aiutp_packet{type = Type,
                      conn_id = ConnId,
                      tv_usec = TS,
                      reply_micro = TSDiff,
                      wnd = WSize,
                      seq_nr = SeqNR,
                      ack_nr = AckNR,
                      extension = ExtList,
                      payload = Payload}) ->
  {Extension, ExtBin} = encode_extensions(ExtList),
  <<Type:4/big-integer,1:4/big-integer,
    Extension:8/big-integer, ConnId:16/big-integer,
    TS:32/big-integer,
    TSDiff:32/big-integer,
    WSize:32/big-integer,
    SeqNR:16/big-integer, AckNR:16/big-integer,
    ExtBin/binary,Payload/binary>>.


-spec encode_extensions(extension_list()) -> {non_neg_integer(), binary()}.
encode_extensions([]) -> {?EXT_NONE, <<>>};
encode_extensions([{sack, Bits} | R]) ->
  {Next, Bin} = encode_extensions(R),
  Sz = byte_size(Bits),
  {?EXT_SACK, <<Next:8/big-integer, Sz:8/big-integer, Bits/binary, Bin/binary>>};
encode_extensions([{ext_bits,undefined}| R])-> encode_extensions(R);
encode_extensions([{ext_bits, Bits} | R]) ->
  {Next, Bin} = encode_extensions(R),
  Sz = byte_size(Bits),
  {?EXT_EXT_BITS, <<Next:8/big-integer, Sz:8/big-integer, Bits/binary, Bin/binary>>}.

