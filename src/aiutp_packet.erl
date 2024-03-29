-module(aiutp_packet).
-include("aiutp.hrl").

-export([decode/1,encode/1]).
-export([reset/2,syn/1,fin/2,ack/3,ack/2,data/2]).

-define(EXT_SACK, 1).
-define(EXT_BITS, 2).

reset(ConnId,AckNR)->
  #aiutp_packet{type = ?ST_RESET,
                ack_nr = AckNR,
                seq_nr = 0,
                wnd = 0,
                extension = [],
                conn_id = ConnId}.
syn(SeqNR) ->
  #aiutp_packet {type = ?ST_SYN,
                 seq_nr = SeqNR,
                 ack_nr = 0,
                 extension = []}.
fin(SeqNR,AckNR)->
  #aiutp_packet{type = ?ST_FIN,
                seq_nr = SeqNR,
                ack_nr = AckNR,
                extension = []}.
ack(SeqNR,AckNR,Ext)->
  #aiutp_packet {type = ?ST_STATE,
                 seq_nr = SeqNR,
                 ack_nr = AckNR,
                 extension = Ext}.
ack(SeqNR, AckNR) ->
  #aiutp_packet {type = ?ST_STATE,
                 seq_nr = SeqNR,
                 ack_nr = AckNR,
                 extension = []}.
data(SeqNR,AckNR)->
  #aiutp_packet{type = ?ST_DATA,
                seq_nr = SeqNR,
                ack_nr = AckNR,
                extension = []}.

decode(Packet) ->
  try
    case decode_packet(Packet) of
      {error,_} = Error-> Error;
      R -> {ok,R}
    end
  catch
    _:_ -> {error,drop}
  end.

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

decode_extensions(0, Payload, Exts) -> {lists:reverse(Exts), Payload};
decode_extensions(?EXT_SACK, <<Next:8/big-integer,Len:8/big-integer, R/binary>>, Acc) ->
  <<Bits:Len/binary, Rest/binary>> = R,
  decode_extensions(Next, Rest, [{sack, Bits} | Acc]);
decode_extensions(?EXT_BITS, <<Next:8/big-integer,
                               Len:8/big-integer, R/binary>>, Acc) ->
  <<ExtBits:Len/binary, Rest/binary>> = R,
  decode_extensions(Next, Rest, [{ext_bits, ExtBits} | Acc]).


validate_packet_type(Ty, Payload) ->
  case Ty of
    ?ST_STATE when Payload == <<>> -> ok;
    ?ST_DATA when Payload =/= <<>> -> ok;
    ?ST_FIN -> ok;
    ?ST_SYN -> ok;
    ?ST_RESET -> ok
  end.


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


encode_extensions([]) -> {0, <<>>};
encode_extensions([{sack, Bits} | R]) ->
  {Next, Bin} = encode_extensions(R),
  Sz = byte_size(Bits),
  {?EXT_SACK, <<Next:8/big-integer, Sz:8/big-integer, Bits/binary, Bin/binary>>};
encode_extensions([{ext_bits,undefined}| R])-> encode_extensions(R);
encode_extensions([{ext_bits, Bits} | R]) ->
  {Next, Bin} = encode_extensions(R),
  Sz = byte_size(Bits),
  {?EXT_BITS, <<Next:8/big-integer, Sz:8/big-integer, Bits/binary, Bin/binary>>}.

