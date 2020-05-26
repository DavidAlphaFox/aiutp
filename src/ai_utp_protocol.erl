-module(ai_utp_protocol).
-include("ai_utp.hrl").

-export([decode/1,encode/2]).
-export([make_syn_packet/0,make_ack_packet/2]).

-define(SYN_EXTS, [{ext_bits, <<0:64/integer>>}]).

-type conn_id() :: 0..16#FFFF.
-export_type([packet/0, conn_id/0]).

-define(EXT_SACK, 1).
-define(EXT_BITS, 2).

-define(ST_DATA,  0).
-define(ST_FIN,   1).
-define(ST_STATE, 2).
-define(ST_RESET, 3).
-define(ST_SYN,   4).


make_syn_packet() ->
     #packet { type = st_syn,
               seq_no = 1,
               ack_no = 0,
               extension = ?SYN_EXTS
             }. % Rest are defaults

make_ack_packet(SeqNo, AckNo) ->
  #packet {type = st_state,
           seq_no = SeqNo,
           ack_no = AckNo,
           extension = ?SYN_EXTS
          }.


-spec decode(binary()) -> {ok, {packet(), timestamp(), timestamp(), timestamp()}}
                              | {error, term()}.
decode(Packet) ->
  try
    P = decode_packet(Packet),
    {ok, P}
  catch
    error:Reason -> {error, Reason}
  end.

-spec decode_packet(binary()) ->
        {packet(),integer(),integer(),integer()}.
decode_packet(Packet) ->
  TS = ai_utp_util:microsecond(),

  %% Decode packet
  <<1:4/integer, Type:4/integer, Extension:8/integer, ConnectionId:16/integer,
    TimeStamp:32/integer,TimeStampdiff:32/integer,
    WindowSize:32/integer,
    SeqNo:16/integer,AckNo:16/integer,
    ExtPayload/binary>> = Packet,
  {Extensions, Payload} = decode_extensions(Extension, ExtPayload, []),

    %% Validate packet contents
  Ty = decode_type(Type),
  ok = validate_packet_type(Ty, Payload),

  {#packet{type = Ty,
           conn_id = ConnectionId,
           win_sz = WindowSize,
           seq_no = SeqNo,
           ack_no = AckNo,
           extension = Extensions,
           payload = Payload},
   TimeStamp,
   TimeStampdiff,
   TS}.
decode_extensions(0, Payload, Exts) -> {lists:reverse(Exts), Payload};
decode_extensions(?EXT_SACK, <<Next:8/integer,
                               Len:8/integer, R/binary>>, Acc) ->
  <<Bits:Len/binary, Rest/binary>> = R,
  decode_extensions(Next, Rest, [{sack, Bits} | Acc]);
decode_extensions(?EXT_BITS, <<Next:8/integer,
                               Len:8/integer, R/binary>>, Acc) ->
  <<ExtBits:Len/binary, Rest/binary>> = R,
  decode_extensions(Next, Rest, [{ext_bits, ExtBits} | Acc]).

decode_type(?ST_DATA) -> st_data;
decode_type(?ST_FIN) -> st_fin;
decode_type(?ST_STATE) -> st_state;
decode_type(?ST_RESET) -> st_reset;
decode_type(?ST_SYN) -> st_syn.
validate_packet_type(Ty, Payload) ->
  case Ty of
    st_state when Payload == <<>> -> ok;
    st_data when Payload =/= <<>> -> ok;
    st_fin -> ok;
    st_syn -> ok;
    st_reset -> ok
  end.


-spec encode(packet(), integer()) -> binary().
encode(#packet {type = Type,
                conn_id = ConnID,
                win_sz = WSize,
                seq_no = SeqNo,
                ack_no = AckNo,
                extension = ExtList,
                payload = Payload}, TSDiff) ->
  {Extension, ExtBin} = encode_extensions(ExtList),
  EncTy = encode_type(Type),
  TS = ai_utp_util:microsecond(),
  <<1:4/integer, EncTy:4/integer, Extension:8/integer, ConnID:16/integer,
    TS:32/integer,TSDiff:32/integer,
    WSize:32/integer,
    SeqNo:16/integer, AckNo:16/integer,
    ExtBin/binary,Payload/binary>>.

encode_extensions([]) -> {0, <<>>};
encode_extensions([{sack, Bits} | R]) ->
  {Next, Bin} = encode_extensions(R),
  Sz = byte_size(Bits),
  {?EXT_SACK, <<Next:8/integer, Sz:8/integer, Bits/binary, Bin/binary>>};
encode_extensions([{ext_bits, Bits} | R]) ->
  {Next, Bin} = encode_extensions(R),
  Sz = byte_size(Bits),
  {?EXT_BITS, <<Next:8/integer, Sz:8/integer, Bits/binary, Bin/binary>>}.

encode_type(st_data) -> ?ST_DATA;
encode_type(st_fin) -> ?ST_FIN;
encode_type(st_state) -> ?ST_STATE;
encode_type(st_reset) -> ?ST_RESET;
encode_type(st_syn) -> ?ST_SYN.
