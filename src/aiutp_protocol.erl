-module(aiutp_protocol).
-include("aiutp.hrl").

-export([decode/1]).

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
  TS = os:system_time(microsecond),

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

  {#packet { type = Ty,
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
