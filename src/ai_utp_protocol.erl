-module(ai_utp_protocol).
-include("ai_utp.hrl").

-export([decode/2,encode/2,encode/3]).
-export([make_syn_packet/1,make_ack_packet/2,
         make_ack_packet/3,make_reset_packet/2,
         make_data_packet/2,make_fin_packet/2]).

-define(SYN_EXTS, [{ext_bits, <<0:64/integer>>}]).

-type conn_id() :: 0..16#FFFF.
-export_type([utp_packet/0, conn_id/0]).

-define(EXT_SACK, 1).
-define(EXT_BITS, 2).

-define(ST_DATA,  0).
-define(ST_FIN,   1).
-define(ST_STATE, 2).
-define(ST_RESET, 3).
-define(ST_SYN,   4).


make_reset_packet(ConnID,AckNo)->
  #utp_packet{type = st_reset,
          ack_no = AckNo,
          seq_no = ai_utp_util:bit16_random(),
          win_sz = 0,
          extension = [],
          conn_id = ConnID}.
make_syn_packet(SeqNo) ->
  #utp_packet { type = st_syn,
            seq_no = SeqNo,
            ack_no = 0,
            extension = ?SYN_EXTS}.
make_fin_packet(SeqNo,AckNo)->
  #utp_packet{ type = st_fin,
               seq_no = SeqNo,
               ack_no = AckNo,
               extension = []}.
make_ack_packet(SeqNo,AckNo,Ext)->
  #utp_packet {type = st_state,
           seq_no = SeqNo,
           ack_no = AckNo,
           extension = Ext}.
make_ack_packet(SeqNo, AckNo) ->
  #utp_packet {type = st_state,
           seq_no = SeqNo,
           ack_no = AckNo,
           extension = []}.
make_data_packet(SeqNo,AckNo)->
  #utp_packet{type = st_data,
             seq_no = SeqNo,
             ack_no = AckNo,
             extension = []}.

-spec decode(binary(),integer()) ->
        {ok, {utp_packet(),{timestamp(), timestamp(), timestamp()}}} |
        {error, term()}.
decode(Packet,RecvTS) ->
  try
    case decode_packet(Packet,RecvTS) of
      {error,_} = Error-> Error;
      R -> {ok,R}
    end
  catch
    _:_ -> {error,drop}
  end.



-spec decode_packet(binary(),integer()) ->
        {utp_packet(),{timestamp(), timestamp(), timestamp()}}.
decode_packet(Packet,RecvTS) ->

  %% Decode packet
  <<1:4/big-integer,Type:4/big-integer,
    Extension:8/big-integer, ConnectionId:16/big-integer,
    TS:32/big-integer,TSDiff:32/big-integer,
    WindowSize:32/big-integer,
    SeqNo:16/big-integer,AckNo:16/big-integer,
    ExtPayload/binary>> = Packet,
  {Extensions, Payload} = decode_extensions(Extension, ExtPayload, []),

  %% Validate packet contents
  Ty = decode_type(Type),
  Verified = validate_packet_type(Ty, Payload),
  if Verified /= ok -> {error,drop};
     true ->
      {#utp_packet{type = Ty,
                   conn_id = ConnectionId,
                   win_sz = WindowSize,
                   seq_no = SeqNo,
                   ack_no = AckNo,
                   extension = Extensions,
                   payload = Payload},
       {TS,TSDiff,RecvTS}}
  end.


decode_extensions(0, Payload, Exts) -> {lists:reverse(Exts), Payload};
decode_extensions(?EXT_SACK, <<Next:8/big-integer,
                               Len:8/big-integer, R/binary>>, Acc) ->
  <<Bits:Len/binary, Rest/binary>> = R,
  decode_extensions(Next, Rest, [{sack, Bits} | Acc]);
decode_extensions(?EXT_BITS, <<Next:8/big-integer,
                               Len:8/big-integer, R/binary>>, Acc) ->
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


encode(Packet,TSDiff)->
  TS = ai_utp_util:microsecond(),
  encode(Packet,TS,TSDiff).
-spec encode(utp_packet(), integer(),integer()) -> binary().
encode(#utp_packet {type = Type,
                conn_id = ConnID,
                win_sz = WSize,
                seq_no = SeqNo,
                ack_no = AckNo,
                extension = ExtList,
                payload = Payload}, TS,TSDiff) ->
  {Extension, ExtBin} = encode_extensions(ExtList),
  EncTy = encode_type(Type),
  <<1:4/big-integer,EncTy:4/big-integer,
    Extension:8/big-integer, ConnID:16/big-integer,
    TS:32/big-integer,TSDiff:32/big-integer,
    WSize:32/big-integer,
    SeqNo:16/big-integer, AckNo:16/big-integer,
    ExtBin/binary,Payload/binary>>.


encode_extensions([]) -> {0, <<>>};
encode_extensions([{sack, Bits} | R]) ->
  {Next, Bin} = encode_extensions(R),
  Sz = byte_size(Bits),
  {?EXT_SACK, <<Next:8/big-integer, Sz:8/big-integer, Bits/binary, Bin/binary>>};
encode_extensions([{ext_bits, Bits} | R]) ->
  {Next, Bin} = encode_extensions(R),
  Sz = byte_size(Bits),
  {?EXT_BITS, <<Next:8/big-integer, Sz:8/big-integer, Bits/binary, Bin/binary>>}.

encode_type(st_data) -> ?ST_DATA;
encode_type(st_fin) -> ?ST_FIN;
encode_type(st_state) -> ?ST_STATE;
encode_type(st_reset) -> ?ST_RESET;
encode_type(st_syn) -> ?ST_SYN.
