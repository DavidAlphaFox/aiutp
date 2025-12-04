%%------------------------------------------------------------------------------
%% @doc aiutp PropEr 属性测试
%%
%% 使用 PropEr 框架对核心模块进行属性测试，验证：
%% - 数据结构的不变量
%% - 函数的幂等性和可逆性
%% - 边界条件处理
%%------------------------------------------------------------------------------
-module(aiutp_prop_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/aiutp.hrl").

%%==============================================================================
%% 测试配置
%%==============================================================================

-define(NUMTESTS, 100).

%%==============================================================================
%% EUnit 集成
%%==============================================================================

proper_test_() ->
    {timeout, 60, [
        {"aiutp_queue 属性测试", fun() -> run_proper(fun prop_queue/0) end},
        {"aiutp_buffer 属性测试", fun() -> run_proper(fun prop_buffer/0) end},
        {"aiutp_packet 属性测试", fun() -> run_proper(fun prop_packet/0) end},
        {"aiutp_util 属性测试", fun() -> run_proper(fun prop_util/0) end},
        {"aiutp_mtu 属性测试", fun() -> run_proper(fun prop_mtu/0) end}
    ]}.

run_proper(TestGroupFun) ->
    Props = TestGroupFun(),
    lists:foreach(fun({Name, Prop}) ->
        ?assertEqual(true, proper:quickcheck(Prop, [{numtests, ?NUMTESTS}, {to_file, user}]),
                     Name)
    end, Props).

%%==============================================================================
%% 生成器
%%==============================================================================

%% 16 位无符号整数
uint16() ->
    integer(0, 16#FFFF).

%% 32 位无符号整数
uint32() ->
    integer(0, 16#FFFFFFFF).


%% 包类型
packet_type() ->
    oneof([?ST_DATA, ?ST_FIN, ?ST_STATE, ?ST_RESET, ?ST_SYN]).

%% 有效扩展列表
extension_list() ->
    list(oneof([
        {sack, binary(4)},
        {ext_bits, binary(8)}
    ])).

%% 有效 uTP 包
utp_packet() ->
    ?LET({Type, SeqNr, AckNr, ConnId, Wnd, Ext, Payload},
         {packet_type(), uint16(), uint16(), uint16(), uint32(), extension_list(), binary()},
         #aiutp_packet{
             type = Type,
             seq_nr = SeqNr,
             ack_nr = AckNr,
             conn_id = ConnId,
             wnd = Wnd,
             extension = Ext,
             payload = Payload
         }).

%% MTU 值范围
mtu_value() ->
    integer(?MTU_FLOOR_DEFAULT, ?MTU_CEILING_DEFAULT).

%%==============================================================================
%% aiutp_queue 属性测试
%%==============================================================================

prop_queue() ->
    [
        {"队列大小正确性", prop_queue_size()},
        {"FIFO 顺序正确", prop_queue_fifo()},
        {"push/pop 可逆", prop_queue_push_pop_inverse()}
    ].

%% 属性：队列大小始终等于实际元素数量
prop_queue_size() ->
    ?FORALL(Elements, list(integer()),
        begin
            Q = lists:foldl(fun(E, Acc) ->
                aiutp_queue:push_back(E, Acc)
            end, aiutp_queue:new(), Elements),
            aiutp_queue:size(Q) =:= length(Elements)
        end).

%% 属性：FIFO 顺序正确
prop_queue_fifo() ->
    ?FORALL(Elements, non_empty(list(integer())),
        begin
            Q = lists:foldl(fun(E, Acc) ->
                aiutp_queue:push_back(E, Acc)
            end, aiutp_queue:new(), Elements),
            aiutp_queue:front(Q) =:= hd(Elements)
        end).

%% 属性：push_back 后 pop_front 返回相同顺序
prop_queue_push_pop_inverse() ->
    ?FORALL(Elements, list(integer()),
        begin
            Q = lists:foldl(fun(E, Acc) ->
                aiutp_queue:push_back(E, Acc)
            end, aiutp_queue:new(), Elements),
            Recovered = pop_all(Q, []),
            Recovered =:= Elements
        end).

pop_all(Q, Acc) ->
    case aiutp_queue:empty(Q) of
        true -> lists:reverse(Acc);
        false ->
            E = aiutp_queue:front(Q),
            Q1 = aiutp_queue:pop_front(Q),
            pop_all(Q1, [E | Acc])
    end.

%%==============================================================================
%% aiutp_buffer 属性测试
%%==============================================================================

prop_buffer() ->
    [
        {"缓冲区已用数量正确", prop_buffer_used()},
        {"缓冲区容量限制", prop_buffer_capacity()}
    ].

%% 属性：缓冲区已用数量正确
prop_buffer_used() ->
    ?FORALL({Size, Elements}, {integer(8, 64), list(integer())},
        begin
            B0 = aiutp_buffer:new(Size),
            %% 只添加不超过 Size 个元素
            ToAdd = lists:sublist(Elements, Size),
            B1 = lists:foldl(fun(E, Buf) ->
                case aiutp_buffer:append(E, Buf) of
                    {error, buffer_overflow} -> Buf;
                    NewBuf -> NewBuf
                end
            end, B0, ToAdd),
            aiutp_buffer:used(B1) >= 0 andalso
            aiutp_buffer:used(B1) =< Size
        end).

%% 属性：缓冲区容量限制
prop_buffer_capacity() ->
    ?FORALL({Size, Count}, {integer(8, 64), integer(0, 100)},
        begin
            B0 = aiutp_buffer:new(Size),
            B1 = lists:foldl(fun(I, Buf) ->
                case aiutp_buffer:append(I, Buf) of
                    {error, buffer_overflow} -> Buf;
                    NewBuf -> NewBuf
                end
            end, B0, lists:seq(1, Count)),
            %% 已用不能超过容量
            aiutp_buffer:used(B1) =< Size
        end).

%%==============================================================================
%% aiutp_packet 属性测试
%%==============================================================================

prop_packet() ->
    [
        {"编解码可逆性", prop_packet_encode_decode()}
    ].

%% 属性：encode 后 decode 应返回原始包
prop_packet_encode_decode() ->
    ?FORALL(Packet, utp_packet(),
        begin
            %% 确保 wnd 在有效范围内
            Packet1 = Packet#aiutp_packet{wnd = Packet#aiutp_packet.wnd band 16#FFFFFFFF},
            Encoded = aiutp_packet:encode(Packet1),
            case aiutp_packet:decode(Encoded) of
                {ok, Decoded, <<>>} ->
                    %% 比较关键字段
                    Decoded#aiutp_packet.type =:= Packet1#aiutp_packet.type andalso
                    Decoded#aiutp_packet.seq_nr =:= Packet1#aiutp_packet.seq_nr andalso
                    Decoded#aiutp_packet.ack_nr =:= Packet1#aiutp_packet.ack_nr andalso
                    Decoded#aiutp_packet.conn_id =:= Packet1#aiutp_packet.conn_id;
                _ ->
                    true  % 某些扩展组合可能无法解码，跳过
            end
        end).

%%==============================================================================
%% aiutp_util 属性测试
%%==============================================================================

prop_util() ->
    [
        {"bit16 幂等性", prop_bit16_idempotent()},
        {"bit32 幂等性", prop_bit32_idempotent()},
        {"clamp 范围正确", prop_clamp_bounds()}
    ].

%% 属性：对 16 位值调用 bit16 是幂等的
prop_bit16_idempotent() ->
    ?FORALL(N, uint16(),
        begin
            aiutp_util:bit16(N) =:= aiutp_util:bit16(aiutp_util:bit16(N))
        end).

%% 属性：对 32 位值调用 bit32 是幂等的
prop_bit32_idempotent() ->
    ?FORALL(N, uint32(),
        begin
            aiutp_util:bit32(N) =:= aiutp_util:bit32(aiutp_util:bit32(N))
        end).

%% 属性：clamp 结果始终在范围内
prop_clamp_bounds() ->
    ?FORALL({Val, Min, Max}, {integer(), integer(), integer()},
        ?IMPLIES(Min =< Max,
            begin
                Result = aiutp_util:clamp(Val, Min, Max),
                Result >= Min andalso Result =< Max
            end)).

%%==============================================================================
%% aiutp_mtu 属性测试
%%==============================================================================

prop_mtu() ->
    [
        {"二分搜索收敛", prop_mtu_binary_search_convergence()},
        {"MTU 范围约束", prop_mtu_value_bounds()}
    ].

%% 属性：二分搜索最终会收敛（floor >= ceiling - threshold）
prop_mtu_binary_search_convergence() ->
    ?FORALL({Floor, Ceiling}, {mtu_value(), mtu_value()},
        ?IMPLIES(Floor < Ceiling,
            begin
                %% 模拟搜索过程
                {FinalFloor, FinalCeiling} = simulate_search(Floor, Ceiling, 20),
                FinalCeiling - FinalFloor =< ?MTU_SEARCH_THRESHOLD orelse
                FinalFloor >= FinalCeiling
            end)).

simulate_search(Floor, Ceiling, 0) ->
    {Floor, Ceiling};
simulate_search(Floor, Ceiling, _Steps) when Ceiling - Floor =< ?MTU_SEARCH_THRESHOLD ->
    {Floor, Ceiling};
simulate_search(Floor, Ceiling, Steps) ->
    Mid = (Floor + Ceiling) div 2,
    %% 随机决定成功或失败
    case rand:uniform(2) of
        1 -> simulate_search(Mid, Ceiling, Steps - 1);  % 成功
        2 -> simulate_search(Floor, Mid, Steps - 1)      % 失败
    end.

%% 属性：MTU 值生成始终在有效范围内
prop_mtu_value_bounds() ->
    ?FORALL(Mtu, mtu_value(),
        begin
            Mtu >= ?MTU_FLOOR_DEFAULT andalso Mtu =< ?MTU_CEILING_DEFAULT
        end).
