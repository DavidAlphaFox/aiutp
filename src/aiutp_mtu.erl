%%------------------------------------------------------------------------------
%% @doc aiutp_mtu - MTU 发现模块
%%
%% 实现路径 MTU 发现 (PMTUD) 功能，使用二分查找探测网络路径支持的最大包大小。
%%
%% 算法:
%% 1. 初始化: floor = 528, ceiling = 1452
%% 2. 探测: 发送大小为 (floor + ceiling) / 2 的包
%% 3. 成功: floor = probe_size，继续探测
%% 4. 失败: ceiling = probe_size - 1，继续探测
%% 5. 完成: 当 ceiling - floor <= 16 时停止
%%
%% 参考: RFC 4821 (PLPMTUD), libutp
%% @end
%%------------------------------------------------------------------------------

-module(aiutp_mtu).

-include("aiutp.hrl").

-export([
    reset/1,
    search_update/1,
    should_probe/2,
    on_probe_acked/2,
    on_probe_timeout/1,
    on_probe_lost/1,
    packet_size/1,
    is_probing/1,
    maybe_restart_discovery/1
]).

%%==============================================================================
%% API 函数
%%==============================================================================

%% @doc 重置 MTU 发现状态
%% 在连接建立时或周期性重新探测时调用
-spec reset(#aiutp_pcb{}) -> #aiutp_pcb{}.
reset(PCB) ->
    Now = aiutp_util:microsecond(),
    PCB#aiutp_pcb{
        mtu_floor = ?MTU_FLOOR_DEFAULT,
        mtu_ceiling = ?MTU_CEILING_DEFAULT,
        mtu_last = ?PACKET_SIZE,
        mtu_probe_seq = 0,
        mtu_probe_size = 0,
        mtu_discover_time = Now + ?MTU_PROBE_INTERVAL,
        mtu_probe_failures = 0
    }.

%% @doc 更新二分查找，计算下一个探测大小
%% 当 ceiling - floor <= MTU_SEARCH_THRESHOLD 时，搜索完成
-spec search_update(#aiutp_pcb{}) -> #aiutp_pcb{}.
search_update(#aiutp_pcb{mtu_floor = Floor, mtu_ceiling = Ceiling} = PCB) ->
    case Ceiling - Floor =< ?MTU_SEARCH_THRESHOLD of
        true ->
            %% 搜索完成，使用 floor 作为最终值
            Now = aiutp_util:microsecond(),
            PCB#aiutp_pcb{
                mtu_last = Floor,
                mtu_ceiling = Floor,
                mtu_discover_time = Now + ?MTU_PROBE_INTERVAL
            };
        false ->
            %% 继续二分查找
            NewLast = (Floor + Ceiling) div 2,
            PCB#aiutp_pcb{mtu_last = NewLast}
    end.

%% @doc 判断是否应该将此包作为 MTU 探测包
%% 条件:
%% 1. 存在搜索空间 (floor < ceiling)
%% 2. 包大小在搜索范围内 (floor < size <= ceiling)
%% 3. 无在途探测包 (probe_seq == 0)
-spec should_probe(non_neg_integer(), #aiutp_pcb{}) -> boolean().
should_probe(PacketSize, #aiutp_pcb{
    mtu_floor = Floor,
    mtu_ceiling = Ceiling,
    mtu_probe_seq = ProbeSeq
}) ->
    (Floor < Ceiling) andalso
    (PacketSize > Floor) andalso
    (PacketSize =< Ceiling) andalso
    (ProbeSeq == 0).

%% @doc 探测包被成功 ACK
%% 提高 floor 到探测大小，继续二分查找
-spec on_probe_acked(non_neg_integer(), #aiutp_pcb{}) -> #aiutp_pcb{}.
on_probe_acked(AckedSeq, #aiutp_pcb{
    mtu_probe_seq = ProbeSeq,
    mtu_probe_size = ProbeSize
} = PCB) when AckedSeq == ProbeSeq, ProbeSeq =/= 0 ->
    %% 探测成功，提高 floor
    PCB1 = PCB#aiutp_pcb{
        mtu_floor = ProbeSize,
        mtu_probe_seq = 0,
        mtu_probe_size = 0,
        mtu_probe_failures = 0
    },
    search_update(PCB1);
on_probe_acked(_AckedSeq, PCB) ->
    PCB.

%% @doc 探测包超时
%% 降低 ceiling 到探测大小 - 1，继续二分查找
%% 如果连续失败过多，回退到 floor
-spec on_probe_timeout(#aiutp_pcb{}) -> #aiutp_pcb{}.
on_probe_timeout(#aiutp_pcb{
    mtu_probe_seq = ProbeSeq,
    mtu_probe_size = ProbeSize,
    mtu_probe_failures = Failures
} = PCB) when ProbeSeq =/= 0 ->
    %% 探测失败，降低 ceiling
    NewFailures = Failures + 1,
    PCB1 = PCB#aiutp_pcb{
        mtu_ceiling = ProbeSize - 1,
        mtu_probe_seq = 0,
        mtu_probe_size = 0,
        mtu_probe_failures = NewFailures
    },
    case NewFailures >= ?MTU_PROBE_FAILURE_THRESHOLD of
        true ->
            %% 连续失败过多，回退到 floor
            logger:warning("MTU discovery failed after ~p consecutive attempts, "
                           "falling back to floor=~p bytes, probe_size=~p bytes",
                           [NewFailures, PCB1#aiutp_pcb.mtu_floor, ProbeSize]),
            Now = aiutp_util:microsecond(),
            BackoffInterval = ?MTU_PROBE_INTERVAL * ?MTU_PROBE_BACKOFF_MULTIPLIER,
            PCB1#aiutp_pcb{
                mtu_last = PCB1#aiutp_pcb.mtu_floor,
                mtu_ceiling = PCB1#aiutp_pcb.mtu_floor,
                mtu_discover_time = Now + BackoffInterval
            };
        false ->
            search_update(PCB1)
    end;
on_probe_timeout(PCB) ->
    PCB.

%% @doc 探测包因重复 ACK 判定丢失
%% 与超时处理相同
-spec on_probe_lost(#aiutp_pcb{}) -> #aiutp_pcb{}.
on_probe_lost(PCB) ->
    on_probe_timeout(PCB).

%% @doc 获取当前有效的 packet size
-spec packet_size(#aiutp_pcb{}) -> non_neg_integer().
packet_size(#aiutp_pcb{mtu_last = Last}) ->
    Last.

%% @doc 检查是否有在途探测包
-spec is_probing(#aiutp_pcb{}) -> boolean().
is_probing(#aiutp_pcb{mtu_probe_seq = Seq}) ->
    Seq =/= 0.

%% @doc 检查是否需要重新开始 MTU 发现
%% 当前时间超过 mtu_discover_time 且存在搜索空间时重新开始
-spec maybe_restart_discovery(#aiutp_pcb{}) -> #aiutp_pcb{}.
maybe_restart_discovery(#aiutp_pcb{
    mtu_floor = Floor,
    mtu_ceiling = Ceiling,
    mtu_discover_time = DiscoverTime
} = PCB) ->
    Now = aiutp_util:microsecond(),
    case (Now >= DiscoverTime) andalso (Floor >= Ceiling) of
        true ->
            %% 时间到且搜索已完成，重新开始探测
            reset(PCB);
        false ->
            PCB
    end.

%%==============================================================================
%% 内部函数
%%==============================================================================

%% 暂无内部函数
