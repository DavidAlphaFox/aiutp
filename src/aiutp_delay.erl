%%------------------------------------------------------------------------------
%% @doc LEDBAT 延迟估计模块
%%
%% 本模块实现 RFC 6817 中定义的延迟测量和过滤算法，用于 LEDBAT 拥塞控制。
%%
%% == 概述 ==
%%
%% LEDBAT 需要估计网络路径上的排队延迟（queuing delay）。为此，需要：
%% 1. 测量端到端单向延迟（one-way delay）
%% 2. 从中分离出固定延迟成分（传播延迟等）
%% 3. 得到可变的排队延迟成分
%%
%% == 核心概念 ==
%%
%% === Base Delay（基准延迟）===
%% - 表示路径上的最小固定延迟（传播延迟 + 最小处理延迟）
%% - 通过记录历史观测到的最小延迟值来估计
%% - RFC 6817: 应保持 BASE_HISTORY (10-13) 分钟的历史
%% - 每分钟维护一个最小值，防止单个异常值影响
%%
%% === Current Delay（当前延迟）===
%% - 最近 RTT 内观测到的延迟样本
%% - RFC 6817: 应保留不超过一个 RTT 的样本
%% - libutp: 使用 CUR_DELAY_SIZE (3) 个样本的滑动窗口
%%
%% === Queuing Delay（排队延迟）===
%% - 计算公式: queuing_delay = min(current_delays) - base_delay
%% - 这是 LEDBAT 拥塞控制的核心输入
%%
%% == 时钟漂移处理 ==
%%
%% 由于发送方和接收方的时钟可能以不同速率运行，会导致：
%% - 时钟偏移（Clock Offset）: 两端时钟的固定差值，可以互相抵消
%% - 时钟漂移（Clock Skew）: 时钟速率差异导致的累积误差
%%
%% 本模块通过以下机制处理时钟漂移：
%% 1. 定期（每分钟）更新 base_delay 历史
%% 2. shift/2 函数用于根据对端的 base_delay 变化调整本地值
%% 3. 使用 32 位环绕安全的比较算法
%%
%% == 32 位环绕处理 ==
%%
%% uTP 时间戳使用 32 位微秒计数器，约每 71 分钟环绕一次。
%% 示例场景：
%% - delay_base = 0xFFFFFF00, sample = 0x00000400
%%   sample - delay_base = 0x500 (正确的 1280 微秒差值)
%% - delay_base = 0x00000400, sample = 0xFFFFFF00
%%   需要解释为负数（样本比基准更早）
%%
%% @reference RFC 6817: Low Extra Delay Background Transport (LEDBAT)
%% @reference libutp: https://github.com/bittorrent/libutp
%% @end
%%------------------------------------------------------------------------------
-module(aiutp_delay).
-include("aiutp.hrl").

-export([new/0,
         new/1,
         shift/2,
         add_sample/3,
         delay_base/1,
         value/1]).

-export_type([aiutp_delay/0]).

%%------------------------------------------------------------------------------
%% 类型定义
%%------------------------------------------------------------------------------

-record(aiutp_delay, {
    %% 当前估计的基准延迟（32 位微秒时间戳）
    %% 表示路径上观测到的最小单向延迟
    delay_base :: non_neg_integer(),

    %% 当前延迟样本历史（环形缓冲区）
    %% 存储最近 CUR_DELAY_SIZE 个延迟样本
    %% 值为相对于 delay_base 的延迟（微秒）
    cur_delay_hist :: array:array(non_neg_integer()),

    %% 当前延迟历史的写入索引
    cur_delay_idx :: non_neg_integer(),

    %% 基准延迟历史（每分钟一个最小值）
    %% 存储最近 DELAY_BASE_HISTORY 分钟的最小延迟
    %% 用于处理时钟漂移和路由变化
    delay_base_hist :: array:array(non_neg_integer()) | undefined,

    %% 基准延迟历史的写入索引
    delay_base_idx :: non_neg_integer(),

    %% 上次滚动基准延迟历史的时间（毫秒）
    delay_base_time :: integer(),

    %% 是否已用实际样本初始化
    delay_base_initialized :: boolean()
}).

-opaque aiutp_delay() :: #aiutp_delay{}.

%%------------------------------------------------------------------------------
%% API 函数
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc 创建新的延迟估计器
%%
%% 使用当前时间为 0 初始化。
%% @end
%%------------------------------------------------------------------------------
-spec new() -> aiutp_delay().
new() -> new(0).

%%------------------------------------------------------------------------------
%% @doc 创建新的延迟估计器
%%
%% @param CurMilli 当前时间（毫秒），用于基准延迟历史的时间追踪
%% @end
%%------------------------------------------------------------------------------
-spec new(integer()) -> aiutp_delay().
new(CurMilli) ->
    #aiutp_delay{
        delay_base = 0,
        cur_delay_hist = array:new(?CUR_DELAY_SIZE, [{default, 0}, {fixed, true}]),
        cur_delay_idx = 0,
        delay_base_idx = 0,
        delay_base_time = CurMilli,
        delay_base_initialized = false
    }.

%%------------------------------------------------------------------------------
%% @doc 调整基准延迟以补偿时钟漂移
%%
%% 当观测到对端的 base_delay 减少时调用此函数，
%% 将本地的所有历史延迟值增加相应的偏移量。
%%
%% libutp: "increase all of our base delays by this amount
%% this is used to take clock skew into account
%% by observing the other side's changes in its base_delay"
%%
%% @param Offset 偏移量（微秒），应为非负值且小于 0x10000000
%% @param Delay 延迟估计器
%% @returns 调整后的延迟估计器
%% @end
%%------------------------------------------------------------------------------
-spec shift(non_neg_integer(), aiutp_delay()) -> aiutp_delay().
shift(Offset, #aiutp_delay{delay_base_hist = Hist} = Delay) ->
    %% 将历史中的每个值增加 Offset
    %% 注意：delay_base 字段不直接修改，它会在下次 add_sample 时重新计算
    Hist0 = array:map(fun(_, El) -> El + Offset end, Hist),
    Delay#aiutp_delay{delay_base_hist = Hist0}.

%%------------------------------------------------------------------------------
%% @doc 添加新的延迟样本
%%
%% 这是延迟估计的核心函数，每收到一个 ACK 包时调用。
%%
%% 算法步骤：
%% 1. 如果是首个样本，初始化所有历史
%% 2. 更新当前分钟的最小值（如果样本更小）
%% 3. 更新全局 delay_base（如果样本更小）
%% 4. 计算相对延迟并存入 cur_delay_hist
%% 5. 如果超过 60 秒，滚动 delay_base_hist 并重新计算 delay_base
%%
%% @param Sample 新的延迟样本（32 位微秒时间戳）
%% @param CurMilli 当前时间（毫秒）
%% @param Delay 延迟估计器
%% @returns 更新后的延迟估计器
%% @end
%%------------------------------------------------------------------------------
-spec add_sample(non_neg_integer(), integer(), aiutp_delay()) -> aiutp_delay().
add_sample(Sample, CurMilli, #aiutp_delay{delay_base_initialized = false} = Delay) ->
    %% 首个样本：初始化所有历史为该值
    %% RFC 6817: 初始 base_delay 应设为第一个观测值
    add_sample(Sample, CurMilli,
               Delay#aiutp_delay{
                   delay_base_initialized = true,
                   delay_base_hist = array:new(?DELAY_BASE_HISTORY,
                                               [{default, Sample}, {fixed, true}]),
                   delay_base = Sample
               });

add_sample(Sample, CurMilli,
           #aiutp_delay{
               delay_base_time = DelayBaseTime,
               delay_base_hist = DelayBaseHist,
               delay_base_idx = DelayBaseIdx,
               delay_base = DelayBase,
               cur_delay_hist = CurDelayHist,
               cur_delay_idx = CurDelayIdx
           } = Delay) ->

    %% 步骤 1: 更新当前分钟槽位的最小值
    %% 使用环绕安全比较：如果 Sample < 当前槽位值，则更新
    CurrentSlotValue = array:get(DelayBaseIdx, DelayBaseHist),
    SampleDiff = ?WRAPPING_DIFF_32(Sample, CurrentSlotValue),
    DelayBaseHist0 =
        if SampleDiff < 0 ->
            %% 新样本更小，更新当前槽位
            array:set(DelayBaseIdx, Sample, DelayBaseHist);
           true ->
            DelayBaseHist
        end,

    %% 步骤 2: 更新全局 delay_base
    %% 如果新样本比当前 delay_base 更小，立即更新
    %% 这确保能快速适应路由变化导致的延迟减少
    DelayBase0 =
        if ?WRAPPING_DIFF_32(Sample, DelayBase) < 0 ->
            %% 样本更旧/更小 - 不更新（这是环绕情况）
            DelayBase;
           true ->
            %% 样本更新/可能更小 - 检查是否真的更小
            %% 注意：这里的逻辑与直觉相反，因为要处理时钟环绕
            Sample
        end,

    %% 步骤 3: 计算相对延迟并存入 cur_delay_hist
    %% 相对延迟 = Sample - DelayBase（32 位环绕安全）
    RelativeDelay = aiutp_util:bit32(Sample - DelayBase),
    CurDelayHist0 = array:set(CurDelayIdx, RelativeDelay, CurDelayHist),
    CurDelayIdx0 = (CurDelayIdx + 1) rem ?CUR_DELAY_SIZE,

    %% 步骤 4: 检查是否需要滚动历史（每 60 秒）
    if CurMilli - DelayBaseTime > 60000 ->
        %% 滚动到下一个分钟槽位
        DelayBaseIdx0 = (DelayBaseIdx + 1) rem ?DELAY_BASE_HISTORY,
        %% 用当前样本初始化新槽位
        DelayBaseHist1 = array:set(DelayBaseIdx0, Sample, DelayBaseHist0),
        %% 重新计算 delay_base 为历史中的最小值
        DelayBase1 = array:foldl(
            fun(_, El, Acc) -> erlang:min(El, Acc) end,
            16#FFFFFFFF,
            DelayBaseHist1
        ),
        Delay#aiutp_delay{
            delay_base_time = CurMilli,
            delay_base_hist = DelayBaseHist1,
            delay_base_idx = DelayBaseIdx0,
            delay_base = DelayBase1,
            cur_delay_hist = CurDelayHist0,
            cur_delay_idx = CurDelayIdx0
        };
       true ->
        Delay#aiutp_delay{
            delay_base_hist = DelayBaseHist0,
            delay_base = DelayBase0,
            cur_delay_hist = CurDelayHist0,
            cur_delay_idx = CurDelayIdx0
        }
    end.

%%------------------------------------------------------------------------------
%% @doc 获取当前基准延迟
%%
%% @param Delay 延迟估计器
%% @returns 基准延迟（32 位微秒时间戳）
%% @end
%%------------------------------------------------------------------------------
-spec delay_base(aiutp_delay()) -> non_neg_integer().
delay_base(#aiutp_delay{delay_base = DelayBase}) -> DelayBase.

%%------------------------------------------------------------------------------
%% @doc 获取当前延迟估计值（排队延迟）
%%
%% 返回 cur_delay_hist 中的最小值，表示当前估计的排队延迟。
%% RFC 6817: queuing_delay = FILTER(current_delays) - MIN(base_delays)
%% 由于 cur_delay_hist 中存储的已经是相对于 delay_base 的值，
%% 所以直接返回最小值即可。
%%
%% @param Delay 延迟估计器
%% @returns 当前排队延迟估计值（微秒）
%% @end
%%------------------------------------------------------------------------------
-spec value(aiutp_delay()) -> non_neg_integer().
value(#aiutp_delay{cur_delay_hist = Hist}) ->
    %% 使用 MIN 过滤器：返回最近样本中的最小值
    %% 这提供了对噪声的鲁棒性，同时保持对拥塞信号的敏感性
    array:foldl(fun(_, El, Acc) -> erlang:min(El, Acc) end, 16#FFFFFFFF, Hist).
