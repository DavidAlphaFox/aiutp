-module(aiutp_delay).
-include("aiutp.hrl").

-export([new/0,
         new/1,
         shift/2,
         add_sample/3,
         delay_base/1,
         value/1]).

-export_type([aiutp_delay/0]).

-record(aiutp_delay, {delay_base :: non_neg_integer(),
                      cur_delay_hist :: array:array(),
                      %% 延迟样本历史，使用 delay_base 归一化
                      %% 这些值始终大于 0，以微秒为单位测量排队延迟
                      cur_delay_idx :: non_neg_integer(),
                      delay_base_hist :: array:array() | undefined,
                      %% delay_base 的历史记录。
                      %% 这是一个没有绝对意义的数字，只有相对意义。
                      delay_base_idx :: non_neg_integer(),
                      delay_base_time :: integer(),
                      %% 上次步进 delay_base_idx 的时间
                      delay_base_initialized :: boolean()}).

-opaque aiutp_delay() :: #aiutp_delay{}.

-spec new() -> aiutp_delay().
new() -> new(0).
-spec new(integer()) -> aiutp_delay().
new(CurMilli)->
  #aiutp_delay{
     delay_base = 0,
     cur_delay_hist = array:new(?CUR_DELAY_SIZE,[{default,0},{fixed, true}]),
     cur_delay_idx = 0,
     delay_base_idx = 0,
     delay_base_time = CurMilli,
     delay_base_initialized = false
    }.

-spec shift(non_neg_integer(), aiutp_delay()) -> aiutp_delay().
shift(Offset,#aiutp_delay{delay_base_hist = Hist} = Delay)->
  Hist0 = array:map(fun(_,El) -> El + Offset end, Hist),
  Delay#aiutp_delay{delay_base_hist = Hist0}.
%% offset 不应为负值
%% assert(offset < 0x10000000);
%% 将所有基准延迟增加此值
%% 用于通过观察对端 base_delay 的变化来考虑时钟偏移

-spec add_sample(non_neg_integer(), integer(), aiutp_delay()) -> aiutp_delay().
add_sample(Sample,CurMilli,#aiutp_delay{delay_base_initialized = false} = Delay)->
    %% delay_base 为 0 表示我们尚未用任何实际测量值
    %% 初始化它或它的历史记录。用此样本初始化所有内容。
  add_sample(Sample,CurMilli,
            Delay#aiutp_delay{delay_base_initialized = true,
                             delay_base_hist = array:new(?DELAY_BASE_HISTORY,[{default,Sample},{fixed,true}]),
                             delay_base = Sample});
add_sample(Sample,CurMilli,
           #aiutp_delay{delay_base_time = DelayBaseTime,
                        delay_base_hist = DelayBaseHist,
                        delay_base_idx = DelayBaseIdx,
                        delay_base = DelayBase,
                        cur_delay_hist = CurDelayHist,
                        cur_delay_idx = CurDelayIdx} = Delay) ->
  SampleDiff = ?WRAPPING_DIFF_32(Sample,array:get(DelayBaseIdx, DelayBaseHist)),
  DelayBaseHist0 =
    if SampleDiff < 0 ->
        array:set(DelayBaseIdx,Sample,DelayBaseHist);
       true -> DelayBaseHist
    end,
  DelayBase0 =
    if ?WRAPPING_DIFF_32(Sample,DelayBase) < 0 -> DelayBase;
       true -> Sample
    end,

  DelayTime = aiutp_util:bit32(Sample - DelayBase),
  CurDelayHist0 = array:set(CurDelayIdx,DelayTime,CurDelayHist),
  CurDelayIdx0 = (CurDelayIdx + 1 ) rem ?CUR_DELAY_SIZE,
  if CurMilli - DelayBaseTime > 60000 ->
      DelayBaseIdx0 = (DelayBaseIdx + 1 ) rem ?DELAY_BASE_HISTORY,
      DelayBaseHist1 = array:set(DelayBaseIdx0,Sample,DelayBaseHist0),
      DelayBase1 =
        array:foldl(fun(_,El,Acc) -> erlang:min(El,Acc) end,16#FFFFFFFF,DelayBaseHist1),
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

%% 假设两端的时钟不会以完全相同的速率前进。
%% 它们被假设为漂移的，这导致延迟样本包含系统误差，
%% 要么被低估，要么被高估。这就是为什么我们每两分钟
%% 更新一次 delay_base 来调整这个问题。
%% 这意味着值会持续漂移并最终回绕。
%% 我们可以从两个方向跨越回绕边界：
%% 向上跨越最高值，或向下跨越 0。
%% 如果 delay_base 接近最大值，而 sample 实际上在另一端回绕了，
%% 我们会看到类似这样的情况：
%% delay_base = 0xffffff00, sample = 0x00000400
%% sample - delay_base = 0x500 这是正确的差值
%% 如果 delay_base 接近 0，我们得到一个更低的 sample
%% （最终会更新 delay_base），我们可能会看到：
%% delay_base = 0x00000400, sample = 0xffffff00
%% sample - delay_base = 0xfffffb00
%% 这需要被解释为负数，实际记录的延迟应该是 0。
%% 重要的是所有假设回绕的算术都使用无符号整数完成。

-spec delay_base(aiutp_delay()) -> non_neg_integer().
delay_base(#aiutp_delay{delay_base = DelayBase}) -> DelayBase.

-spec value(aiutp_delay()) -> non_neg_integer().
value(#aiutp_delay{cur_delay_hist = Hist})->
  array:foldl(fun(_,El,Acc) -> erlang:min(El,Acc) end, 16#FFFFFFFF, Hist).
