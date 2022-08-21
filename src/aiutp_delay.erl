-module(aiutp_delay).
-include("aiutp.hrl").

-export([new/0,
         new/1,
         shift/2,
         add_sample/3,
         delay_base/1,
         value/1]).

-record(aiutp_delay, {delay_base,
                      cur_delay_hist,
                      % this is the history of delay samples,
                      % normalized by using the delay_base. These
                      % values are always greater than 0 and measures
                      % the queuing delay in microseconds
                      cur_delay_idx,
                      delay_base_hist,
                      % this is the history of delay_base. It's
                      % a number that doesn't have an absolute meaning
                      % only relative. It doesn't make sense to initialize
                      % it to anything other than values relative to
                      % what's been seen in the real world.
                      delay_base_idx,
                      delay_base_time,
                      % the time when we last stepped the delay_base_idx
                      delay_base_initialized}).
new() -> new(0).
new(CurMilli)->
  #aiutp_delay{
     delay_base = 0,
     cur_delay_hist = array:new(?CUR_DELAY_SIZE,[{default,0},{fixed, true}]),
     cur_delay_idx = 0,
     delay_base_idx = 0,
     delay_base_time = CurMilli,
     delay_base_initialized = false
    }.


shift(Offset,#aiutp_delay{delay_base_hist = Hist} = Delay)->
  Hist0 = array:map(fun(_,El) -> El + Offset end, Hist),
  Delay#aiutp_delay{delay_base_hist = Hist0}.
% the offset should never be "negative"
% assert(offset < 0x10000000);
% increase all of our base delays by this amount
% this is used to take clock skew into account
% by observing the other side's changes in its base_delay

add_sample(Sample,CurMilli,#aiutp_delay{delay_base_initialized = false} = Delay)->
    % delay_base being 0 suggests that we haven't initialized
    % it or its history with any real measurements yet. Initialize
    % everything with this sample.
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
      DelayBase1 = array:get(0,DelayBaseHist1),
      DelayBase2 = array:foldl(
        fun(_,El,Acc) -> 
          if ?WRAPPING_DIFF_32(El,Acc) < 0 -> El;
              true -> Acc
           end
        end,
        DelayBase1,DelayBaseHist1),
      Delay#aiutp_delay{
        delay_base_time = CurMilli,
        delay_base_hist = DelayBaseHist1,
        delay_base_idx = DelayBaseIdx0,
        delay_base = DelayBase2,
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

% The two clocks (in the two peers) are assumed not to
% progress at the exact same rate. They are assumed to be
% drifting, which causes the delay samples to contain
% a systematic error, either they are under-estimated
% or over-estimated. This is why we update the
% delay_base every two minutes, to adjust for this.
% This means the values will keep drifting and eventually wrap.
% We can cross the wrapping boundry in two directions, either
% going up, crossing the highest value, or going down, crossing 0.
% if the delay_base is close to the max value and sample actually
% wrapped on the other end we would see something like this:
% delay_base = 0xffffff00, sample = 0x00000400
% sample - delay_base = 0x500 which is the correct difference
% if the delay_base is instead close to 0, and we got an even lower
% sample (that will eventually update the delay_base), we may see
% something like this:
% delay_base = 0x00000400, sample = 0xffffff00
%  sample - delay_base = 0xfffffb00
% this needs to be interpreted as a negative number and the actual
% recorded delay should be 0.
% It is important that all arithmetic that assume wrapping
% is done with unsigned intergers. Signed integers are not guaranteed
% to wrap the way unsigned integers do. At least GCC takes advantage
% of this relaxed rule and won't necessarily wrap signed ints.
% remove the clock offset and propagation delay.
% delay base is min of the sample and the current
% delay base. This min-operation is subject to wrapping
% and care needs to be taken to correctly choose the
%  true minimum.
% specifically the problem case is when delay_base is very small
% and sample is very large (because it wrapped past zero), sample
%  needs to be considered the smaller

delay_base(#aiutp_delay{delay_base = DelayBase}) -> DelayBase.
value(#aiutp_delay{cur_delay_hist = Hist})->
  array:foldl(fun(_,El,Acc) -> erlang:min(El,Acc) end, 16#FFFFFFFF, Hist).
