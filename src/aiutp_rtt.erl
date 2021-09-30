-module(aiutp_rtt).
-include("aiutp.hrl").
-export([caculate_delay/4,
         caculate_rtt/3]).

caculate_delay(Now,MicroNow,
               #aiutp_packet{tv_usec = TS, reply_micro = TSDiff},
               #aiutp_pcb{their_hist = TheirHist,our_hist = OurHist,
                          current_delay_sum = CurrentDelaySum,
                          current_delay_samples = CurrentDelaySamples,
                          average_delay = PrevAverageDelay,
                          average_sample_time = AverageSampleTime,
                          average_delay_base = AverageDelayBase} = PCB)->
  TheirDelay =
    if TS > 0 -> MicroNow - TS;
       true -> 0
    end,
  PrevDelayBase = aiutp_delay:delay_base(TheirHist),
  TheirHist0 =
    if TheirDelay > 0 -> aiutp_delay:add_sample(TheirDelay,Now,TheirHist);
       true -> TheirHist
    end,
  DelayBase = aiutp_delay:delay_base(TheirHist0),
  OurHist0 =
    if (DelayBase /= 0) and
        (?WRAPPING_DIFF_32(DelayBase,PrevDelayBase) < 0) ->
        DelayBaseShift = PrevDelayBase - DelayBase,
        if DelayBaseShift =< 10000 -> aiutp_delay:shift(DelayBaseShift,OurHist);
           true -> OurHist
        end;
       true -> OurHist
    end,
  ActualDelay = TSDiff band ?TIMESTAMP_MASK,
  PCB0 =
    if ActualDelay /= 0 ->
        OurHist1 = aiutp_delay:add_sample(ActualDelay,Now,OurHist0),
        AverageDelayBase0 =
          if AverageDelayBase == 0 -> ActualDelay;
             true -> AverageDelayBase
          end,
        DistDown = AverageDelayBase0 - ActualDelay,
        DistUp = ActualDelay - AverageDelayBase0,
        AverageDelaySample =
          if DistDown > DistUp -> DistUp;
             true -> 0  - DistDown
          end,
        CurrentDelaySum0 = CurrentDelaySum + AverageDelaySample,
        if Now > AverageSampleTime ->
            AverageDelay  = math:floor(CurrentDelaySum0 / (CurrentDelaySamples + 1)),
            MinSample = ?MIN(PrevAverageDelay,AverageDelay),
            MaxSample = ?MAX(PrevAverageDelay,AverageDelay),
            Adjust =
              if MinSample > 0 -> 0 - MinSample;
                 MaxSample < 0 -> 0 - MaxSample;
                 true -> 0
              end,
            {AverageDelayBase1,AverageDelay0} =
              if Adjust /= 0 -> {AverageDelayBase0 - Adjust,AverageDelay + Adjust};
                 true -> {AverageDelayBase0,AverageDelay}
              end,
            PCB#aiutp_pcb{ reply_micro = TheirDelay,last_measured_delay = Now,
                           their_hist = TheirHist0,our_hist = OurHist1,
                           clock_drift = AverageDelay - PrevAverageDelay,
                           average_delay_base = AverageDelayBase1,
                           average_delay = AverageDelay0,
                           average_sample_time = AverageSampleTime + 5000,
                           current_delay_sum = 0,
                           current_delay_samples = 0};
           true->
            PCB#aiutp_pcb{ reply_micro = TheirDelay,last_measured_delay = Now,
                           their_hist = TheirHist0,our_hist = OurHist1,
                           average_delay_base = AverageDelayBase0,
                           current_delay_sum = CurrentDelaySum0,
                           current_delay_samples = CurrentDelaySamples + 1}
        end;
       true ->
        PCB#aiutp_pcb{ reply_micro = TheirDelay,last_measured_delay = Now,
                       their_hist = TheirHist0}
    end,
  {ActualDelay,PCB0}.

caculate_rtt(RTT,RTTVar,TimeSent)->
  MicroNow = aiutp_util:microsecond(),
  ERTT = aiutp_util:bit32((MicroNow - TimeSent) div 1000),
  if RTT == 0 -> {ERTT,ERTT div 2,ERTT};
     true ->
      Delta = RTT - ERTT,
      RTTVar0 = RTTVar + (erlang:abs(Delta) - RTTVar) div 4,
      RTT0 = RTT - RTT div 8 + ERTT div 8,
      {RTT0,RTTVar0,ERTT}
  end.
