-module(ai_utp_net).
-include("ai_utp.hrl").

-export([handle_packet/4]).

-define(REORDER_BUFFER_MAX_SIZE,1024).

-record(ai_utp_net,
        {%%sndbuf setting, in bytes
         opt_sndbuf = 0,
         %%rcvbuf setting, in bytes
         opt_rcvbuf = 0,
         retransmit_count = 0,
         reorder_count = 0,
         %% the number of packets in the send queue. Packets that haven't
         %% yet been sent count as well as packets marked as needing resend
         %% the oldest un-acked packet in the send queue is seq_nr - cur_window_packets
         cur_window_packets = 0,
         %% how much of the window is used, number of bytes in-flight
         %% packets that have not yet been sent do not count, packets
         %% that are marked as needing to be re-sent (due to a timeout)
         %% don't count either
         cur_window = 0,
         %% maximum window size, in bytes
         max_window = 0,
         %% max receive window for other end, in bytes
         max_peer_window = 0,
         %% All sequence numbers up to including this have been properly received
         %% by us
         ack_nr = 1,
         %% This is the sequence number for the next packet to be sent.
         seq_nr = 1,
         inbuf = array:new([{size,16#FFFF},{fixed,true}]),
         outbuf = array:new([{size,16#FFFF},{fixed,true}])
        }).


handle_packet(syn_sent,Net,
              #packet{type = st_state,
                      seq_no = SeqNo
                     },_)->
  {ok,Net#ai_utp_net{ack_nr = ai_utp_util:bit16(SeqNo-1)} }.
%%recv_packet(State,Net,Packet,Timing)->
