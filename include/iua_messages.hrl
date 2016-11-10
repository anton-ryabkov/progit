%%%-------------------------------------------------------------------
%%% @author Anton N Ryabkov <anton.ryabkov@gmail.com>
%%% @copyright (C) 2011, Eltex
%%% @doc
%%%
%%% @end
%%% Created : 22 Apr 2011 by Anton N Ryabkov <anton.ryabkov@gmail.com>
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% The Establish Messages are used to establish a data link on the
%%    signaling channel or to confirm that a data link on the signaling
%%    channel has been established.
%%--------------------------------------------------------------------
-record('EstablishMsg',
        {
          common_header :: #'CommonMessageHeader'{},
          iua_header :: #'IUAMessageHeader'{}
         }
       ).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

%%--------------------------------------------------------------------
%% The Release Request message is used to release the data link on the
%%    signaling channel.  The Release Confirm and Indication messages are
%%    used to indicate that the data link on the signaling channel has been
%%    released.
%%--------------------------------------------------------------------
-record('ReleaseMsg',
        {
          common_header :: #'CommonMessageHeader'{},
          iua_header :: #'IUAMessageHeader'{},
          reason :: #'VLParameter'{} | undefined % undefined MUST be use to Release Confirm message.
         }
       ).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

%%--------------------------------------------------------------------
%% The Data message contains an ISDN Q.921-User Protocol Data Unit (PDU)
%%    corresponding to acknowledged information transfer service.
%%--------------------------------------------------------------------
-record('DataMsg',
        {
          common_header :: #'CommonMessageHeader'{},
          iua_header :: #'IUAMessageHeader'{},
          data :: #'VLParameter'{}
         }
       ).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

%%--------------------------------------------------------------------
%% The Unit Data message contains an ISDN Q.921-User Protocol Data Unit
%%   (PDU) corresponding to unacknowledged information transfer service.
%%--------------------------------------------------------------------
-record('UnitDataMsg',
        {
          common_header :: #'CommonMessageHeader'{},
          iua_header :: #'IUAMessageHeader'{},
          data :: #'VLParameter'{}
         }
       ).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



%%--------------------------------------------------------------------
%% The ASP Up (ASPUP) message is sent by an ASP to indicate to an SG
%%    that it is ready to receive traffic or maintenance messages.
%%--------------------------------------------------------------------
-record('ASPUpMsg',
        {
          common_header :: #'CommonMessageHeader'{},
          info :: #'VLParameter'{} | undefined       % INFO String
         }
       ).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

%%--------------------------------------------------------------------
%% The ASP Up Ack message is used to acknowledge an ASP Up message
%%    received from a remote IUA peer.
%%--------------------------------------------------------------------
-record('ASPUpAckMsg',
        {
          common_header :: #'CommonMessageHeader'{},
          info :: #'VLParameter'{} | undefined       % INFO String
         }
       ).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


%%--------------------------------------------------------------------
%% The ASP Down (ASPDN) message is sent by an ASP to indicate to an SG
%%   that it is NOT ready to receive traffic or maintenance messages.
%%--------------------------------------------------------------------
-record('ASPDNMsg',
        {
          common_header :: #'CommonMessageHeader'{},
          reason :: #'VLParameter'{},                 % Reason
          info :: #'VLParameter'{} | undefined        % INFO String
         }
       ).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

%%--------------------------------------------------------------------
%% The ASP Down Ack message is used to acknowledge an ASP Down message
%%    received from a remote IUA peer.
%%--------------------------------------------------------------------
-record('ASPDNAckMsg',
        {
          common_header :: #'CommonMessageHeader'{},
          reason :: #'VLParameter'{},                 % Reason
          info :: #'VLParameter'{} | undefined        % INFO String
         }
       ).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++





%%--------------------------------------------------------------------
%% The Heartbeat message is optionally used to ensure that the IUA peers
%%    are still available to each other.  It is recommended for use when
%%    the IUA runs over a transport layer other than the SCTP, which has
%%    its own heartbeat.
%%--------------------------------------------------------------------
-record('HeartbeatMsg',
        {
          common_header :: #'CommonMessageHeader'{},
          data :: #'VLParameter'{} | undefined        % Heartbeat Data
         }
       ).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

%%--------------------------------------------------------------------
%% The Heartbeat Ack message is sent in response to a received Heartbeat
%%    message.  It includes all the parameters of the received Heartbeat
%%    message, without any change.
%%--------------------------------------------------------------------
-record('HeartbeatActMsg',
        {
          common_header :: #'CommonMessageHeader'{},
          data :: #'VLParameter'{} | undefined        % Heartbeat Data
         }
       ).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
