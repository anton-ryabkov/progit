%%%-------------------------------------------------------------------
%%% @author Anton N Ryabkov <anton.ryabkov@gmail.com>
%%% @copyright (C) 2011, Eltex
%%% @doc
%%% ISDN Q.921-User Adaptation Layer records
%%% @end
%%% Created :  4 Apr 2011 by Anton N Ryabkov <anton.ryabkov@gmail.com>
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
% Internal defines
-define(decode_error(Reason), throw({decode_error, ?MODULE, ?LINE, Reason})).
-define(encode_error(Reason), throw({encode_error, ?MODULE, ?LINE, Reason})).

-type decode_error() :: {decode_error, Module :: atom(), Line :: integer(), Reason :: term()}.
-type encode_error() :: {encode_error, Module :: atom(), Line :: integer(), Reason :: term()}.

-define(MAX_UINT8, 255).
-define(MAX_UINT16, 65535).
-define(MAX_UINT32, 4294967295).
-define(MAX_UINT64, 18446744073709551615).

-define(MAX_INT8, 127).
-define(MIN_INT8, -128).
-define(MAX_INT16, 32767).
-define(MIN_INT16, -32768).
-define(MAX_INT32, 2147483647).
-define(MIN_INT32, -2147483648).
-define(MAX_INT64, 9223372036854775807).
-define(MIN_INT64, -9223372036854775808).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-type 'iua_octet'() :: 0..?MAX_UINT8.
-type 'iua_uint16'() :: 0..?MAX_UINT16.
-type 'iua_bool'() :: true | false. % One bit value

-type 'Version'() :: 'iua_octet'().

%%--------------------------------------------------------------------
% Common message classes (CMC)

-define(CMC_MGMT, 0).  % Management (MGMT) Message [IUA/M2UA/M3UA/SUA]
-define(CMC_T, 1).     % Transfer Messages [M3UA]
-define(CMC_SSNM, 2).  % SS7 Signalling Network Management (SSNM) Messages [M3UA/SUA]
-define(CMC_ASPSM, 3). % ASP State Maintenance (ASPSM) Messages [IUA/M2UA/M3UA/SUA]
-define(CMC_ASPTM, 4). % ASP Traffic Maintenance (ASPTM) Messages [IUA/M2UA/M3UA/SUA]
-define(CMC_QPTM, 5).  % Q.921/Q.931 Boundary Primitives Transport (QPTM) Messages [IUA]
-define(CMC_MAUP, 6).  % MTP2 User Adaptation (MAUP) Messages [M2UA]
-define(CMC_CL, 7).    % Connectionless Messages [SUA]
-define(CMC_CO, 8).    % Connection-Oriented Messages [SUA]
-type 'CMC_IETF'() :: 9..127. % Reserved by the IETF
-type 'CMC_IETFD'() :: 128..255. % Reserved for IETF-Defined Message Class extensions

-type 'CommonMessageClass'() :: ?CMC_MGMT |
                                ?CMC_T |
                                ?CMC_SSNM |
                                ?CMC_ASPSM |
                                ?CMC_ASPTM |
                                ?CMC_QPTM |
                                ?CMC_MAUP |
                                ?CMC_CL |
                                ?CMC_CO |
                                'CMC_IETF'() | 'CMC_IETFD'().
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

%%--------------------------------------------------------------------
% Common message types (CMT)

% Q.921/Q.931 Boundary Primitives Transport (QPTM) Messages
-define(CMT_QPTM_Reserved, 0).            % Reserved
-define(CMT_QPTM_DataRequest, 1).         % Data Request Message
-define(CMT_QPTM_DataIndication, 2).      % Data Indication Message
-define(CMT_QPTM_UnitDataRequest, 3).     % Unit Data Request Message
-define(CMT_QPTM_UnitDataIndication, 4).  % Unit Data Indication Message
-define(CMT_QPTM_EstablishRequest, 5).    % Establish Request
-define(CMT_QPTM_EstablishConfirm, 6).    % Establish Confirm
-define(CMT_QPTM_EstablishIndication, 7). % Establish Indication
-define(CMT_QPTM_ReleaseRequest, 8).      % Release Request
-define(CMT_QPTM_ReleaseConfirm, 9).      % Release Confirm
-define(CMT_QPTM_ReleaseIndication, 10).  % Release Indication
-type 'CMT_QPTM_IETF'() :: 11..127.       % Reserved by the IETF
-type 'CMT_QPTM_IETFD'() :: 128..255.     % Reserved for IETF-Defined QPTM extensions

-type 'QPTMMessage'() :: ?CMT_QPTM_Reserved |
                          ?CMT_QPTM_DataRequest |
                          ?CMT_QPTM_DataIndication |
                          ?CMT_QPTM_UnitDataRequest |
                          ?CMT_QPTM_UnitDataIndication |
                          ?CMT_QPTM_EstablishRequest |
                          ?CMT_QPTM_EstablishConfirm |
                          ?CMT_QPTM_EstablishIndication |
                          ?CMT_QPTM_ReleaseRequest |
                          ?CMT_QPTM_ReleaseConfirm |
                          ?CMT_QPTM_ReleaseIndication |
                          'CMT_QPTM_IETF'() | 'CMT_QPTM_IETFD'().
%%--------------------------------------------------------------------

% Application Server Process State Maintenance (ASPSM) messages
-define(CMT_ASPSM_Reserved, 0).         % Reserved
-define(CMT_ASPSM_UP, 1).               % ASP Up (UP)
-define(CMT_ASPSM_DOWN, 2).             % ASP Down (DOWN)
-define(CMT_ASPSM_BEAT, 3).             % Heartbeat (BEAT)
-define(CMT_ASPSM_UP_ACK, 4).           % ASP Up Ack (UP ACK)
-define(CMT_ASPSM_DOWN_ACK, 5).         % ASP Down Ack (DOWN ACK)
-define(CMT_ASPSM_BEAT_ACK, 6).         % Heatbeat Ack (BEAT ACK)
-type 'CMT_ASPSM_IETF'() :: 7..127.     % Reserved by the IETF
-type 'CMT_ASPSM_IETFD'() :: 128..255.  % Reserved for IETF-Defined ASPSM extensions

-type 'ASPSMMessage'() :: ?CMT_ASPSM_Reserved |
                           ?CMT_ASPSM_UP |
                           ?CMT_ASPSM_DOWN |
                           ?CMT_ASPSM_BEAT |
                           ?CMT_ASPSM_UP_ACK |
                           ?CMT_ASPSM_DOWN_ACK |
                           ?CMT_ASPSM_BEAT_ACK |
                           'CMT_ASPSM_IETF'() | 'CMT_ASPSM_IETFD'().
%%--------------------------------------------------------------------

% Application Server Process Traffic Maintenance (ASPTM) messages
-define(CMT_ASPTM_Reserved, 0).         % Reserved
-define(CMT_ASPTM_ACTIVE, 1).           % ASP Active
-define(CMT_ASPTM_INACTIVE, 2).         % ASP Inactive
-define(CMT_ASPTM_ACTIVE_ACK, 3).       % ASP Active Ack
-define(CMT_ASPTM_INACTIVE_ACK, 4).     % ASP Inactive Ack
-type 'CMT_ASPTM_IETF'() :: 5..127.     % Reserved by the IETF
-type 'CMT_ASPTM_IETFD'() :: 128..255.  % Reserved for IETF-Defined ASPSM extensions

-type 'ASPTMMessage'() :: ?CMT_ASPTM_Reserved |
                           ?CMT_ASPTM_ACTIVE |
                           ?CMT_ASPTM_INACTIVE |
                           ?CMT_ASPTM_ACTIVE_ACK |
                           ?CMT_ASPTM_INACTIVE_ACK |
                           'CMT_ASPTM_IETF'() | 'CMT_ASPTM_IETFD'().
%%--------------------------------------------------------------------

% Management (MGMT) Messages
-define(CMT_MGMT_ERR, 0).              % Error
-define(CMT_MGMT_NTFY, 1).             % Notify
-define(CMT_MGMT_TEI_Request, 2).      % TEI Status Request
-define(CMT_MGMT_TEI_Confirm, 3).      % TEI Status Confirm
-define(CMT_MGMT_TEI_Indication, 4).   % TEI Status Indication
-type 'CMT_MGMT_IETF'() :: 5..127.     % Reserved by the IETF
-type 'CMT_MGMT_IETFD'() :: 128..255.  % Reserved for IETF-Defined ASPSM extensions

-type 'MGMTMessage'() :: ?CMT_MGMT_ERR |
                         ?CMT_MGMT_NTFY |
                         ?CMT_MGMT_TEI_Request |
                         ?CMT_MGMT_TEI_Confirm |
                         ?CMT_MGMT_TEI_Indication |
                         'CMT_MGMT_IETF'() | 'CMT_MGMT_IETFD'().
%%--------------------------------------------------------------------

-type 'CommonMessageType'() :: 'QPTMMessage'() | 'ASPSMMessage'() | 'ASPTMMessage'() | 'MGMTMessage'().

-record('CommonMessageHeader',
        {
          version = 1  :: 'Version'(),  % The version field contains the version of the IUA adaptation layer.
          reserved = 0 :: 'iua_octet'(),    % The Reserved field is 8-bits.  It SHOULD be set to all '0's and ignored by the receiver.
          class        :: 'CommonMessageClass'(),
          type         :: 'CommonMessageType'(),
          length       :: integer()     %The Message length defines the length of the message in octets, including the Common header.
         }).
-type 'CommonMessageHeader'() :: #'CommonMessageHeader'{}.

-define(COMMON_MSG_HEADER_SIZE, 8).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

%%--------------------------------------------------------------------
% Variable-Length Parameter Format

% Variable-Length Parameter Variable's types
-define(VL_INTEGER,         16#1).
-define(VL_TEXT,            16#3).  % TODO Rename to VL_STRING.
-define(VL_INFO_STRING,     16#4).  % any meaningful 8-bit ASCII character string along with the message.  Length of the INFO String parameter is from 0 to 255 characters.
-define(VL_DLCI,            16#5).
-define(VL_DIAGNOSTIC_INFO, 16#7).
-define(VL_INTEGER_RANGE,   16#8).
-define(VL_HEARTBEAT,       16#9).
-define(VL_ASPDN_REASON,    16#A).
-define(VL_TRAFFIC_MODE,    16#B).
-define(VL_ERROR_CODE,      16#C).
-define(VL_STATUS,          16#D).
-define(VL_PROTOCOL_DATA,   16#E).  % The protocol data contains upper layer signaling message e.g.  Q.931, QSIG.
-define(VL_RELEASE_REASON,  16#F).  % Value of 'ReleaseType'.
-define(VL_TEI_STATUS,      16#10).

-type 'VLType'() :: ?VL_INTEGER |
                    ?VL_TEXT |
                    ?VL_INFO_STRING |
                    ?VL_DLCI |
                    ?VL_DIAGNOSTIC_INFO |
                    ?VL_INTEGER_RANGE |
                    ?VL_HEARTBEAT |
                    ?VL_ASPDN_REASON |
                    ?VL_TRAFFIC_MODE |
                    ?VL_ERROR_CODE |
                    ?VL_STATUS |
                    ?VL_PROTOCOL_DATA |
                    ?VL_RELEASE_REASON |
                    ?VL_TEI_STATUS.

-record('VLParameter',
        {
          tag     :: 'VLType'(),     % The Tag field is a 16 bit identifier of the type of parameter.  It  takes a value of 0 to 65534.
                                     % The value of 65535 is reserved for IETF-defined extensions.  Values other than those defined in
                                     % specific parameter description are reserved for use by the IETF.
          length  :: 'iua_uint16'(), % The Parameter Length field contains the size of the parameter in bytes, including the Parameter Tag,
                                     % Parameter Length, and Parameter Value fields.  The Parameter Length does not include any padding bytes.
          value   :: term()        % contains the actual information to be transferred in the parameter
         }).                         % Note: The total length of a parameter (including Tag, Parameter Length and Value fields) MUST be a multiple of 4 bytes.
-type 'VLParameter'() :: #'VLParameter'{}.
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

%%--------------------------------------------------------------------
% Data Link Connection Identifier.
-record('DLCI', {
          '_first_bit' = 0 :: 0,  % First bit in first octet MUST be zero.
          spr :: 0..1,            % Spare
          sapi :: 0..63,          % Service Access Point Identifier
          '_second_bit' = 1 :: 1, % First bit in second octet MUST be zero.
          tei :: 0..127           % Terminal Endpoint Identifier
         }).
-type 'DLCI'() :: #'DLCI'{}.
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

%%--------------------------------------------------------------------
% IUA Message Header
-record('IUAMessageHeader',
        {
          interface_identifier :: #'VLParameter'{}, % The Interface Identifier identifies the physical interface terminating
                                                    % the signaling channel at the SG for which the signaling messages are
                                                    % sent/received.
          dlci       :: #'VLParameter'{}           % Data Link Connection Identifier.
         }).
-type 'IUAMessageHeader'() :: #'IUAMessageHeader'{}.
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

%%--------------------------------------------------------------------
% IUA Messages
-define(RELEASE_MGMT, 0).  % Management layer generated release.
-define(RELEASE_PHYS, 1).  % Physical layer alarm generated release.
-define(RELEASE_DM, 2).    % Specific to a request.  Indicates Layer 2 SHOULD release and deny all requests from
                           % far end to establish a data link on the signaling channel (i.e., if SABME is received send a DM)
-define(RELEASE_OTHER, 3). % Other reasons

-type 'ReleaseType'() :: ?RELEASE_MGMT |
                         ?RELEASE_PHYS |
                         ?RELEASE_DM |
                         ?RELEASE_OTHER. % Note:  Only RELEASE_MGMT, RELEASE_DM and RELEASE_OTHER are valid reason codes for a Release Request message.
%%--------------------------------------------------------------------

% ASP Down reason
-define(ASPDN_MGMT_INHIBIT_REASON, 16#1).
-type 'ASPDownReason'() :: ?ASPDN_MGMT_INHIBIT_REASON.

% Trafic mode
-define(OVER_RIDE_TRAFIC_MODE, 16#1).
-define(LOAD_SHARE_TRAFIC_MODE, 16#2).
-type 'TraficMode'() :: ?OVER_RIDE_TRAFIC_MODE | ?LOAD_SHARE_TRAFIC_MODE.

% Error code
-define(InvalidVersion, 16#1).                     % The "Invalid Version" error would be sent if a message was received
                                                   % with an invalid or unsupported version.  The Error message would
                                                   % contain the supported version in the Common header.  The Error
                                                   % message could optionally provide the supported version in the Diagnostic Information area.

-define(InvalidInterfaceIdentifier, 16#2).         % The "Invalid Interface Identifier" error would be sent by a SG if an
                                                   % ASP sends a message with an invalid (unconfigured) Interface Identifier value.

-define(UnsupportedMessageClass, 16#3).            % The "Unsupported Message Class" error would be sent if a message with
                                                   % an unexpected or unsupported Message Class is received.

-define(UnsupportedMessageType, 16#4).             % The "Unsupported Message Type" error would be sent if a message with
                                                   % an unexpected or unsupported Message Type is received.

-define(UnsupportedTrafficHandlingMode, 16#5).     % The "Unsupported Traffic Handling Mode" error would be sent by a SG
                                                   % if an ASP sends an ASP Active with an unsupported Traffic Handling
                                                   % Mode.  An example would be a case in which the SG did not support load-sharing.

-define(UnexpectedMessage, 16#6).                  % The "Unexpected Message" error would be sent by an ASP if it received
                                                   % a QPTM message from an SG while it was in the Inactive state (the ASP
                                                   % could optionally drop the message and not send an Error).  It would
                                                   % also be sent by an ASP if it received a defined and recognized
                                                   % message that the SG is not expected to send (e.g., if the MGC
                                                   % receives an IUA Establish Request message).

-define(ProtocolError, 16#7).                      % The "Protocol Error" error would be sent for any protocol anomaly (i.e., a bogus message).

-define(UnsupportedInterfaceIdentifierType, 16#8). % The "Unsupported Interface Identifier Type" error would be sent by a
                                                   % SG if an ASP sends a Text formatted Interface Identifier and the SG
                                                   % only supports Integer formatted Interface Identifiers.  When the ASP
                                                   % receives this error, it will need to resend its message with an
                                                   % Integer formatted Interface Identifier.

-define(InvalidStreamIdentifier, 16#9).            % The "Invalid Stream Identifier" error would be sent if a message was
                                                   % received on an unexpected SCTP stream (i.e., a MGMT message was received on a stream other than "0").

-define(UnassignedTEI, 16#A).                      % The "Unassigned TEI" error may be used when the SG receives an IUA
                                                   % message that includes a TEI which has not been assigned or recognized
                                                   % for use on the indicated ISDN D-channel.

-define(UnrecognizedSAPI, 16#B).                   % The "Unrecognized SAPI" error would handle the case of using a SAPI
                                                   % that is not recognized by the SG.

-define(InvalidTEI_SAPI_Combination, 16#C).        % The "Invalid TEI, SAPI combination" error identify errors where the TEI is assigned and the
                                                   % the SAPI is recognized, but the combination is not valid for the interface (e.g., on a BRI
                                                   % the MGC tries to send Q.921 Management messages via IUA when Layer Management at the SG SHOULD
                                                   % be performing this function).

-type 'ErrorCode'() :: ?InvalidVersion |
                       ?InvalidInterfaceIdentifier |
                       ?UnsupportedMessageClass |
                       ?UnsupportedMessageType |
                       ?UnsupportedTrafficHandlingMode |
                       ?UnexpectedMessage |
                       ?ProtocolError |
                       ?UnsupportedInterfaceIdentifierType |
                       ?InvalidStreamIdentifier |
                       ?UnassignedTEI |
                       ?UnrecognizedSAPI |
                       ?InvalidTEI_SAPI_Combination.

% Status type
-define(ST_AS_STATE_CHANGE, 16#1). % Application Server state change
-define(ST_OTHER, 16#2).

-type 'StatusType'() :: ?ST_AS_STATE_CHANGE | ?ST_OTHER.

% Status identification
-define(SI_AS_DOWN, 16#1).                                 % Application Server Down
-define(SI_AS_INACTIVE, 16#2).                             % Application Server Inactive
-define(SI_AS_ACTIVE, 16#3).                               % Application Server Active
-define(SI_AS_PENDING, 16#4).                              % Application Server Pending
-define(SI_OTHER_INSUFFICIENT_ASP_RES_ACTIVE_IN_AS, 16#1). % Insufficient ASP resources active in AS
-define(SI_OTHER_ALT_ASP_ACTIVE, 16#2).                    % Alternate ASP Active

-type 'StatusIdentification'() :: ?SI_AS_DOWN |
                                  ?SI_AS_INACTIVE |
                                  ?SI_AS_ACTIVE |
                                  ?SI_AS_PENDING |
                                  ?SI_OTHER_INSUFFICIENT_ASP_RES_ACTIVE_IN_AS |
                                  ?SI_OTHER_ALT_ASP_ACTIVE.
% Status record
-record('STATUS', {
          type :: 'StatusType'(),
          id :: 'StatusIdentification'()
         }).

% TEI Status
-define(TEI_STATUS_ASSIGNED, 16#0).   % TEI is considered assigned by Q.921
-define(TEI_STATUS_UNASSIGNED, 16#1). % TEI is considered unassigned by Q.921

-type 'TEIStatus'() :: ?TEI_STATUS_ASSIGNED |
                       ?TEI_STATUS_UNASSIGNED.
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
