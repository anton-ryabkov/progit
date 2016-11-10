%%%-------------------------------------------------------------------
%%% @author Anton N Ryabkov <anton.ryabkov@gmail.com>
%%% @copyright (C) 2011, Eltex
%%% @doc
%%%
%%% @end
%%% Created :  4 Apr 2011 by Anton N Ryabkov <anton.ryabkov@gmail.com>
%%%-------------------------------------------------------------------
-module(iua_encoder).

-include("../include/iua.hrl").
-include("../include/iua_messages.hrl").

-export([
         encode_octet/1,
         encode_int16/1,
         encode_uint16/1,
         encode_int32/1,
         encode_uint32/1,
         encode_string/1,
         encode_version/1,
         encode_message_class/1,
         encode_message_type/1,
         encode_dlci/1,
         encode_status/1,
         encode_common_message_header/1,
         encode_vl_parameter/1,
         encode_iua_message_header/1,
         encode_establish_msg/1,
         encode_release_msg/1,
         encode_data_msg/1,
         encode_unit_data_msg/1,


         encode_heartbeat_msg/1
        ]).

-spec encode_octet(Octet :: 'iua_octet'()) -> binary() | encode_error().
%%--------------------------------------------------------------------
%% Encode 'iua_octet' data type.
%%--------------------------------------------------------------------
encode_octet(Octet) when is_integer(Octet),
                         Octet >= 0,
                         Octet =< 255 ->
    <<Octet:8>>;

encode_octet(BadArg) ->
    ?encode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-spec encode_int16(Value :: integer()) -> binary() | encode_error().
%%--------------------------------------------------------------------
%% Encode int16 data type.
%%--------------------------------------------------------------------
encode_int16(Value) when is_integer(Value),
                         Value >= ?MIN_INT16,
                         Value =< ?MAX_INT16 ->
    <<Value:16>>;

encode_int16(BadArg) ->
    ?encode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec encode_uint16(Value :: integer()) -> binary() | encode_error().
%%--------------------------------------------------------------------
%% Encode uint16 data type.
%%--------------------------------------------------------------------
encode_uint16(Value) when is_integer(Value),
                          Value >= 0,
                          Value =< ?MAX_UINT16 ->
    <<Value:16>>;

encode_uint16(BadArg) ->
    ?encode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



-spec encode_int32(Value :: integer()) -> binary() | encode_error().
%%--------------------------------------------------------------------
%% Encode int32 data type.
%%--------------------------------------------------------------------
encode_int32(Value) when is_integer(Value),
                         Value >= ?MIN_INT32,
                         Value =< ?MAX_INT32 ->
    <<Value:32>>;

encode_int32(BadArg) ->
    ?encode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec encode_uint32(Value :: integer()) -> binary() | encode_error().
%%--------------------------------------------------------------------
%% Encode uint32 data type.
%%--------------------------------------------------------------------
encode_uint32(Value) when is_integer(Value),
                          Value >= 0,
                          Value =< ?MAX_UINT32 ->
    <<Value:32>>;

encode_uint32(BadArg) ->
    ?encode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec encode_string(Value :: string()) -> binary() | encode_error().
%%--------------------------------------------------------------------
%% Encode string data type.
%%--------------------------------------------------------------------
encode_string(Value) when is_list(Value) ->
    erlang:list_to_binary(Value);

encode_string(BadArg) ->
    ?encode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec encode_version(Version :: 'Version'()) -> binary() | encode_error().
%%--------------------------------------------------------------------
%% Encode 'Version' data type.
%%--------------------------------------------------------------------
encode_version(Version) ->
    encode_octet(Version).    
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec encode_message_class(MsgClass :: 'CommonMessageClass'()) -> binary() | encode_error().
%%--------------------------------------------------------------------
%% Encode 'CommonMessageClass' data type.
%%--------------------------------------------------------------------
encode_message_class(MsgClass) when is_integer(MsgClass),
                                    MsgClass >= 0,
                                    MsgClass =< 255 ->
    <<MsgClass:8>>;
encode_message_class(BadArg) ->
    ?encode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec encode_message_type(MsgType :: 'CommonMessageType'()) -> binary() | encode_error().
%%--------------------------------------------------------------------
%% Encode 'CommonMessageType' data type.
%%--------------------------------------------------------------------
encode_message_type(MsgType) when is_integer(MsgType),
                                  MsgType >= 0,
                                  MsgType =< 255 ->
    <<MsgType:8>>;
encode_message_type(BadArg) ->
    ?encode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec encode_dlci(DLCI :: #'DLCI'{}) -> binary() | encode_error().
%%--------------------------------------------------------------------
%% @doc Encode 'DLCI' data type.
%%--------------------------------------------------------------------
encode_dlci(#'DLCI'{spr = Spr, sapi = Sapi, tei = Tei}) when is_integer(Spr), Spr >= 0, Spr =< 1,
                                                             is_integer(Sapi), Sapi >= 0, Sapi =< 63,
                                                             is_integer(Tei), Tei >= 0, Tei =< 127 ->
    <<0:1, Spr:1, Sapi:6, 1:1, Tei:7>>;
encode_dlci(#'DLCI'{} = DLCI) ->
    ?encode_error({invalid_record_value, DLCI});
encode_dlci(BadArg) ->
    ?encode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec encode_status(Status :: #'STATUS'{}) -> binary() | encode_error().
%%--------------------------------------------------------------------
%% @doc Encode 'STATUS' data type.
%%--------------------------------------------------------------------
encode_status(#'STATUS'{type = Type, id = Id} = Status) ->
    if
        Type =:= ?ST_AS_STATE_CHANGE andalso
        (Id =:= ?SI_AS_DOWN orelse Id =:= ?SI_AS_INACTIVE orelse Id =:= ?SI_AS_ACTIVE orelse Id =:= ?SI_AS_PENDING)
        ->
            <<Type:16, Id:16>>;
        Type =:= ?ST_OTHER andalso
        (Id =:= ?SI_OTHER_INSUFFICIENT_ASP_RES_ACTIVE_IN_AS orelse Id =:= ?SI_OTHER_ALT_ASP_ACTIVE)
         ->
            <<Type:16, Id:16>>;
        true ->
            ?encode_error({invalid_record_value, Status})
    end;

encode_status(BadArg) ->
    ?encode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec encode_common_message_header(Msg :: #'CommonMessageHeader'{}) -> binary() | encode_error().
%%--------------------------------------------------------------------
%% Encode common message header.
%%--------------------------------------------------------------------
encode_common_message_header(#'CommonMessageHeader'{version = Version,
                                                    class = Class,
                                                    type = Type,
                                                    length = Length}) ->
    Reserved = 0,
    <<(encode_version(Version))/binary,
     Reserved:8,
     (encode_message_class(Class))/binary,
     (encode_message_type(Type))/binary,
     (encode_uint32(Length))/binary>>;
encode_common_message_header(BadArg) ->
    ?encode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec encode_vl_parameter(Msg :: #'VLParameter'{}) -> binary() | encode_error().
%%--------------------------------------------------------------------
%% Encode Variable-Length Parameter.
%%--------------------------------------------------------------------
encode_vl_parameter(#'VLParameter'{tag = ?VL_INTEGER, value = Value}) when is_integer(Value) ->
    encode_vl_parameter(?VL_INTEGER, encode_int32(Value));

encode_vl_parameter(#'VLParameter'{tag = ?VL_TEXT, value = Value}) when is_list(Value) ->
    encode_vl_parameter(?VL_TEXT, encode_string(Value));

encode_vl_parameter(#'VLParameter'{tag = ?VL_INFO_STRING, value = Value}) when is_list(Value) ->
    Length = erlang:length(Value),
    if Length =< 255 ->
            encode_vl_parameter(?VL_INFO_STRING, encode_string(Value));
       true ->
            ?encode_error({invalid_length, Length})
    end;

encode_vl_parameter(#'VLParameter'{tag = ?VL_DLCI, value = Value}) when is_record(Value, 'DLCI') ->
    encode_vl_parameter(?VL_DLCI, encode_dlci(Value));

encode_vl_parameter(#'VLParameter'{tag = ?VL_DIAGNOSTIC_INFO, value = Value}) when is_binary(Value) ->
    encode_vl_parameter(?VL_DIAGNOSTIC_INFO, Value);

encode_vl_parameter(#'VLParameter'{tag = ?VL_INTEGER_RANGE, value = Values}) when is_list(Values) ->
    BinaryValues = lists:map(fun(IntValue) -> encode_int32(IntValue) end, Values),
    encode_vl_parameter(?VL_INTEGER_RANGE, erlang:list_to_binary(BinaryValues));

encode_vl_parameter(#'VLParameter'{tag = ?VL_HEARTBEAT, value = Value}) when is_binary(Value) ->
    encode_vl_parameter(?VL_HEARTBEAT, Value);

encode_vl_parameter(#'VLParameter'{tag = ?VL_ASPDN_REASON, value = Value}) when is_integer(Value) ->
    encode_vl_parameter(?VL_ASPDN_REASON, encode_int32(Value));

encode_vl_parameter(#'VLParameter'{tag = ?VL_TRAFFIC_MODE, value = Value}) when is_integer(Value) ->
    encode_vl_parameter(?VL_TRAFFIC_MODE, encode_int32(Value));

encode_vl_parameter(#'VLParameter'{tag = ?VL_ERROR_CODE, value = Value}) when is_integer(Value) ->
    encode_vl_parameter(?VL_ERROR_CODE, encode_int32(Value));

encode_vl_parameter(#'VLParameter'{tag = ?VL_STATUS, value = Value}) when is_record(Value, 'STATUS') ->
    encode_vl_parameter(?VL_STATUS, encode_status(Value));

encode_vl_parameter(#'VLParameter'{tag = ?VL_PROTOCOL_DATA, value = Value}) when is_binary(Value) ->
    encode_vl_parameter(?VL_PROTOCOL_DATA, Value);

encode_vl_parameter(#'VLParameter'{tag = ?VL_RELEASE_REASON, value = Value} = Param) when is_integer(Value) ->
    if
        Value =:= ?RELEASE_MGMT orelse Value =:= ?RELEASE_PHYS orelse Value =:= ?RELEASE_DM orelse Value =:= ?RELEASE_OTHER ->
            encode_vl_parameter(?VL_RELEASE_REASON, encode_int32(Value));
        true ->
            ?encode_error({badarg, Param})
    end;

encode_vl_parameter(#'VLParameter'{tag = ?VL_TEI_STATUS, value = Value} = Param) when is_integer(Value) ->
    if
        Value =:= ?TEI_STATUS_ASSIGNED orelse Value =:= ?TEI_STATUS_UNASSIGNED ->
            encode_vl_parameter(?VL_TEI_STATUS, encode_int32(Value));
        true ->
            ?encode_error({badarg, Param})
    end;

encode_vl_parameter(BadArg) ->
    ?encode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec encode_iua_message_header(Header :: #'IUAMessageHeader'{}) -> binary() | encode_error().
%%--------------------------------------------------------------------
%% @doc Encode 'IUAMessageHeader' data type.
%%--------------------------------------------------------------------
encode_iua_message_header(#'IUAMessageHeader'{interface_identifier = IId, dlci = DLCI} = Header) ->
    case {IId, DLCI} of
        {#'VLParameter'{tag = ?VL_INTEGER}, #'VLParameter'{tag = ?VL_DLCI}} ->
            ok;
        {#'VLParameter'{tag = ?VL_TEXT}, #'VLParameter'{tag = ?VL_DLCI}} ->
            ok;
        _ ->
            ?encode_error({badarg, Header})
    end,
    <<(encode_vl_parameter(IId))/binary, (encode_vl_parameter(DLCI))/binary>>;

encode_iua_message_header(BadArg) ->
    ?encode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec encode_establish_msg(#'EstablishMsg'{}) -> binary() | encode_error().
%%--------------------------------------------------------------------
%% Encode 'EstablishMsg' data type.
%%--------------------------------------------------------------------
encode_establish_msg(#'EstablishMsg'{common_header = #'CommonMessageHeader'{class = ?CMC_QPTM, type = Type} = CHeader,
                                     iua_header = #'IUAMessageHeader'{} = IHeader})
  when Type =:= ?CMT_QPTM_EstablishRequest orelse
       Type =:= ?CMT_QPTM_EstablishIndication orelse
       Type =:= ?CMT_QPTM_EstablishConfirm ->
    IHeaderBinary = encode_iua_message_header(IHeader),
    MsgLength = erlang:size(IHeaderBinary) + ?COMMON_MSG_HEADER_SIZE,
    <<(encode_common_message_header(CHeader#'CommonMessageHeader'{length = MsgLength}))/binary, IHeaderBinary/binary>>;

encode_establish_msg(#'EstablishMsg'{common_header = #'CommonMessageHeader'{} = CHeader,
                                     iua_header = #'IUAMessageHeader'{}}) ->
    ?encode_error({badarg, CHeader});

encode_establish_msg(BadArg) ->
    ?encode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec encode_release_msg(#'ReleaseMsg'{}) -> binary() | encode_error().
%%--------------------------------------------------------------------
%% Encode 'ReleaseMsg' data type.
%%--------------------------------------------------------------------
encode_release_msg(#'ReleaseMsg'{common_header = #'CommonMessageHeader'{class = ?CMC_QPTM, type = Type} = CHeader,
                                 iua_header = #'IUAMessageHeader'{} = IHeader, reason = #'VLParameter'{value = RValue} = Reason})
  when Type =:= ?CMT_QPTM_ReleaseRequest orelse
       Type =:= ?CMT_QPTM_ReleaseIndication ->
    if
        % Only RELEASE_MGMT, RELEASE_DM and RELEASE_OTHER are valid
        % reason codes for a Release Request message.
        Type =:= ?CMT_QPTM_ReleaseRequest andalso
        RValue /= ?RELEASE_MGMT andalso
        RValue /= ?RELEASE_DM andalso
        RValue /= ?RELEASE_OTHER ->
            ?encode_error({invalid_reason_to_type, {{type, Type}, {reason, RValue}}});
        true ->
            ok
    end,
    IHeaderBinary = encode_iua_message_header(IHeader),
    VLParamBinary = encode_vl_parameter(Reason),
    MsgLength = erlang:size(IHeaderBinary) + erlang:size(VLParamBinary) + ?COMMON_MSG_HEADER_SIZE,
    <<(encode_common_message_header(CHeader#'CommonMessageHeader'{length = MsgLength}))/binary,
     IHeaderBinary/binary, VLParamBinary/binary>>;

encode_release_msg(#'ReleaseMsg'{common_header = #'CommonMessageHeader'{class = ?CMC_QPTM, type = ?CMT_QPTM_ReleaseConfirm} = CHeader,
                                 iua_header = #'IUAMessageHeader'{} = IHeader, reason = undefined}) ->
    IHeaderBinary = encode_iua_message_header(IHeader),
    MsgLength = erlang:size(IHeaderBinary) + ?COMMON_MSG_HEADER_SIZE,
    <<(encode_common_message_header(CHeader#'CommonMessageHeader'{length = MsgLength}))/binary, IHeaderBinary/binary>>;

encode_release_msg(#'ReleaseMsg'{common_header = #'CommonMessageHeader'{} = CHeader,
                                 iua_header = #'IUAMessageHeader'{}}) ->
    ?encode_error({badarg, CHeader});

encode_release_msg(BadArg) ->
    ?encode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec encode_data_msg(#'DataMsg'{}) -> binary() | encode_error().
%%--------------------------------------------------------------------
%% Encode 'DataMsg' data type.
%%--------------------------------------------------------------------
encode_data_msg(#'DataMsg'{common_header = #'CommonMessageHeader'{class = ?CMC_QPTM, type = Type} = CHeader,
                           iua_header = #'IUAMessageHeader'{} = IHeader,
                           data = #'VLParameter'{value = DValue} = Data})
  when (Type =:= ?CMT_QPTM_DataRequest orelse
        Type =:= ?CMT_QPTM_DataIndication) andalso
       is_binary(DValue) ->
    IHeaderBinary = encode_iua_message_header(IHeader),
    VLParamBinary = encode_vl_parameter(Data),
    MsgLength = erlang:size(IHeaderBinary) + erlang:size(VLParamBinary) + ?COMMON_MSG_HEADER_SIZE,
    <<(encode_common_message_header(CHeader#'CommonMessageHeader'{length = MsgLength}))/binary,
     IHeaderBinary/binary, VLParamBinary/binary>>;

encode_data_msg(#'DataMsg'{common_header = #'CommonMessageHeader'{} = CHeader,
                           iua_header = #'IUAMessageHeader'{}}) ->
    ?encode_error({badarg, CHeader});

encode_data_msg(BadArg) ->
    ?encode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec encode_unit_data_msg(#'UnitDataMsg'{}) -> binary() | encode_error().
%%--------------------------------------------------------------------
%% Encode 'UnitDataMsg' data type.
%%--------------------------------------------------------------------
encode_unit_data_msg(#'UnitDataMsg'{common_header = #'CommonMessageHeader'{class = ?CMC_QPTM, type = Type} = CHeader,
                               iua_header = #'IUAMessageHeader'{} = IHeader,
                               data = #'VLParameter'{value = DValue} = Data})
  when (Type =:= ?CMT_QPTM_UnitDataRequest orelse
        Type =:= ?CMT_QPTM_UnitDataIndication) andalso
       is_binary(DValue) ->
    IHeaderBinary = encode_iua_message_header(IHeader),
    VLParamBinary = encode_vl_parameter(Data),
    MsgLength = erlang:size(IHeaderBinary) + erlang:size(VLParamBinary) + ?COMMON_MSG_HEADER_SIZE,
    <<(encode_common_message_header(CHeader#'CommonMessageHeader'{length = MsgLength}))/binary,
     IHeaderBinary/binary, VLParamBinary/binary>>;

encode_unit_data_msg(#'UnitDataMsg'{common_header = #'CommonMessageHeader'{} = CHeader,
                               iua_header = #'IUAMessageHeader'{}}) ->
    ?encode_error({badarg, CHeader});

encode_unit_data_msg(BadArg) ->
    ?encode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



-spec encode_heartbeat_msg(#'HeartbeatMsg'{}) -> binary() | encode_error().
%%--------------------------------------------------------------------
%% Encode 'HeartbeatMsg' data type.
%%--------------------------------------------------------------------
encode_heartbeat_msg(#'HeartbeatMsg'{common_header = #'CommonMessageHeader'{class = ?CMC_ASPTM, type = ?CMT_ASPSM_BEAT} = CHeader,
                                     data = #'VLParameter'{value = DValue} = Data}) when is_binary(DValue) ->
    VLParamBinary = encode_vl_parameter(Data),
    MsgLength = erlang:size(VLParamBinary) + ?COMMON_MSG_HEADER_SIZE,
    <<(encode_common_message_header(CHeader#'CommonMessageHeader'{length = MsgLength}))/binary, VLParamBinary/binary>>;

encode_heartbeat_msg(#'HeartbeatMsg'{common_header = #'CommonMessageHeader'{class = ?CMC_ASPTM, type = ?CMT_ASPSM_BEAT} = CHeader, data = undefined}) ->
    MsgLength = ?COMMON_MSG_HEADER_SIZE,
    <<(encode_common_message_header(CHeader#'CommonMessageHeader'{length = MsgLength}))/binary>>;

encode_heartbeat_msg(#'HeartbeatMsg'{common_header = #'CommonMessageHeader'{} = CHeader}) ->
    ?encode_error({badarg, CHeader});

encode_heartbeat_msg(BadArg) ->
    ?encode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec encode_vl_parameter(Tag :: integer(), Value :: binary()) -> binary().
encode_vl_parameter(Tag, Value) when is_integer(Tag),
                                     is_binary(Value)->
    % Calculate length by ourself
    Length = 2 + 2 + erlang:size(Value),
    Alignment = ((4 - (Length rem 4)) rem 4) * 8,
    <<(encode_uint16(Tag))/binary, (encode_uint16(Length))/binary, Value/binary, <<0:Alignment>>/binary>>;

encode_vl_parameter(Tag, Value) ->
    ?encode_error({badarg, {Tag, Value}}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
