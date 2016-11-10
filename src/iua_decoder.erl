%%%-------------------------------------------------------------------
%%% @author Anton N Ryabkov <anton.ryabkov@gmail.com>
%%% @copyright (C) 2016, Eltex
%%% @doc
%%% 
%%% @end
%%% Created :  10 Nov 2016 by Anton N Ryabkov <anton.ryabkov@gmail.com>
%%%-------------------------------------------------------------------
-module(iua_decoder).

-include("../include/iua.hrl").
-include("../include/iua_messages.hrl").

-export([
         decode_octet/1,
         decode_int32/1,
         decode_uint32/1,
         decode_int16/1,
         decode_uint16/1,
         decode_string/2,
         decode_version/1,
         decode_message_class/1,
         decode_message_type/1,
         decode_dlci/1,
         decode_status/1,
         decode_common_message_header/1,
         decode_vl_parameter/1,
         decode_iua_message_header/1,
         decode_msg/1
        ]).

-spec decode_octet(Binary :: binary()) -> {Octet :: integer(), Tail :: binary()} | decode_error().
%%--------------------------------------------------------------------
%% Decode 'iua_octet' data type.
%%--------------------------------------------------------------------
decode_octet(<<Octet:8, Tail/bytes>>) ->
    {Octet, Tail};

decode_octet(BadArg) ->
    ?decode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec decode_int16(Binary :: binary()) -> {Octet :: integer(), Tail :: binary()} | decode_error().
%%--------------------------------------------------------------------
%% Decode int16 data type.
%%--------------------------------------------------------------------
decode_int16(<<Val:16/signed-integer, Tail/bytes>>) ->
    {Val, Tail};

decode_int16(BadArg) ->
    ?decode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-spec decode_uint16(Binary :: binary()) -> {Octet :: integer(), Tail :: binary()} | decode_error().
%%--------------------------------------------------------------------
%% Decode int16 data type.
%%--------------------------------------------------------------------
decode_uint16(<<Val:16, Tail/bytes>>) ->
    {Val, Tail};

decode_uint16(BadArg) ->
    ?decode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec decode_int32(Binary :: binary()) -> {Octet :: integer(), Tail :: binary()} | decode_error().
%%--------------------------------------------------------------------
%% Decode int32 data type.
%%--------------------------------------------------------------------
decode_int32(<<Val:32/signed-integer, Tail/bytes>>) ->
    {Val, Tail};

decode_int32(BadArg) ->
    ?decode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-spec decode_uint32(Binary :: binary()) -> {Octet :: integer(), Tail :: binary()} | decode_error().
%%--------------------------------------------------------------------
%% Decode int32 data type.
%%--------------------------------------------------------------------
decode_uint32(<<Val:32, Tail/bytes>>) ->
    {Val, Tail};

decode_uint32(BadArg) ->
    ?decode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec decode_string(Binary :: binary(), Length :: integer()) -> {Str :: string(), Tail :: binary()} | decode_error().
%%--------------------------------------------------------------------
%% Decode string data type.
%%--------------------------------------------------------------------
decode_string(Msg, Length) when erlang:size(Msg) >= Length, is_integer(Length), Length >= 0 ->
    <<Val:Length/binary, Tail/bytes>> = Msg,
    {erlang:binary_to_list(Val), Tail};

decode_string(BadArg1, BadArg2) ->
    ?decode_error({badarg, {BadArg1, BadArg2}}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec decode_version(Binary :: binary()) -> {Version :: integer(), Tail :: binary()} | decode_error().
%%--------------------------------------------------------------------
%% Decode 'Version' data type.
%%--------------------------------------------------------------------
decode_version(Version) ->
    decode_octet(Version).    
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec decode_message_class(Binary :: binary()) -> {MsgClass :: 'CommonMessageClass'(), Tail :: binary()} | decode_error().
%%--------------------------------------------------------------------
%% Decode 'CommonMessageClass' data type.
%%--------------------------------------------------------------------
decode_message_class(<<MsgClass:8, Tail/bytes>>) ->
    {MsgClass, Tail};

decode_message_class(BadArg) ->
    ?decode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec decode_message_type(Binary :: binary()) -> {MsgType :: 'CommonMessageType'(), Tail :: binary()} | decode_error().
%%--------------------------------------------------------------------
%% Decode 'CommonMessageType' data type.
%%--------------------------------------------------------------------
decode_message_type(<<MsgType:8, Tail/bytes>>) ->
    {MsgType, Tail};

decode_message_type(BadArg) ->
    ?decode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec decode_dlci(Binary :: binary()) -> {DLCI :: #'DLCI'{}, Tail :: binary()} | decode_error().
%%--------------------------------------------------------------------
%% @doc Decode 'DLCI' data type.
%%--------------------------------------------------------------------
decode_dlci(<<0:1, Spr:1, Sapi:6, 1:1, Tei:7, Tail/bytes>>) ->
    {#'DLCI'{spr = Spr, sapi = Sapi, tei = Tei}, Tail};

decode_dlci(BadArg) ->
    ?decode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec decode_status(Binary :: binary()) -> {DLCI :: #'STATUS'{}, Tail :: binary()} | decode_error().
%%--------------------------------------------------------------------
%% @doc Decode 'STATUS' data type.
%%--------------------------------------------------------------------
decode_status(<<Type:16, Id:16, Tail/bytes>>) ->
    if
        Type =:= ?ST_AS_STATE_CHANGE andalso
        (Id =:= ?SI_AS_DOWN orelse Id =:= ?SI_AS_INACTIVE orelse Id =:= ?SI_AS_ACTIVE orelse Id =:= ?SI_AS_PENDING)
        ->
                {#'STATUS'{type = Type, id = Id}, Tail};
        Type =:= ?ST_OTHER andalso
        (Id =:= ?SI_OTHER_INSUFFICIENT_ASP_RES_ACTIVE_IN_AS orelse Id =:= ?SI_OTHER_ALT_ASP_ACTIVE)
         ->
                {#'STATUS'{type = Type, id = Id}, Tail};
        true ->
            ?decode_error({invalid_record_value, <<Type:16, Id:16>>})
    end;

decode_status(BadArg) ->
    ?decode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec decode_common_message_header(Msg :: binary()) -> {Header :: #'CommonMessageHeader'{}, Tail :: binary()} | decode_error().
%%--------------------------------------------------------------------
%% Decode common message header.
%%--------------------------------------------------------------------
decode_common_message_header(Msg) when is_binary(Msg) ->
    {Version, Msg1} = decode_version(Msg),
    {_Reserved, Msg2} = decode_octet(Msg1),
    {Class, Msg3} = decode_message_class(Msg2),
    {Type, Msg4} = decode_message_type(Msg3),
    {Length, Msg5} = decode_uint32(Msg4),
    {#'CommonMessageHeader'{version = Version, class = Class, type = Type, length = Length}, Msg5};

decode_common_message_header(BadArg) ->
    ?decode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec decode_vl_parameter(Msg :: binary()) -> {VLParameter :: #'VLParameter'{}, Tail :: binary()} | decode_error().
%%--------------------------------------------------------------------
%% Decode common message header.
%%--------------------------------------------------------------------
decode_vl_parameter(Msg) when is_binary(Msg) ->
    {Tag, Msg1} = decode_uint16(Msg),
    {Length, Msg2} = decode_uint16(Msg1),
    {VlParameter, OutMsg} = decode_vl_parameter(Tag, Length, Msg2),
    Alignment = (4 - (Length rem 4)) rem 4,
    <<_:Alignment/binary, ResultOutMsg/binary>> = OutMsg,
    {VlParameter, ResultOutMsg};

decode_vl_parameter(BadArg) ->
    ?decode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec decode_iua_message_header(Msg :: binary()) -> {Header :: #'IUAMessageHeader'{}, Tail :: binary()} | decode_error().
%%--------------------------------------------------------------------
%% @doc Decode 'IUAMessageHeader' data type.
%%--------------------------------------------------------------------
decode_iua_message_header(Msg) when is_binary(Msg) ->
    {IId, OutMsg1} = decode_vl_parameter(Msg),
    {DLCI, OutMsg2} = decode_vl_parameter(OutMsg1),

    case {IId, DLCI} of
        {#'VLParameter'{tag = ?VL_INTEGER}, #'VLParameter'{tag = ?VL_DLCI}} ->
            {#'IUAMessageHeader'{interface_identifier = IId, dlci = DLCI}, OutMsg2};
        {#'VLParameter'{tag = ?VL_TEXT}, #'VLParameter'{tag = ?VL_DLCI}} ->
            {#'IUAMessageHeader'{interface_identifier = IId, dlci = DLCI}, OutMsg2};
        _ ->
            ?decode_error({badarg, Msg})
    end;
decode_iua_message_header(BadArg) ->
    ?decode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec decode_msg(Msg :: binary()) -> {term(), Tail :: binary()} | decode_error().
%%--------------------------------------------------------------------
%% Decode 'IUA Message'.
%%--------------------------------------------------------------------
decode_msg(Msg) when is_binary(Msg) ->
    {CHeader = #'CommonMessageHeader'{}, Tail1} = decode_common_message_header(Msg),
    decode_msg_by_common_msg_header(CHeader, Tail1);

decode_msg(BadArg) ->
    ?decode_error({badarg, BadArg}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

%%%===================================================================
%%% Internal functions
%%%===================================================================

decode_vl_parameter(?VL_INTEGER, Length, Msg) when is_integer(Length) ->
    {Value, OutMsg} = decode_int32(Msg),
    {#'VLParameter'{tag = ?VL_INTEGER, length = Length, value = Value}, OutMsg};

decode_vl_parameter(?VL_TEXT, Length, Msg) when is_integer(Length) ->
    {Value, OutMsg} = decode_string(Msg, Length - 2 - 2),
    {#'VLParameter'{tag = ?VL_TEXT, length = Length, value = Value}, OutMsg};

decode_vl_parameter(?VL_INFO_STRING, Length, Msg) when is_integer(Length) ->
    if
        Length =< 255 ->
            {Value, OutMsg} = decode_string(Msg, Length - 2 - 2),
            {#'VLParameter'{tag = ?VL_INFO_STRING, length = Length, value = Value}, OutMsg};
        true ->
            ?decode_error({invalid_length, Length})
    end;

decode_vl_parameter(?VL_DLCI, Length, Msg) when is_integer(Length) ->
    {DLCI, OutMsg} = decode_dlci(Msg),
    {#'VLParameter'{tag = ?VL_DLCI, length = Length, value = DLCI}, OutMsg};

decode_vl_parameter(?VL_DIAGNOSTIC_INFO, Length, Msg) when is_integer(Length), size(Msg) >= Length - 4 ->
    DILingth = Length - 4,
    <<DI:DILingth/binary, Tail/bytes>> = Msg,
    {#'VLParameter'{tag = ?VL_DIAGNOSTIC_INFO, length = Length, value = DI}, Tail};

decode_vl_parameter(?VL_INTEGER_RANGE, Length, Msg) when is_integer(Length) ->
    RangeCount = (Length - 4) div 4,
    Range = lists:seq(1, RangeCount),
    {Values, OutMsgs} = lists:foldl(fun(_, {Result, InMsg}) -> {Value, OutMsg} = decode_int32(InMsg), {[Value | Result], OutMsg} end, {[], Msg}, Range),
    {#'VLParameter'{tag = ?VL_INTEGER_RANGE, length = Length, value = lists:reverse(Values)}, OutMsgs};

decode_vl_parameter(?VL_HEARTBEAT, Length, Msg) when is_integer(Length), size(Msg) >= Length - 4 ->
    HBLingth = Length - 4,
    <<HB:HBLingth/binary, Tail/bytes>> = Msg,
    {#'VLParameter'{tag = ?VL_HEARTBEAT, length = Length, value = HB}, Tail};

decode_vl_parameter(?VL_ASPDN_REASON, Length, Msg) when is_integer(Length) ->
    {Value, OutMsg} = decode_int32(Msg),
    {#'VLParameter'{tag = ?VL_ASPDN_REASON, length = Length, value = Value}, OutMsg};

decode_vl_parameter(?VL_TRAFFIC_MODE, Length, Msg) when is_integer(Length) ->
    {Value, OutMsg} = decode_int32(Msg),
    {#'VLParameter'{tag = ?VL_TRAFFIC_MODE, length = Length, value = Value}, OutMsg};

decode_vl_parameter(?VL_ERROR_CODE, Length, Msg) when is_integer(Length) ->
    {Value, OutMsg} = decode_int32(Msg),
    {#'VLParameter'{tag = ?VL_ERROR_CODE, length = Length, value = Value}, OutMsg};

decode_vl_parameter(?VL_STATUS, Length, Msg) when is_integer(Length) ->
    {Value, OutMsg} = decode_status(Msg),
    {#'VLParameter'{tag = ?VL_STATUS, length = Length, value = Value}, OutMsg};

decode_vl_parameter(?VL_PROTOCOL_DATA, Length, Msg) when is_integer(Length), size(Msg) >= Length - 4 ->
    DILingth = Length - 4,
    <<DI:DILingth/binary, Tail/bytes>> = Msg,
    {#'VLParameter'{tag = ?VL_PROTOCOL_DATA, length = Length, value = DI}, Tail};

decode_vl_parameter(?VL_RELEASE_REASON, Length, Msg) when is_integer(Length) ->
    {Value, OutMsg} = decode_int32(Msg),
    if
        Value =:= ?RELEASE_MGMT orelse Value =:= ?RELEASE_PHYS orelse Value =:= ?RELEASE_DM orelse Value =:= ?RELEASE_OTHER ->
            {#'VLParameter'{tag = ?VL_RELEASE_REASON, length = Length, value = Value}, OutMsg};
        true ->
            ?decode_error({invalid_param, {?VL_RELEASE_REASON, Value}})
    end;

decode_vl_parameter(?VL_TEI_STATUS, Length, Msg) when is_integer(Length) ->
    {Value, OutMsg} = decode_int32(Msg),
    if
        Value =:= ?TEI_STATUS_ASSIGNED orelse Value =:= ?TEI_STATUS_UNASSIGNED ->
            {#'VLParameter'{tag = ?VL_TEI_STATUS, length = Length, value = Value}, OutMsg};
        true ->
            ?decode_error({invalid_param, {?VL_TEI_STATUS, Value}})
    end;

decode_vl_parameter(Tag, Length, Msg) ->
    ?decode_error({badarg, {Tag, Length, Msg}}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec decode_msg_by_common_msg_header(Header :: #'CommonMessageHeader'{}, Msg :: binary()) -> {term(), binary()} | decode_error().
%%--------------------------------------------------------------------
%% Decode msg by information, get from the 'CommonMessageHeader'.
%%--------------------------------------------------------------------
decode_msg_by_common_msg_header(#'CommonMessageHeader'{class = ?CMC_QPTM, type = Type, length = Length} = CHeader, Msg)
  when is_binary(Msg),
       size(Msg) >= Length - ?COMMON_MSG_HEADER_SIZE,
       (Type =:= ?CMT_QPTM_EstablishRequest orelse
        Type =:= ?CMT_QPTM_EstablishConfirm orelse
        Type =:= ?CMT_QPTM_EstablishIndication) ->
    IUAMsgLength = Length - ?COMMON_MSG_HEADER_SIZE,
    <<IUAMsg:IUAMsgLength/binary, Tail/binary>> = Msg,
    IHeader =
        case decode_iua_message_header(IUAMsg) of
            {_IHeader, <<>>} ->
                _IHeader;
            _ ->
                ?decode_error({invalid_length, IUAMsg})
        end,
    Result = #'EstablishMsg'{common_header = CHeader, iua_header = IHeader},
    {Result, Tail};

decode_msg_by_common_msg_header(#'CommonMessageHeader'{class = ?CMC_QPTM, type = Type, length = Length} = CHeader, Msg)
  when is_binary(Msg),
       size(Msg) >= Length - ?COMMON_MSG_HEADER_SIZE,
(Type =:= ?CMT_QPTM_ReleaseRequest orelse
        Type =:= ?CMT_QPTM_ReleaseConfirm orelse
        Type =:= ?CMT_QPTM_ReleaseIndication) ->
    MsgsLength = Length - ?COMMON_MSG_HEADER_SIZE,
    <<Msgs:MsgsLength/binary, Tail/binary>> = Msg,
    {IHeader, ReasonMsgBin} = decode_iua_message_header(Msgs),
    {Reason, RestBinary} =
        if Type =:= ?CMT_QPTM_ReleaseConfirm ->
                {undefined, ReasonMsgBin};
           true ->
                {_Reason, _RestBinary} = decode_vl_parameter(ReasonMsgBin),
                case _Reason of
                    #'VLParameter'{tag = ?VL_RELEASE_REASON, value = Value} ->
                        if
                            Type =:= ?CMT_QPTM_ReleaseRequest andalso
                            Value /= ?RELEASE_MGMT andalso
                            Value /= ?RELEASE_DM andalso
                            Value /= ?RELEASE_OTHER ->
                                ?decode_error({invalid_reason_to_type, {{type, Type}, {reason, Value}}});
                            true ->
                                ok
                        end;
                    _ ->
                        ?decode_error({invalid_param, _Reason})
                end,
                {_Reason, _RestBinary}
        end,
    if
        RestBinary /= <<>> ->
            ?decode_error({invalid_length, CHeader});
        true ->
            ok
    end,
    Result = #'ReleaseMsg'{common_header = CHeader, iua_header = IHeader, reason = Reason},
    {Result, Tail};

decode_msg_by_common_msg_header(#'CommonMessageHeader'{class = ?CMC_QPTM, type = Type, length = Length} = CHeader, Msg)
  when is_binary(Msg),
       size(Msg) >= Length - ?COMMON_MSG_HEADER_SIZE,
       (Type =:= ?CMT_QPTM_DataRequest orelse
       Type =:= ?CMT_QPTM_DataIndication) ->
    MsgsLength = Length - ?COMMON_MSG_HEADER_SIZE,
    <<Msgs:MsgsLength/binary, Tail/binary>> = Msg,
    {IHeader, ReasonMsgBin} = decode_iua_message_header(Msgs),
    {Data, RestBinary} = decode_vl_parameter(ReasonMsgBin),
    case Data of
        #'VLParameter'{tag = ?VL_PROTOCOL_DATA} ->
            ok;
        _ ->
            ?decode_error({invalid_data_tag, Data})
    end,
    if
        RestBinary /= <<>> ->
            ?decode_error({invalid_length, CHeader});
        true ->
            ok
    end,
    Result = #'DataMsg'{common_header = CHeader, iua_header = IHeader, data = Data},
    {Result, Tail};

decode_msg_by_common_msg_header(#'CommonMessageHeader'{class = ?CMC_QPTM, type = Type, length = Length} = CHeader, Msg)
  when is_binary(Msg),
       size(Msg) >= Length - ?COMMON_MSG_HEADER_SIZE,
       (Type =:= ?CMT_QPTM_UnitDataRequest orelse
       Type =:= ?CMT_QPTM_UnitDataIndication) ->
    MsgsLength = Length - ?COMMON_MSG_HEADER_SIZE,
    <<Msgs:MsgsLength/binary, Tail/binary>> = Msg,
    {IHeader, ReasonMsgBin} = decode_iua_message_header(Msgs),
    {Data, RestBinary} = decode_vl_parameter(ReasonMsgBin),
    case Data of
        #'VLParameter'{tag = ?VL_PROTOCOL_DATA} ->
            ok;
        _ ->
            ?decode_error({invalid_data_tag, Data})
    end,
    if
        RestBinary /= <<>> ->
            ?decode_error({invalid_length, CHeader});
        true ->
            ok
    end,
    Result = #'UnitDataMsg'{common_header = CHeader, iua_header = IHeader, data = Data},
    {Result, Tail};

% decode_msg_by_common_msg_header(H = #'CommonMessageHeader'{type = T}, Msg) when is_binary(Msg) ->
%     {ok, {H, T, ?CMT_QPTM_EstablishRequest, ?CMT_QPTM_EstablishConfirm}};

decode_msg_by_common_msg_header(Header, Msg) ->
    ?decode_error({badarg, {Header, Msg}}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
