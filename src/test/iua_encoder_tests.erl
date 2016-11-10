%%%-------------------------------------------------------------------
%%% @author Anton N Ryabkov <anton.ryabkov@gmail.com>
%%% @copyright (C) 2011, Eltex
%%% @doc
%%%
%%% @end
%%% Created :  5 Apr 2011 by Anton N Ryabkov <anton.ryabkov@gmail.com>
%%%-------------------------------------------------------------------
-module(iua_encoder_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../include/iua.hrl").
-include("../../include/iua_messages.hrl").

-ifdef(EUNIT).

encode_octet_test() ->
    [
     ?assertEqual(<<0>>, iua_encoder:encode_octet(0)),
     ?assertEqual(<<1>>, iua_encoder:encode_octet(1)),
     ?assertEqual(<<255>>, iua_encoder:encode_octet(255)),
     ?assertEqual(<<175>>, iua_encoder:encode_octet(175)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, -1}}, iua_encoder:encode_octet(-1)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, 256}}, iua_encoder:encode_octet(256)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, 127.0}}, iua_encoder:encode_octet(127.0))
    ].

encode_int16_test() ->
    [
     ?assertEqual(<<?MIN_INT16:16/signed-integer>>, iua_encoder:encode_int16(?MIN_INT16)),
     ?assertEqual(<<0:16/signed-integer>>, iua_encoder:encode_int16(0)),
     ?assertEqual(<<?MAX_INT16:16/signed-integer>>, iua_encoder:encode_int16(?MAX_INT16)),
     ?assertEqual(<<13435:16/signed-integer>>, iua_encoder:encode_int16(13435)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, ?MIN_INT16 - 1}}, iua_encoder:encode_int16(?MIN_INT16 - 1)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, ?MAX_INT16 + 1}}, iua_encoder:encode_int16(?MAX_INT16 + 1)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, 127.0}}, iua_encoder:encode_int16(127.0))
    ].

encode_uint16_test() ->
    [
     ?assertEqual(<<0:16>>, iua_encoder:encode_uint16(0)),
     ?assertEqual(<<?MAX_UINT16:16>>, iua_encoder:encode_uint16(?MAX_UINT16)),
     ?assertEqual(<<1343:16>>, iua_encoder:encode_uint16(1343)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, -1}}, iua_encoder:encode_uint16(-1)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, ?MAX_UINT16 + 1}}, iua_encoder:encode_uint16(?MAX_UINT16 + 1)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, 127.0}}, iua_encoder:encode_uint16(127.0))
    ].

encode_int32_test() ->
    [
     ?assertEqual(<<?MIN_INT32:32/signed-integer>>, iua_encoder:encode_int32(?MIN_INT32)),
     ?assertEqual(<<0:32/signed-integer>>, iua_encoder:encode_int32(0)),
     ?assertEqual(<<?MAX_INT32:32/signed-integer>>, iua_encoder:encode_int32(?MAX_INT32)),
     ?assertEqual(<<1343475:32/signed-integer>>, iua_encoder:encode_int32(1343475)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, ?MIN_INT32 - 1}}, iua_encoder:encode_int32(?MIN_INT32 - 1)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, ?MAX_INT32 + 1}}, iua_encoder:encode_int32(?MAX_INT32 + 1)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, 127.0}}, iua_encoder:encode_int32(127.0))
    ].

encode_uint32_test() ->
    [
     ?assertEqual(<<0:32>>, iua_encoder:encode_uint32(0)),
     ?assertEqual(<<?MAX_UINT32:32>>, iua_encoder:encode_uint32(?MAX_UINT32)),
     ?assertEqual(<<1343475:32>>, iua_encoder:encode_uint32(1343475)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, -1}}, iua_encoder:encode_uint32(-1)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, ?MAX_UINT32 + 1}}, iua_encoder:encode_uint32(?MAX_UINT32 + 1)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, 127.0}}, iua_encoder:encode_uint32(127.0))
    ].

encode_string_test() ->
    [
     ?assertEqual(<<>>, iua_encoder:encode_string("")),
     ?assertEqual(<<"Test">>, iua_encoder:encode_string("Test")),
     ?assertEqual(<<"Hello World!">>, iua_encoder:encode_string("Hello World!")),
     ?assertEqual(<<"1234567890-=qwertyuiop[]asdfghjkl;'\<zxcvbnm,./">>,
                  iua_encoder:encode_string("1234567890-=qwertyuiop[]asdfghjkl;'\<zxcvbnm,./"))
    ].

encode_version_test() ->
    [
     ?assertEqual(<<0>>, iua_encoder:encode_version(0)),
     ?assertEqual(<<1>>, iua_encoder:encode_version(1)),
     ?assertEqual(<<255>>, iua_encoder:encode_version(255)),
     ?assertEqual(<<175>>, iua_encoder:encode_version(175)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, -1}}, iua_encoder:encode_version(-1)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, 256}}, iua_encoder:encode_version(256)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, 127.0}}, iua_encoder:encode_version(127.0))
    ].

encode_message_class_test() ->
    [
     ?assertEqual(<<?CMC_MGMT>>, iua_encoder:encode_message_class(?CMC_MGMT)),
     ?assertEqual(<<?CMC_T>>, iua_encoder:encode_message_class(?CMC_T)),
     ?assertEqual(<<?CMC_CO>>, iua_encoder:encode_message_class(?CMC_CO)),
     ?assertEqual(<<255>>, iua_encoder:encode_message_class(255)),
     ?assertEqual(<<175>>, iua_encoder:encode_message_class(175)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, -1}}, iua_encoder:encode_message_class(-1)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, 256}}, iua_encoder:encode_message_class(256)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, 127.0}}, iua_encoder:encode_message_class(127.0))
    ].

encode_dlci_test() ->
    Val1 = #'DLCI'{spr = 0, sapi = 0, tei = 0},
    Val2 = #'DLCI'{spr = 1, sapi = 63, tei = 127},
    Val3 = #'DLCI'{spr = 1, sapi = 17, tei = 78},
    Val4 = #'DLCI'{spr = 2, sapi = 17, tei = 78},
    Val5 = #'DLCI'{spr = 1, sapi = 64, tei = 78},
    Val6 = #'DLCI'{spr = 1, sapi = 17, tei = 128},
    [
     ?assertEqual(<<0:8, 1:1, 0:7>>, iua_encoder:encode_dlci(Val1)),
     ?assertEqual(<<0:1, 1:1, 63:6, 1:1, 127:7>>, iua_encoder:encode_dlci(Val2)),
     ?assertEqual(<<0:1, 1:1, 17:6, 1:1, 78:7>>, iua_encoder:encode_dlci(Val3)),
     ?assertThrow({encode_error, iua_encoder, _, {invalid_record_value, Val4}}, iua_encoder:encode_dlci(Val4)),
     ?assertThrow({encode_error, iua_encoder, _, {invalid_record_value, Val5}}, iua_encoder:encode_dlci(Val5)),
     ?assertThrow({encode_error, iua_encoder, _, {invalid_record_value, Val6}}, iua_encoder:encode_dlci(Val6)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, no_dlci}}, iua_encoder:encode_dlci(no_dlci))
    ].

encode_status_test() ->
    Val1 = #'STATUS'{type = ?ST_AS_STATE_CHANGE, id = ?SI_AS_DOWN},
    Val2 = #'STATUS'{type = ?ST_AS_STATE_CHANGE, id = ?SI_AS_INACTIVE},
    Val3 = #'STATUS'{type = ?ST_AS_STATE_CHANGE, id = ?SI_AS_ACTIVE},
    Val4 = #'STATUS'{type = ?ST_AS_STATE_CHANGE, id = ?SI_AS_PENDING},
    Val5 = #'STATUS'{type = ?ST_AS_STATE_CHANGE, id = 127},
    Val6 = #'STATUS'{type = ?ST_OTHER, id = ?SI_OTHER_INSUFFICIENT_ASP_RES_ACTIVE_IN_AS},
    Val7 = #'STATUS'{type = ?ST_OTHER, id = ?SI_OTHER_ALT_ASP_ACTIVE},
    Val8 = #'STATUS'{type = ?ST_OTHER, id = 15},
    Val9 = #'STATUS'{type = 3, id = 3},
    [
     ?assertEqual(<<?ST_AS_STATE_CHANGE:16, ?SI_AS_DOWN:16>>, iua_encoder:encode_status(Val1)),
     ?assertEqual(<<?ST_AS_STATE_CHANGE:16, ?SI_AS_INACTIVE:16>>, iua_encoder:encode_status(Val2)),
     ?assertEqual(<<?ST_AS_STATE_CHANGE:16, ?SI_AS_ACTIVE:16>>, iua_encoder:encode_status(Val3)),
     ?assertEqual(<<?ST_AS_STATE_CHANGE:16, ?SI_AS_PENDING:16>>, iua_encoder:encode_status(Val4)),
     ?assertThrow({encode_error, iua_encoder, _, {invalid_record_value, Val5}}, iua_encoder:encode_status(Val5)),
     ?assertEqual(<<?ST_OTHER:16, ?SI_OTHER_INSUFFICIENT_ASP_RES_ACTIVE_IN_AS:16>>, iua_encoder:encode_status(Val6)),
     ?assertEqual(<<?ST_OTHER:16, ?SI_OTHER_ALT_ASP_ACTIVE:16>>, iua_encoder:encode_status(Val7)),     
     ?assertThrow({encode_error, iua_encoder, _, {invalid_record_value, Val8}}, iua_encoder:encode_status(Val8)),
     ?assertThrow({encode_error, iua_encoder, _, {invalid_record_value, Val9}}, iua_encoder:encode_status(Val9))
    ].

encode_message_type_test() ->
    [
     ?assertEqual(<<?CMT_MGMT_ERR>>, iua_encoder:encode_message_type(?CMT_MGMT_ERR)),
     ?assertEqual(<<?CMT_ASPTM_ACTIVE>>, iua_encoder:encode_message_type(?CMT_ASPTM_ACTIVE)),
     ?assertEqual(<<?CMT_ASPSM_BEAT>>, iua_encoder:encode_message_type(?CMT_ASPSM_BEAT)),
     ?assertEqual(<<?CMT_QPTM_ReleaseIndication>>, iua_encoder:encode_message_type(?CMT_QPTM_ReleaseIndication)),
     ?assertEqual(<<255>>, iua_encoder:encode_message_type(255)),
     ?assertEqual(<<175>>, iua_encoder:encode_message_type(175)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, -1}}, iua_encoder:encode_message_type(-1)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, 256}}, iua_encoder:encode_message_type(256)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, 127.0}}, iua_encoder:encode_message_type(127.0))
    ].

encode_common_message_header_test() ->
    Val1 = #'CommonMessageHeader'{class = ?CMC_MGMT, type = ?CMT_MGMT_TEI_Request, length = 15},
    Val2 = #'CommonMessageHeader'{version = 2, class = ?CMC_QPTM, type = ?CMT_QPTM_Reserved, length = 15},
    Val3 = #'CommonMessageHeader'{class = ?CMC_CL, type = 182, length = 15},
    Val4 = #'CommonMessageHeader'{class = 18, type = 1, length = 17},
    Val5 = #'CommonMessageHeader'{version = 5, class = 172, type = 208, length = 1024},
    Val6 = #'CommonMessageHeader'{version = 532, class = 172, type = 208, length = 1024},
    Val7 = #'CommonMessageHeader'{version = 1, class = 1000, type = 208, length = 1024},
    Val8 = #'CommonMessageHeader'{version = 1, class = 172, type = 987, length = 1024},
    Val9 = #'CommonMessageHeader'{version = 1, class = 172, type = 208, length = -1},
    Val10= #'CommonMessageHeader'{version = 1, class = 172, type = 208, length = 12345689098765},
    [
     ?assertEqual(<<1,0,0,2,0,0,0,15>>,    iua_encoder:encode_common_message_header(Val1)),
     ?assertEqual(<<2,0,5,0,0,0,0,15>>,    iua_encoder:encode_common_message_header(Val2)),
     ?assertEqual(<<1,0,7,182,0,0,0,15>>,  iua_encoder:encode_common_message_header(Val3)),
     ?assertEqual(<<1,0,18,1,0,0,0,17>>,   iua_encoder:encode_common_message_header(Val4)),
     ?assertEqual(<<5,0,172,208,0,0,4,0>>, iua_encoder:encode_common_message_header(Val5)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, Val6}}, iua_encoder:encode_message_type(Val6)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, Val7}}, iua_encoder:encode_message_type(Val7)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, Val8}}, iua_encoder:encode_message_type(Val8)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, Val9}}, iua_encoder:encode_message_type(Val9)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, Val10}}, iua_encoder:encode_message_type(Val10))
    ].

encode_vl_parameter_test() ->
    Val1 = #'VLParameter'{tag = ?VL_INTEGER, value = 123},
    Val2 = #'VLParameter'{tag = ?VL_INTEGER, value = 968},
    Val3 = #'VLParameter'{tag = ?VL_TEXT, value = ""},
    Val4 = #'VLParameter'{tag = ?VL_TEXT, value = "Hello World!!!"},
    Val5 = #'VLParameter'{tag = ?VL_INFO_STRING, value = ""},
    Val6 = #'VLParameter'{tag = ?VL_INFO_STRING, value = "Hello World!!!"},
    Val7 = #'VLParameter'{tag = ?VL_INFO_STRING, value = multiplier("1234567890", 26)},
    Val8 = #'VLParameter'{tag = ?VL_DLCI, value = #'DLCI'{spr = 0, sapi = 0, tei = 0}},
    Val9 = #'VLParameter'{tag = ?VL_DLCI, value = #'DLCI'{spr = 1, sapi = 63, tei = 127}},
    Val10 = #'VLParameter'{tag = ?VL_DLCI, value = #'DLCI'{spr = 1, sapi = 17, tei = 78}},
    Val11 = #'VLParameter'{tag = ?VL_DLCI, value = #'DLCI'{spr = 2, sapi = 17, tei = 78}},
    Val12 = #'VLParameter'{tag = ?VL_DIAGNOSTIC_INFO, value = <<1, 2, 3, 4, 5>>},
    Val13 = #'VLParameter'{tag = ?VL_DIAGNOSTIC_INFO, value = <<>>},
    Val14 = #'VLParameter'{tag = ?VL_DIAGNOSTIC_INFO, value = no_binary},
    Val15 = #'VLParameter'{tag = ?VL_INTEGER_RANGE, value = []},
    Val16 = #'VLParameter'{tag = ?VL_INTEGER_RANGE, value = [1, 2, 3, 345, 123734]},
    Val17 = #'VLParameter'{tag = ?VL_INTEGER_RANGE, value = no_binary},
    Val18 = #'VLParameter'{tag = ?VL_HEARTBEAT, value = <<1, 2, 3, 4, 5>>},
    Val19 = #'VLParameter'{tag = ?VL_HEARTBEAT, value = <<>>},
    Val20 = #'VLParameter'{tag = ?VL_HEARTBEAT, value = no_binary},
    Val21 = #'VLParameter'{tag = ?VL_ASPDN_REASON, value = ?ASPDN_MGMT_INHIBIT_REASON},
    Val22 = #'VLParameter'{tag = ?VL_ASPDN_REASON, value = 9},
    Val23 = #'VLParameter'{tag = ?VL_ASPDN_REASON, value = no_integer},
    Val24 = #'VLParameter'{tag = ?VL_TRAFFIC_MODE, value = ?OVER_RIDE_TRAFIC_MODE},
    Val25 = #'VLParameter'{tag = ?VL_TRAFFIC_MODE, value = ?LOAD_SHARE_TRAFIC_MODE},
    Val26 = #'VLParameter'{tag = ?VL_TRAFFIC_MODE, value = 9},
    Val27 = #'VLParameter'{tag = ?VL_TRAFFIC_MODE, value = no_integer},    
    Val28 = #'VLParameter'{tag = ?VL_ERROR_CODE, value = ?InvalidVersion},
    Val29 = #'VLParameter'{tag = ?VL_ERROR_CODE, value = 9},
    Val30 = #'VLParameter'{tag = ?VL_ERROR_CODE, value = no_integer},
    Val31 = #'VLParameter'{tag = ?VL_STATUS, length = 8, value = #'STATUS'{type = ?ST_AS_STATE_CHANGE, id = ?SI_AS_DOWN}},
    Val32 = #'VLParameter'{tag = ?VL_STATUS, length = 8, value = #'STATUS'{type = ?ST_OTHER, id = ?SI_OTHER_ALT_ASP_ACTIVE}},
    Val33 = #'VLParameter'{tag = ?VL_STATUS, value = no_status},
    Val34 = #'VLParameter'{tag = ?VL_PROTOCOL_DATA, value = <<1, 2, 3, 4, 5>>},
    Val35 = #'VLParameter'{tag = ?VL_PROTOCOL_DATA, value = <<>>},
    Val36 = #'VLParameter'{tag = ?VL_RELEASE_REASON, length = 8, value = ?RELEASE_MGMT},
    Val37 = #'VLParameter'{tag = ?VL_RELEASE_REASON, length = 8, value = ?RELEASE_OTHER},
    Val38 = #'VLParameter'{tag = ?VL_RELEASE_REASON, length = 8, value = 192},
    Val39 = #'VLParameter'{tag = ?VL_TEI_STATUS, length = 8, value = ?TEI_STATUS_ASSIGNED},
    Val40 = #'VLParameter'{tag = ?VL_TEI_STATUS, length = 8, value = ?TEI_STATUS_UNASSIGNED},
    Val41 = #'VLParameter'{tag = ?VL_TEI_STATUS, length = 8, value = 192},
    
    [
     ?assertEqual(<<?VL_INTEGER:16, 8:16, 123:32>>, iua_encoder:encode_vl_parameter(Val1)),
     ?assertEqual(<<?VL_INTEGER:16, 8:16, 968:32>>, iua_encoder:encode_vl_parameter(Val2)),
     ?assertEqual(<<?VL_TEXT:16, 4:16>>, iua_encoder:encode_vl_parameter(Val3)),
     ?assertEqual(<<?VL_TEXT:16, 18:16, <<72,101,108,108,111,32,87,111,114,108,100,33,33,33,0,0>>/binary>>,
                  iua_encoder:encode_vl_parameter(Val4)),
     ?assertEqual(<<?VL_INFO_STRING:16, 4:16>>, iua_encoder:encode_vl_parameter(Val5)),
     ?assertEqual(<<?VL_INFO_STRING:16, 18:16, <<72,101,108,108,111,32,87,111,114,108,100,33,33,33,0,0>>/binary>>,
                  iua_encoder:encode_vl_parameter(Val6)),
     ?assertThrow({encode_error, iua_encoder, _, {invalid_length, 260}}, iua_encoder:encode_vl_parameter(Val7)),
     ?assertEqual(<<?VL_DLCI:16, 6:16, <<0:8, 1:1, 0:7>>/binary, 0,0>>, iua_encoder:encode_vl_parameter(Val8)),
     ?assertEqual(<<?VL_DLCI:16, 6:16, <<0:1, 1:1, 63:6, 1:1, 127:7>>/binary, 0,0>>, iua_encoder:encode_vl_parameter(Val9)),
     ?assertEqual(<<?VL_DLCI:16, 6:16, <<0:1, 1:1, 17:6, 1:1, 78:7>>/binary, 0,0>>, iua_encoder:encode_vl_parameter(Val10)),
     ?assertThrow({encode_error, iua_encoder, _, {invalid_record_value, #'DLCI'{spr = 2, sapi = 17, tei = 78}}}, iua_encoder:encode_vl_parameter(Val11)),
     ?assertEqual(<<?VL_DIAGNOSTIC_INFO:16, 9:16, <<1, 2, 3, 4, 5>>/binary, 0,0,0>>, iua_encoder:encode_vl_parameter(Val12)),
     ?assertEqual(<<?VL_DIAGNOSTIC_INFO:16, 4:16, <<>>/binary>>, iua_encoder:encode_vl_parameter(Val13)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, Val14}}, iua_encoder:encode_vl_parameter(Val14)),
     ?assertEqual(<<?VL_INTEGER_RANGE:16, 4:16, <<>>/binary>>, iua_encoder:encode_vl_parameter(Val15)),
     ?assertEqual(<<?VL_INTEGER_RANGE:16, 24:16, 1:32, 2:32, 3:32, 345:32, 123734:32>>, iua_encoder:encode_vl_parameter(Val16)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, Val17}}, iua_encoder:encode_vl_parameter(Val17)),
     ?assertEqual(<<?VL_HEARTBEAT:16, 9:16, <<1, 2, 3, 4, 5>>/binary, 0,0,0>>, iua_encoder:encode_vl_parameter(Val18)),
     ?assertEqual(<<?VL_HEARTBEAT:16, 4:16, <<>>/binary>>, iua_encoder:encode_vl_parameter(Val19)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, Val20}}, iua_encoder:encode_vl_parameter(Val20)),
     ?assertEqual(<<?VL_ASPDN_REASON:16, 8:16, ?ASPDN_MGMT_INHIBIT_REASON:32>>, iua_encoder:encode_vl_parameter(Val21)),
     ?assertEqual(<<?VL_ASPDN_REASON:16, 8:16, 9:32>>, iua_encoder:encode_vl_parameter(Val22)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, Val23}}, iua_encoder:encode_vl_parameter(Val23)),
     ?assertEqual(<<?VL_TRAFFIC_MODE:16, 8:16, ?OVER_RIDE_TRAFIC_MODE:32>>, iua_encoder:encode_vl_parameter(Val24)),
     ?assertEqual(<<?VL_TRAFFIC_MODE:16, 8:16, ?LOAD_SHARE_TRAFIC_MODE:32>>, iua_encoder:encode_vl_parameter(Val25)),
     ?assertEqual(<<?VL_TRAFFIC_MODE:16, 8:16, 9:32>>, iua_encoder:encode_vl_parameter(Val26)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, Val27}}, iua_encoder:encode_vl_parameter(Val27)),
     ?assertEqual(<<?VL_ERROR_CODE:16, 8:16, ?InvalidVersion:32>>, iua_encoder:encode_vl_parameter(Val28)),
     ?assertEqual(<<?VL_ERROR_CODE:16, 8:16, 9:32>>, iua_encoder:encode_vl_parameter(Val29)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, Val30}}, iua_encoder:encode_vl_parameter(Val30)),
     ?assertEqual(<<?VL_STATUS:16, 8:16, ?ST_AS_STATE_CHANGE:16, ?SI_AS_DOWN:16>>, iua_encoder:encode_vl_parameter(Val31)),
     ?assertEqual(<<?VL_STATUS:16, 8:16, ?ST_OTHER:16, ?SI_OTHER_ALT_ASP_ACTIVE:16>>, iua_encoder:encode_vl_parameter(Val32)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, Val33}}, iua_encoder:encode_vl_parameter(Val33)),
     ?assertEqual(<<?VL_PROTOCOL_DATA:16, 9:16, <<1, 2, 3, 4, 5>>/binary, 0,0,0>>, iua_encoder:encode_vl_parameter(Val34)),
     ?assertEqual(<<?VL_PROTOCOL_DATA:16, 4:16, <<>>/binary>>, iua_encoder:encode_vl_parameter(Val35)),
     ?assertEqual(<<?VL_RELEASE_REASON:16, 8:16, ?RELEASE_MGMT:32>>, iua_encoder:encode_vl_parameter(Val36)),
     ?assertEqual(<<?VL_RELEASE_REASON:16, 8:16, ?RELEASE_OTHER:32>>, iua_encoder:encode_vl_parameter(Val37)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, Val38}}, iua_encoder:encode_vl_parameter(Val38)),
     ?assertEqual(<<?VL_TEI_STATUS:16, 8:16, ?TEI_STATUS_ASSIGNED:32>>, iua_encoder:encode_vl_parameter(Val39)),
     ?assertEqual(<<?VL_TEI_STATUS:16, 8:16, ?TEI_STATUS_UNASSIGNED:32>>, iua_encoder:encode_vl_parameter(Val40)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, Val41}}, iua_encoder:encode_vl_parameter(Val41))
    ].

encode_iua_message_header_test() ->
    Val1 = #'IUAMessageHeader'{interface_identifier = #'VLParameter'{tag = ?VL_INTEGER, value = 123},
                               dlci = #'VLParameter'{tag = ?VL_DLCI, value = #'DLCI'{spr = 1, sapi = 17, tei = 78}}},
    Val2 = #'IUAMessageHeader'{interface_identifier = #'VLParameter'{tag = ?VL_TEXT, value = "test interface"},
                               dlci = #'VLParameter'{tag = ?VL_DLCI, value = #'DLCI'{spr = 1, sapi = 17, tei = 78}}},
    Val3 = #'IUAMessageHeader'{interface_identifier = #'VLParameter'{tag = ?VL_DIAGNOSTIC_INFO, value = <<1, 2, 3, 4, 5>>},
                               dlci = #'VLParameter'{tag = ?VL_DLCI, value = #'DLCI'{spr = 1, sapi = 17, tei = 78}}},
    Val4 = #'IUAMessageHeader'{interface_identifier = #'VLParameter'{tag = ?VL_TEXT, value = "test interface"},
                               dlci = #'VLParameter'{tag = ?VL_PROTOCOL_DATA, value = <<1, 2, 3, 4, 5>>}},
    [
     ?assertEqual(<<?VL_INTEGER:16, 8:16, 123:32, <<?VL_DLCI:16, 6:16, <<0:1, 1:1, 17:6, 1:1, 78:7>>/binary, 0,0>>/binary>>,
                  iua_encoder:encode_iua_message_header(Val1)),
     ?assertEqual(<<?VL_TEXT:16, 18:16, <<116,101,115,116,32,105,110,116,101,114,102,97,99,101>>/binary, 0,0, <<?VL_DLCI:16, 6:16, <<0:1, 1:1, 17:6, 1:1, 78:7>>/binary, 0,0>>/binary>>,
                  iua_encoder:encode_iua_message_header(Val2)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, Val3}}, iua_encoder:encode_iua_message_header(Val3)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, Val4}}, iua_encoder:encode_iua_message_header(Val4))
    ].

encode_establish_msg_test() ->
    %, length = 15
    CHeader1 = #'CommonMessageHeader'{class = ?CMC_QPTM, type = ?CMT_QPTM_EstablishRequest},
    CHeader2 = #'CommonMessageHeader'{class = ?CMC_QPTM, type = ?CMT_QPTM_EstablishConfirm},
    CHeader3 = #'CommonMessageHeader'{class = ?CMC_QPTM, type = ?CMT_QPTM_EstablishIndication},
    InvalidCHeader = #'CommonMessageHeader'{class = ?CMC_QPTM, type = ?CMT_QPTM_Reserved},
    IHeader = #'IUAMessageHeader'{interface_identifier = #'VLParameter'{tag = ?VL_INTEGER, value = 123},
                                  dlci = #'VLParameter'{tag = ?VL_DLCI, value = #'DLCI'{spr = 1, sapi = 17, tei = 78}}},
    IHeaderBin = <<?VL_INTEGER:16, 8:16, 123:32, <<?VL_DLCI:16, 6:16, <<0:1, 1:1, 17:6, 1:1, 78:7>>/binary, 0,0>>/binary>>,
    Val1 = #'EstablishMsg'{common_header = CHeader1, iua_header = IHeader},
    Val2 = #'EstablishMsg'{common_header = CHeader2, iua_header = IHeader},
    Val3 = #'EstablishMsg'{common_header = CHeader3, iua_header = IHeader},
    Val4 = #'EstablishMsg'{common_header = InvalidCHeader, iua_header = IHeader},
    [
     ?assertEqual(<<1,0,?CMC_QPTM,5,0,0,0,24, IHeaderBin/binary>>, iua_encoder:encode_establish_msg(Val1)),
     ?assertEqual(<<1,0,?CMC_QPTM,6,0,0,0,24, IHeaderBin/binary>>, iua_encoder:encode_establish_msg(Val2)),
     ?assertEqual(<<1,0,?CMC_QPTM,7,0,0,0,24, IHeaderBin/binary>>, iua_encoder:encode_establish_msg(Val3)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, InvalidCHeader}}, iua_encoder:encode_establish_msg(Val4)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, no_header}}, iua_encoder:encode_establish_msg(no_header))
    ].

encode_reason_msg_test() ->
    CHeader1 = #'CommonMessageHeader'{class = ?CMC_QPTM, type = ?CMT_QPTM_ReleaseRequest},
    CHeader2 = #'CommonMessageHeader'{class = ?CMC_QPTM, type = ?CMT_QPTM_ReleaseIndication},
    CHeader3 = #'CommonMessageHeader'{class = ?CMC_QPTM, type = ?CMT_QPTM_ReleaseConfirm},
    InvalidCHeader = #'CommonMessageHeader'{class = ?CMC_QPTM, type = ?CMT_QPTM_Reserved},
    IHeader = #'IUAMessageHeader'{interface_identifier = #'VLParameter'{tag = ?VL_INTEGER, value = 123},
                                  dlci = #'VLParameter'{tag = ?VL_DLCI, value = #'DLCI'{spr = 1, sapi = 17, tei = 78}}},
    IHeaderBin = <<?VL_INTEGER:16, 8:16, 123:32, <<?VL_DLCI:16, 6:16, <<0:1, 1:1, 17:6, 1:1, 78:7>>/binary, 0,0>>/binary>>,
    Reason1 = #'VLParameter'{tag = ?VL_RELEASE_REASON, length = 8, value = ?RELEASE_MGMT},
    BReason1 = <<?VL_RELEASE_REASON:16, 8:16, ?RELEASE_MGMT:32>>,
    Reason2 = #'VLParameter'{tag = ?VL_RELEASE_REASON, length = 8, value = ?RELEASE_DM},
    BReason2 = <<?VL_RELEASE_REASON:16, 8:16, ?RELEASE_DM:32>>,
    Reason3 = #'VLParameter'{tag = ?VL_RELEASE_REASON, length = 8, value = ?RELEASE_OTHER},
    BReason3 = <<?VL_RELEASE_REASON:16, 8:16, ?RELEASE_OTHER:32>>,
    Reason4 = #'VLParameter'{tag = ?VL_RELEASE_REASON, length = 8, value = ?RELEASE_PHYS},
    BReason4 = <<?VL_RELEASE_REASON:16, 8:16, ?RELEASE_PHYS:32>>,
    Val1 = #'ReleaseMsg'{common_header = CHeader1, iua_header = IHeader, reason = Reason1},
    Val2 = #'ReleaseMsg'{common_header = CHeader2, iua_header = IHeader, reason = Reason2},
    Val3 = #'ReleaseMsg'{common_header = CHeader3, iua_header = IHeader},
    Val4 = #'ReleaseMsg'{common_header = CHeader2, iua_header = IHeader, reason = Reason4},
    Val5 = #'ReleaseMsg'{common_header = CHeader1, iua_header = IHeader, reason = Reason3},
    ValE1 = #'ReleaseMsg'{common_header = CHeader1, iua_header = IHeader, reason = Reason4},
    ValE2 = #'ReleaseMsg'{common_header = CHeader3, iua_header = IHeader, reason = Reason1},
    ValE3 = #'ReleaseMsg'{common_header = InvalidCHeader, iua_header = IHeader, reason = Reason1},
    [
     ?assertEqual(<<1,0,?CMC_QPTM,?CMT_QPTM_ReleaseRequest,0,0,0,32, IHeaderBin/binary, BReason1/binary>>, iua_encoder:encode_release_msg(Val1)),
     ?assertEqual(<<1,0,?CMC_QPTM,?CMT_QPTM_ReleaseIndication,0,0,0,32, IHeaderBin/binary, BReason2/binary>>, iua_encoder:encode_release_msg(Val2)),
     ?assertEqual(<<1,0,?CMC_QPTM,?CMT_QPTM_ReleaseConfirm,0,0,0,24, IHeaderBin/binary>>, iua_encoder:encode_release_msg(Val3)),
     ?assertEqual(<<1,0,?CMC_QPTM,?CMT_QPTM_ReleaseIndication,0,0,0,32, IHeaderBin/binary, BReason4/binary>>, iua_encoder:encode_release_msg(Val4)),
     ?assertEqual(<<1,0,?CMC_QPTM,?CMT_QPTM_ReleaseRequest,0,0,0,32, IHeaderBin/binary, BReason3/binary>>, iua_encoder:encode_release_msg(Val5)),
     ?assertThrow({encode_error, iua_encoder, _, {invalid_reason_to_type, _}}, iua_encoder:encode_release_msg(ValE1)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, CHeader3}}, iua_encoder:encode_release_msg(ValE2)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, InvalidCHeader}}, iua_encoder:encode_release_msg(ValE3)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, no_header}}, iua_encoder:encode_release_msg(no_header))
    ].

encode_data_msg_test() ->
    CHeader1 = #'CommonMessageHeader'{class = ?CMC_QPTM, type = ?CMT_QPTM_DataRequest},
    CHeader2 = #'CommonMessageHeader'{class = ?CMC_QPTM, type = ?CMT_QPTM_DataIndication},
    CHeader3 = #'CommonMessageHeader'{class = ?CMC_QPTM, type = ?CMT_QPTM_ReleaseRequest},
    InvalidCHeader = #'CommonMessageHeader'{class = ?CMC_QPTM, type = ?CMT_QPTM_Reserved},
    IHeader = #'IUAMessageHeader'{interface_identifier = #'VLParameter'{tag = ?VL_INTEGER, value = 123},
                                  dlci = #'VLParameter'{tag = ?VL_DLCI, value = #'DLCI'{spr = 1, sapi = 17, tei = 78}}},
    IHeaderBin = <<?VL_INTEGER:16, 8:16, 123:32, <<?VL_DLCI:16, 6:16, <<0:1, 1:1, 17:6, 1:1, 78:7>>/binary, 0,0>>/binary>>,
    
    Reason1 = #'VLParameter'{tag = ?VL_PROTOCOL_DATA, length = 8, value = <<1,2,3,4>>},
    BReason1 = <<?VL_PROTOCOL_DATA:16, 8:16, 1,2,3,4>>,
    Reason2 = #'VLParameter'{tag = ?VL_PROTOCOL_DATA, length = 8, value = <<5,6,7,8>>},
    BReason2 = <<?VL_PROTOCOL_DATA:16, 8:16, 5,6,7,8>>,
    Reason3 = #'VLParameter'{tag = ?VL_PROTOCOL_DATA, length = 8, value = <<110,111,112,113>>},
    BReason3 = <<?VL_PROTOCOL_DATA:16, 8:16, 110,111,112,113>>,
    Reason4 = #'VLParameter'{tag = ?VL_PROTOCOL_DATA, length = 9, value = <<200,201,202,203,204>>},
    BReason4 = <<?VL_PROTOCOL_DATA:16, 9:16, 200,201,202,203,204, 0,0,0>>,
    Val1 = #'DataMsg'{common_header = CHeader1, iua_header = IHeader, data = Reason1},
    Val2 = #'DataMsg'{common_header = CHeader2, iua_header = IHeader, data = Reason2},
    Val4 = #'DataMsg'{common_header = CHeader2, iua_header = IHeader, data = Reason4},
    Val5 = #'DataMsg'{common_header = CHeader1, iua_header = IHeader, data = Reason3},
    ValE1 = #'DataMsg'{common_header = CHeader3, iua_header = IHeader, data = Reason4},
    ValE2 = #'DataMsg'{common_header = InvalidCHeader, iua_header = IHeader, data = Reason1},
    [
     ?assertEqual(<<1,0,?CMC_QPTM,?CMT_QPTM_DataRequest,0,0,0,32, IHeaderBin/binary, BReason1/binary>>, iua_encoder:encode_data_msg(Val1)),
     ?assertEqual(<<1,0,?CMC_QPTM,?CMT_QPTM_DataIndication,0,0,0,32, IHeaderBin/binary, BReason2/binary>>, iua_encoder:encode_data_msg(Val2)),
     ?assertEqual(<<1,0,?CMC_QPTM,?CMT_QPTM_DataIndication,0,0,0,36, IHeaderBin/binary, BReason4/binary>>, iua_encoder:encode_data_msg(Val4)),     
     ?assertEqual(<<1,0,?CMC_QPTM,?CMT_QPTM_DataRequest,0,0,0,32, IHeaderBin/binary, BReason3/binary>>, iua_encoder:encode_data_msg(Val5)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, CHeader3}}, iua_encoder:encode_data_msg(ValE1)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, InvalidCHeader}}, iua_encoder:encode_data_msg(ValE2)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, no_header}}, iua_encoder:encode_data_msg(no_header))
    ].

encode_unit_data_msg_test() ->
    CHeader1 = #'CommonMessageHeader'{class = ?CMC_QPTM, type = ?CMT_QPTM_UnitDataRequest},
    CHeader2 = #'CommonMessageHeader'{class = ?CMC_QPTM, type = ?CMT_QPTM_UnitDataIndication},
    CHeader3 = #'CommonMessageHeader'{class = ?CMC_QPTM, type = ?CMT_QPTM_ReleaseRequest},
    InvalidCHeader = #'CommonMessageHeader'{class = ?CMC_QPTM, type = ?CMT_QPTM_Reserved},
    IHeader = #'IUAMessageHeader'{interface_identifier = #'VLParameter'{tag = ?VL_INTEGER, value = 123},
                                  dlci = #'VLParameter'{tag = ?VL_DLCI, value = #'DLCI'{spr = 1, sapi = 17, tei = 78}}},
    IHeaderBin = <<?VL_INTEGER:16, 8:16, 123:32, <<?VL_DLCI:16, 6:16, <<0:1, 1:1, 17:6, 1:1, 78:7>>/binary, 0,0>>/binary>>,
    
    Reason1 = #'VLParameter'{tag = ?VL_PROTOCOL_DATA, length = 8, value = <<1,2,3,4>>},
    BReason1 = <<?VL_PROTOCOL_DATA:16, 8:16, 1,2,3,4>>,
    Reason2 = #'VLParameter'{tag = ?VL_PROTOCOL_DATA, length = 8, value = <<5,6,7,8>>},
    BReason2 = <<?VL_PROTOCOL_DATA:16, 8:16, 5,6,7,8>>,
    Reason3 = #'VLParameter'{tag = ?VL_PROTOCOL_DATA, length = 8, value = <<110,111,112,113>>},
    BReason3 = <<?VL_PROTOCOL_DATA:16, 8:16, 110,111,112,113>>,
    Reason4 = #'VLParameter'{tag = ?VL_PROTOCOL_DATA, length = 9, value = <<200,201,202,203,204>>},
    BReason4 = <<?VL_PROTOCOL_DATA:16, 9:16, 200,201,202,203,204, 0,0,0>>,
    Val1 = #'UnitDataMsg'{common_header = CHeader1, iua_header = IHeader, data = Reason1},
    Val2 = #'UnitDataMsg'{common_header = CHeader2, iua_header = IHeader, data = Reason2},
    Val4 = #'UnitDataMsg'{common_header = CHeader2, iua_header = IHeader, data = Reason4},
    Val5 = #'UnitDataMsg'{common_header = CHeader1, iua_header = IHeader, data = Reason3},
    ValE1 = #'UnitDataMsg'{common_header = CHeader3, iua_header = IHeader, data = Reason4},
    ValE2 = #'UnitDataMsg'{common_header = InvalidCHeader, iua_header = IHeader, data = Reason1},
    [
     ?assertEqual(<<1,0,?CMC_QPTM,?CMT_QPTM_UnitDataRequest,0,0,0,32, IHeaderBin/binary, BReason1/binary>>, iua_encoder:encode_unit_data_msg(Val1)),
     ?assertEqual(<<1,0,?CMC_QPTM,?CMT_QPTM_UnitDataIndication,0,0,0,32, IHeaderBin/binary, BReason2/binary>>, iua_encoder:encode_unit_data_msg(Val2)),
     ?assertEqual(<<1,0,?CMC_QPTM,?CMT_QPTM_UnitDataIndication,0,0,0,36, IHeaderBin/binary, BReason4/binary>>, iua_encoder:encode_unit_data_msg(Val4)),     
     ?assertEqual(<<1,0,?CMC_QPTM,?CMT_QPTM_UnitDataRequest,0,0,0,32, IHeaderBin/binary, BReason3/binary>>, iua_encoder:encode_unit_data_msg(Val5)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, CHeader3}}, iua_encoder:encode_unit_data_msg(ValE1)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, InvalidCHeader}}, iua_encoder:encode_unit_data_msg(ValE2)),
     ?assertThrow({encode_error, iua_encoder, _, {badarg, no_header}}, iua_encoder:encode_unit_data_msg(no_header))
    ].

multiplier(Src, Count) when is_list(Src), is_integer(Count) ->
    lists:flatten(lists:duplicate(Count, Src)).

-endif.
