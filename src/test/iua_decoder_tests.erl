%%%-------------------------------------------------------------------
%%% @author Anton N Ryabkov <anton.ryabkov@gmail.com>
%%% @copyright (C) 2011, Eltex
%%% @doc
%%%
%%% @end
%%% Created :  5 Apr 2011 by Anton N Ryabkov <anton.ryabkov@gmail.com>
%%%-------------------------------------------------------------------
-module(iua_decoder_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../include/iua.hrl").
-include("../../include/iua_messages.hrl").

-ifdef(EUNIT).

decode_octet_test() ->
    [
     ?assertEqual({0, <<1, 2, 3>>}, iua_decoder:decode_octet(<<0, 1, 2, 3>>)),
     ?assertEqual({1, <<1, 2, 3>>}, iua_decoder:decode_octet(<<1, 1, 2, 3>>)),
     ?assertEqual({15, <<>>}, iua_decoder:decode_octet(<<15>>)),
     ?assertEqual({255, <<4, 5, 6>>}, iua_decoder:decode_octet(<<255, 4, 5, 6>>)),
     ?assertEqual({175, <<7, 8, 9>>}, iua_decoder:decode_octet(<<175, 7, 8, 9>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, <<>>}}, iua_decoder:decode_octet(<<>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, not_binary}}, iua_decoder:decode_octet(not_binary))
    ].

decode_int16_test() ->
    [
     ?assertEqual({0, <<1, 2, 3>>}, iua_decoder:decode_int16(<<0, 0, 1, 2, 3>>)),
     ?assertEqual({1234, <<1, 2, 3>>}, iua_decoder:decode_int16(<<4,210, 1, 2, 3>>)),
     ?assertEqual({-1234, <<>>}, iua_decoder:decode_int16(<<251, 46>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, <<>>}}, iua_decoder:decode_int16(<<>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, <<1>>}}, iua_decoder:decode_int16(<<1>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, not_binary}}, iua_decoder:decode_int16(not_binary))
    ].

decode_uint16_test() ->
    [
     ?assertEqual({0, <<1, 2, 3>>}, iua_decoder:decode_uint16(<<0, 0, 1, 2, 3>>)),
     ?assertEqual({1234, <<1, 2, 3>>}, iua_decoder:decode_uint16(<<4,210, 1, 2, 3>>)),
     ?assertEqual({64302, <<>>}, iua_decoder:decode_uint16(<<251, 46>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, <<>>}}, iua_decoder:decode_uint16(<<>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, <<1>>}}, iua_decoder:decode_uint16(<<1>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, not_binary}}, iua_decoder:decode_uint16(not_binary))
    ].

decode_int32_test() ->
    [
     ?assertEqual({0, <<1, 2, 3>>}, iua_decoder:decode_int32(<<0, 0, 0, 0, 1, 2, 3>>)),
     ?assertEqual({123456789, <<1, 2, 3>>}, iua_decoder:decode_int32(<<7,91,205,21, 1, 2, 3>>)),
     ?assertEqual({-123456789, <<>>}, iua_decoder:decode_int32(<<248,164,50,235>>)),
     ?assertEqual({1894, <<4, 5, 6>>}, iua_decoder:decode_int32(<<0,0,7,102, 4, 5, 6>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, <<>>}}, iua_decoder:decode_int32(<<>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, <<1, 2, 3>>}}, iua_decoder:decode_int32(<<1, 2, 3>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, not_binary}}, iua_decoder:decode_int32(not_binary))
    ].

decode_uint32_test() ->
    [
     ?assertEqual({0, <<1, 2, 3>>}, iua_decoder:decode_uint32(<<0, 0, 0, 0, 1, 2, 3>>)),
     ?assertEqual({123456789, <<1, 2, 3>>}, iua_decoder:decode_uint32(<<7,91,205,21, 1, 2, 3>>)),
     ?assertEqual({4171510507, <<>>}, iua_decoder:decode_uint32(<<248,164,50,235>>)),
     ?assertEqual({1894, <<4, 5, 6>>}, iua_decoder:decode_uint32(<<0,0,7,102, 4, 5, 6>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, <<>>}}, iua_decoder:decode_uint32(<<>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, <<1, 2, 3>>}}, iua_decoder:decode_uint32(<<1, 2, 3>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, not_binary}}, iua_decoder:decode_uint32(not_binary))
    ].

decode_string_test() ->
    [
     ?assertEqual({"", <<>>}, iua_decoder:decode_string(<<>>, 0)),
     ?assertEqual({"Test", <<>>}, iua_decoder:decode_string(<<"Test">>, 4)),
     ?assertEqual({"Test", <<"TestTest">>}, iua_decoder:decode_string(<<"TestTestTest">>, 4)),
     ?assertEqual({"1234567890-=qwertyuiop[]asdfghjkl;'\<zxcvbnm,./", <<>>},
                  iua_decoder:decode_string(<<"1234567890-=qwertyuiop[]asdfghjkl;'\<zxcvbnm,./">>, 46)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, {<<"Test">>, 5}}}, iua_decoder:decode_string(<<"Test">>, 5)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, {<<"Test">>, -1}}}, iua_decoder:decode_string(<<"Test">>, -1)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, {not_binary, 1}}}, iua_decoder:decode_string(not_binary, 1)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, {<<"a">>, not_int}}}, iua_decoder:decode_string(<<"a">>, not_int))
    ].

decode_version_test() ->
    [
     ?assertEqual({0, <<1, 2, 3>>}, iua_decoder:decode_version(<<0, 1, 2, 3>>)),
     ?assertEqual({1, <<1, 2, 3>>}, iua_decoder:decode_version(<<1, 1, 2, 3>>)),
     ?assertEqual({15, <<>>}, iua_decoder:decode_version(<<15>>)),
     ?assertEqual({255, <<4, 5, 6>>}, iua_decoder:decode_version(<<255, 4, 5, 6>>)),
     ?assertEqual({175, <<7, 8, 9>>}, iua_decoder:decode_version(<<175, 7, 8, 9>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, <<>>}}, iua_decoder:decode_version(<<>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, not_binary}}, iua_decoder:decode_version(not_binary))
    ].

decode_message_class_test() ->
    [
     ?assertEqual({?CMC_MGMT, <<1, 2, 3>>}, iua_decoder:decode_message_class(<<?CMC_MGMT, 1, 2, 3>>)),
     ?assertEqual({?CMC_T, <<1, 2, 3>>}, iua_decoder:decode_message_class(<<?CMC_T, 1, 2, 3>>)),
     ?assertEqual({?CMC_CO, <<>>}, iua_decoder:decode_message_class(<<?CMC_CO>>)),
     ?assertEqual({255, <<4, 5, 6>>}, iua_decoder:decode_message_class(<<255, 4, 5, 6>>)),
     ?assertEqual({175, <<7, 8, 9>>}, iua_decoder:decode_message_class(<<175, 7, 8, 9>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, <<>>}}, iua_decoder:decode_message_class(<<>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, not_binary}}, iua_decoder:decode_message_class(not_binary))
    ].

decode_message_type_test() ->
    [
     ?assertEqual({?CMT_MGMT_ERR, <<1, 2, 3>>}, iua_decoder:decode_message_type(<<?CMT_MGMT_ERR, 1, 2, 3>>)),
     ?assertEqual({?CMT_ASPTM_ACTIVE, <<1, 2, 3>>}, iua_decoder:decode_message_type(<<?CMT_ASPTM_ACTIVE, 1, 2, 3>>)),
     ?assertEqual({?CMT_ASPSM_BEAT, <<>>}, iua_decoder:decode_message_type(<<?CMT_ASPSM_BEAT>>)),
     ?assertEqual({?CMT_QPTM_ReleaseIndication, <<>>}, iua_decoder:decode_message_type(<<?CMT_QPTM_ReleaseIndication>>)),
     ?assertEqual({255, <<4, 5, 6>>}, iua_decoder:decode_message_type(<<255, 4, 5, 6>>)),
     ?assertEqual({175, <<7, 8, 9>>}, iua_decoder:decode_message_type(<<175, 7, 8, 9>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, <<>>}}, iua_decoder:decode_message_type(<<>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, not_binary}}, iua_decoder:decode_message_type(not_binary))
    ].

decode_dlci_test() ->
    Val1 = #'DLCI'{spr = 0, sapi = 0, tei = 0},
    Val2 = #'DLCI'{spr = 1, sapi = 63, tei = 127},
    Val3 = #'DLCI'{spr = 1, sapi = 17, tei = 78},
    [
     ?assertEqual({Val1, <<>>},      iua_decoder:decode_dlci(<<0:8, 1:1, 0:7>>)),
     ?assertEqual({Val2, <<>>},      iua_decoder:decode_dlci(<<0:1, 1:1, 63:6, 1:1, 127:7>>)),
     ?assertEqual({Val3, <<1,2,3>>},      iua_decoder:decode_dlci(<<0:1, 1:1, 17:6, 1:1, 78:7, 1,2,3>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, <<1:1, 1:1, 17:6, 1:1, 78:7>>}}, iua_decoder:decode_dlci(<<1:1, 1:1, 17:6, 1:1, 78:7>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, <<0:1, 1:1, 17:6, 0:1, 78:7>>}}, iua_decoder:decode_dlci(<<0:1, 1:1, 17:6, 0:1, 78:7>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, no_binary}}, iua_decoder:decode_dlci(no_binary))
    ].

decode_status_test() ->
    Val1 = #'STATUS'{type = ?ST_AS_STATE_CHANGE, id = ?SI_AS_DOWN},
    Val2 = #'STATUS'{type = ?ST_AS_STATE_CHANGE, id = ?SI_AS_INACTIVE},
    Val3 = #'STATUS'{type = ?ST_AS_STATE_CHANGE, id = ?SI_AS_ACTIVE},
    Val4 = #'STATUS'{type = ?ST_AS_STATE_CHANGE, id = ?SI_AS_PENDING},
    Val6 = #'STATUS'{type = ?ST_OTHER, id = ?SI_OTHER_INSUFFICIENT_ASP_RES_ACTIVE_IN_AS},
    Val7 = #'STATUS'{type = ?ST_OTHER, id = ?SI_OTHER_ALT_ASP_ACTIVE},
    [
     ?assertEqual({Val1, <<>>}, iua_decoder:decode_status(<<?ST_AS_STATE_CHANGE:16, ?SI_AS_DOWN:16>>)),
     ?assertEqual({Val2, <<>>}, iua_decoder:decode_status(<<?ST_AS_STATE_CHANGE:16, ?SI_AS_INACTIVE:16>>)),
     ?assertEqual({Val3, <<>>}, iua_decoder:decode_status(<<?ST_AS_STATE_CHANGE:16, ?SI_AS_ACTIVE:16>>)),
     ?assertEqual({Val4, <<>>}, iua_decoder:decode_status(<<?ST_AS_STATE_CHANGE:16, ?SI_AS_PENDING:16>>)),
     ?assertThrow({decode_error, iua_decoder, _, {invalid_record_value, <<?ST_AS_STATE_CHANGE:16, 127:16>>}}, iua_decoder:decode_status(<<?ST_AS_STATE_CHANGE:16, 127:16>>)),     
     ?assertEqual({Val6, <<>>}, iua_decoder:decode_status(<<?ST_OTHER:16, ?SI_OTHER_INSUFFICIENT_ASP_RES_ACTIVE_IN_AS:16>>)),
     ?assertEqual({Val7, <<>>}, iua_decoder:decode_status(<<?ST_OTHER:16, ?SI_OTHER_ALT_ASP_ACTIVE:16>>)),
     ?assertThrow({decode_error, iua_decoder, _, {invalid_record_value, <<?ST_OTHER:16, 15:16>>}}, iua_decoder:decode_status(<<?ST_OTHER:16, 15:16>>)),
     ?assertThrow({decode_error, iua_decoder, _, {invalid_record_value, <<3:16, 3:16>>}}, iua_decoder:decode_status(<<3:16, 3:16>>))
    ].

decode_common_message_header_test() ->
    Val1 = #'CommonMessageHeader'{class = ?CMC_MGMT, type = ?CMT_MGMT_TEI_Request, length = 15},
    Val2 = #'CommonMessageHeader'{version = 2, class = ?CMC_QPTM, type = ?CMT_QPTM_Reserved, length = 15},
    Val3 = #'CommonMessageHeader'{class = ?CMC_CL, type = 182, length = 15},
    Val4 = #'CommonMessageHeader'{class = 18, type = 1, length = 17},
    Val5 = #'CommonMessageHeader'{version = 5, class = 172, type = 208, length = 1024},
    [
     ?assertEqual({Val1, <<>>},      iua_decoder:decode_common_message_header(<<1,0,0,2,0,0,0,15>>)),
     ?assertEqual({Val2, <<1,2,3>>}, iua_decoder:decode_common_message_header(<<2,0,5,0,0,0,0,15, 1,2,3>>)),
     ?assertEqual({Val3, <<3,4,5>>}, iua_decoder:decode_common_message_header(<<1,0,7,182,0,0,0,15, 3,4,5>>)),
     ?assertEqual({Val4,<<>>},       iua_decoder:decode_common_message_header(<<1,0,18,1,0,0,0,17>>)),
     ?assertEqual({Val5, <<>>},      iua_decoder:decode_common_message_header(<<5,0,172,208,0,0,4,0>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, <<>>}}, iua_decoder:decode_common_message_header(<<>>)),
     % {badarg, <<>>} - first three octects was parsed.
     ?assertThrow({decode_error, iua_decoder, _, {badarg, <<>>}}, iua_decoder:decode_common_message_header(<<1, 2, 3>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, not_binary}}, iua_decoder:decode_common_message_header(not_binary))
    ].

decode_vl_parameter_test() ->
    Val1 = #'VLParameter'{tag = ?VL_INTEGER, length = 8, value = 123},
    Val2 = #'VLParameter'{tag = ?VL_INTEGER, length = 8, value = 968},
    Val3 = #'VLParameter'{tag = ?VL_TEXT, length = 4, value = ""},
    Val4 = #'VLParameter'{tag = ?VL_TEXT, length = 18, value = "Hello World!!!"},
    Val5 = #'VLParameter'{tag = ?VL_INFO_STRING, length = 4, value = ""},
    Val6 = #'VLParameter'{tag = ?VL_INFO_STRING, length = 18, value = "Hello World!!!"},
    Val7 = #'VLParameter'{tag = ?VL_DLCI, length = 6, value =  #'DLCI'{spr = 0, sapi = 0, tei = 0}},
    Val8 = #'VLParameter'{tag = ?VL_DLCI, length = 6, value = #'DLCI'{spr = 1, sapi = 63, tei = 127}},
    Val9 = #'VLParameter'{tag = ?VL_DLCI, length = 6, value = #'DLCI'{spr = 1, sapi = 17, tei = 78}},
    Val10 = #'VLParameter'{tag = ?VL_DIAGNOSTIC_INFO, length = 4, value =  <<>>},
    Val11 = #'VLParameter'{tag = ?VL_DIAGNOSTIC_INFO, length = 9, value = <<1,2,3,4,5>>},
    Val12 = #'VLParameter'{tag = ?VL_INTEGER_RANGE, length = 4, value = []},
    Val13 = #'VLParameter'{tag = ?VL_INTEGER_RANGE, length = 16, value = [1, 2, 3]},
    Val14 = #'VLParameter'{tag = ?VL_INTEGER_RANGE, length = 24, value = [1, 2, 3, 934, 14324]},
    Val15 = #'VLParameter'{tag = ?VL_HEARTBEAT, length = 4, value =  <<>>},
    Val16 = #'VLParameter'{tag = ?VL_HEARTBEAT, length = 9, value = <<1,2,3,4,5>>},
    Val17 = #'VLParameter'{tag = ?VL_ASPDN_REASON, length = 8, value = ?ASPDN_MGMT_INHIBIT_REASON},
    Val18 = #'VLParameter'{tag = ?VL_ASPDN_REASON, length = 8, value = 968},
    Val19 = #'VLParameter'{tag = ?VL_TRAFFIC_MODE, length = 8, value = ?OVER_RIDE_TRAFIC_MODE},
    Val20 = #'VLParameter'{tag = ?VL_TRAFFIC_MODE, length = 8, value = ?LOAD_SHARE_TRAFIC_MODE},
    Val21 = #'VLParameter'{tag = ?VL_TRAFFIC_MODE, length = 8, value = 968},
    Val22 = #'VLParameter'{tag = ?VL_ERROR_CODE, length = 8, value = ?InvalidVersion},
    Val23 = #'VLParameter'{tag = ?VL_ERROR_CODE, length = 8, value = ?UnsupportedInterfaceIdentifierType},
    Val24 = #'VLParameter'{tag = ?VL_ERROR_CODE, length = 8, value = 968},
    Val25 = #'VLParameter'{tag = ?VL_STATUS, length = 8, value = #'STATUS'{type = ?ST_AS_STATE_CHANGE, id = ?SI_AS_DOWN}},
    Val26 = #'VLParameter'{tag = ?VL_STATUS, length = 8, value = #'STATUS'{type = ?ST_OTHER, id = ?SI_OTHER_ALT_ASP_ACTIVE}},
    Val27 = #'VLParameter'{tag = ?VL_PROTOCOL_DATA, length = 4, value =  <<>>},
    Val28 = #'VLParameter'{tag = ?VL_PROTOCOL_DATA, length = 9, value = <<1,2,3,4,5>>},
    Val29 = #'VLParameter'{tag = ?VL_RELEASE_REASON, length = 8, value = ?RELEASE_MGMT},
    Val30 = #'VLParameter'{tag = ?VL_RELEASE_REASON, length = 8, value = ?RELEASE_OTHER},
    Val31 = #'VLParameter'{tag = ?VL_TEI_STATUS, length = 8, value = ?TEI_STATUS_ASSIGNED},
    Val32 = #'VLParameter'{tag = ?VL_TEI_STATUS, length = 8, value = ?TEI_STATUS_UNASSIGNED},
    [
     ?assertEqual({Val1, <<>>}, iua_decoder:decode_vl_parameter(<<?VL_INTEGER:16, 8:16, 123:32>>)),
     ?assertEqual({Val2, <<>>}, iua_decoder:decode_vl_parameter(<<?VL_INTEGER:16, 8:16, 968:32>>)),
     ?assertEqual({Val3, <<>>}, iua_decoder:decode_vl_parameter(<<?VL_TEXT:16, 4:16>>)),
     ?assertEqual({Val4, <<>>}, iua_decoder:decode_vl_parameter(<<?VL_TEXT:16, 18:16, <<72,101,108,108,111,32,87,111,114,108,100,33,33,33,0,0>>/binary>>)),
     ?assertEqual({Val4, <<1,2,3>>}, iua_decoder:decode_vl_parameter(<<?VL_TEXT:16, 18:16, <<72,101,108,108,111,32,87,111,114,108,100,33,33,33,0,0,1,2,3>>/binary>>)),
     ?assertEqual({Val5, <<>>}, iua_decoder:decode_vl_parameter(<<?VL_INFO_STRING:16, 4:16>>)),
     ?assertEqual({Val6, <<>>}, iua_decoder:decode_vl_parameter(<<?VL_INFO_STRING:16, 18:16, <<72,101,108,108,111,32,87,111,114,108,100,33,33,33,0,0>>/binary>>)),
     ?assertEqual({Val6, <<1,2,3>>}, iua_decoder:decode_vl_parameter(<<?VL_INFO_STRING:16, 18:16, <<72,101,108,108,111,32,87,111,114,108,100,33,33,33,0,0,1,2,3>>/binary>>)),
     ?assertThrow({decode_error, iua_decoder, _, {invalid_length, 260}}, iua_decoder:decode_vl_parameter(<<?VL_INFO_STRING:16, 260:16, (multiplier(<<49,50,51,52,53,54,55,56,57,48>>, 26))/binary>>)),
     ?assertEqual({Val7, <<>>},      iua_decoder:decode_vl_parameter(<<?VL_DLCI:16, 6:16, 0:8, 1:1, 0:7, 0:16>>)),
     ?assertEqual({Val8, <<>>},      iua_decoder:decode_vl_parameter(<<?VL_DLCI:16, 6:16, 0:1, 1:1, 63:6, 1:1, 127:7, 0:16>>)),
     ?assertEqual({Val9, <<1,2,3>>}, iua_decoder:decode_vl_parameter(<<?VL_DLCI:16, 6:16, 0:1, 1:1, 17:6, 1:1, 78:7, 0:16, 1,2,3>>)),
     ?assertEqual({Val10, <<>>},      iua_decoder:decode_vl_parameter(<<?VL_DIAGNOSTIC_INFO:16, 4:16>>)),
     ?assertEqual({Val11, <<>>},      iua_decoder:decode_vl_parameter(<<?VL_DIAGNOSTIC_INFO:16, 9:16, 1,2,3,4,5, 0,0,0>>)),
     ?assertEqual({Val11, <<1,2,3>>}, iua_decoder:decode_vl_parameter(<<?VL_DIAGNOSTIC_INFO:16, 9:16, 1,2,3,4,5, 0,0,0,1,2,3>>)),
     ?assertEqual({Val12, <<>>},      iua_decoder:decode_vl_parameter(<<?VL_INTEGER_RANGE:16, 4:16>>)),
     ?assertEqual({Val13, <<>>}, iua_decoder:decode_vl_parameter(<<?VL_INTEGER_RANGE:16, 16:16, 1:32, 2:32, 3:32>>)),
     ?assertEqual({Val14, <<1,2,3>>}, iua_decoder:decode_vl_parameter(<<?VL_INTEGER_RANGE:16, 24:16, 1:32, 2:32, 3:32, 934:32, 14324:32, 1,2,3>>)),
     ?assertEqual({Val15, <<>>},      iua_decoder:decode_vl_parameter(<<?VL_HEARTBEAT:16, 4:16>>)),
     ?assertEqual({Val16, <<>>},      iua_decoder:decode_vl_parameter(<<?VL_HEARTBEAT:16, 9:16, 1,2,3,4,5, 0,0,0>>)),
     ?assertEqual({Val16, <<1,2,3>>}, iua_decoder:decode_vl_parameter(<<?VL_HEARTBEAT:16, 9:16, 1,2,3,4,5, 0,0,0,1,2,3>>)),
     ?assertEqual({Val17, <<>>}, iua_decoder:decode_vl_parameter(<<?VL_ASPDN_REASON:16, 8:16, ?ASPDN_MGMT_INHIBIT_REASON:32>>)),
     ?assertEqual({Val18, <<>>}, iua_decoder:decode_vl_parameter(<<?VL_ASPDN_REASON:16, 8:16, 968:32>>)),
     ?assertEqual({Val19, <<>>}, iua_decoder:decode_vl_parameter(<<?VL_TRAFFIC_MODE:16, 8:16, ?OVER_RIDE_TRAFIC_MODE:32>>)),
     ?assertEqual({Val20, <<>>}, iua_decoder:decode_vl_parameter(<<?VL_TRAFFIC_MODE:16, 8:16, ?LOAD_SHARE_TRAFIC_MODE:32>>)),
     ?assertEqual({Val21, <<>>}, iua_decoder:decode_vl_parameter(<<?VL_TRAFFIC_MODE:16, 8:16, 968:32>>)),
     ?assertEqual({Val22, <<>>}, iua_decoder:decode_vl_parameter(<<?VL_ERROR_CODE:16, 8:16, ?InvalidVersion:32>>)),
     ?assertEqual({Val23, <<1,2,3>>}, iua_decoder:decode_vl_parameter(<<?VL_ERROR_CODE:16, 8:16, ?UnsupportedInterfaceIdentifierType:32, 1,2,3>>)),
     ?assertEqual({Val24, <<>>}, iua_decoder:decode_vl_parameter(<<?VL_ERROR_CODE:16, 8:16, 968:32>>)),
     ?assertEqual({Val25, <<>>}, iua_decoder:decode_vl_parameter(<<?VL_STATUS:16, 8:16, ?ST_AS_STATE_CHANGE:16, ?SI_AS_DOWN:16>>)),
     ?assertEqual({Val26, <<1,2,3>>}, iua_decoder:decode_vl_parameter(<<?VL_STATUS:16, 8:16, ?ST_OTHER:16, ?SI_OTHER_ALT_ASP_ACTIVE:16, 1,2,3>>)),
     ?assertEqual({Val27, <<>>},      iua_decoder:decode_vl_parameter(<<?VL_PROTOCOL_DATA:16, 4:16>>)),
     ?assertEqual({Val28, <<>>},      iua_decoder:decode_vl_parameter(<<?VL_PROTOCOL_DATA:16, 9:16, 1,2,3,4,5, 0,0,0>>)),
     ?assertEqual({Val28, <<1,2,3>>}, iua_decoder:decode_vl_parameter(<<?VL_PROTOCOL_DATA:16, 9:16, 1,2,3,4,5, 0,0,0,1,2,3>>)),
     ?assertEqual({Val29, <<>>}, iua_decoder:decode_vl_parameter(<<?VL_RELEASE_REASON:16, 8:16, ?RELEASE_MGMT:32>>)),
     ?assertEqual({Val30, <<>>}, iua_decoder:decode_vl_parameter(<<?VL_RELEASE_REASON:16, 8:16, ?RELEASE_OTHER:32>>)),
     ?assertThrow({decode_error, iua_decoder, _, {invalid_param, {?VL_RELEASE_REASON, 968}}}, iua_decoder:decode_vl_parameter(<<?VL_RELEASE_REASON:16, 8:16, 968:32>>)),
     ?assertEqual({Val31, <<>>}, iua_decoder:decode_vl_parameter(<<?VL_TEI_STATUS:16, 8:16, ?TEI_STATUS_ASSIGNED:32>>)),
     ?assertEqual({Val32, <<>>}, iua_decoder:decode_vl_parameter(<<?VL_TEI_STATUS:16, 8:16, ?TEI_STATUS_UNASSIGNED:32>>)),
     ?assertThrow({decode_error, iua_decoder, _, {invalid_param, {?VL_TEI_STATUS, 968}}}, iua_decoder:decode_vl_parameter(<<?VL_TEI_STATUS:16, 8:16, 968:32>>))
    ].

decode_iua_message_header_test() ->
    Val1 = #'IUAMessageHeader'{interface_identifier = #'VLParameter'{tag = ?VL_INTEGER, length = 8, value = 123},
                               dlci = #'VLParameter'{tag = ?VL_DLCI, length = 6, value = #'DLCI'{spr = 1, sapi = 17, tei = 78}}},
    Val2 = #'IUAMessageHeader'{interface_identifier = #'VLParameter'{tag = ?VL_TEXT, length = 18, value = "test interface"},
                               dlci = #'VLParameter'{tag = ?VL_DLCI, length = 6, value = #'DLCI'{spr = 1, sapi = 17, tei = 78}}},
    Val3 = <<?VL_PROTOCOL_DATA:16, 4:16, <<?VL_DLCI:16, 6:16, <<0:1, 1:1, 17:6, 1:1, 78:7>>/binary, 0,0>>/binary>>,
    Val4 = <<?VL_INTEGER:16, 8:16, 123:32, <<?VL_PROTOCOL_DATA:16, 4:16>>/binary>>,
    Val5 = <<?VL_INTEGER:16, 8:16, 123:32>>,
    [
     ?assertEqual({Val1, <<>>}, iua_decoder:decode_iua_message_header(<<?VL_INTEGER:16, 8:16, 123:32, <<?VL_DLCI:16, 6:16, <<0:1, 1:1, 17:6, 1:1, 78:7>>/binary, 0,0>>/binary>>)),
     
     ?assertEqual({Val2, <<>>}, iua_decoder:decode_iua_message_header(<<?VL_TEXT:16, 18:16, <<116,101,115,116,32,105,110,116,101,114,102,97,99,101>>/binary, 0,0, <<?VL_DLCI:16, 6:16, <<0:1, 1:1, 17:6, 1:1, 78:7>>/binary, 0,0>>/binary>>)),
     
     ?assertThrow({decode_error, iua_decoder, _, {badarg, Val3}}, iua_decoder:decode_iua_message_header(Val3)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, Val4}}, iua_decoder:decode_iua_message_header(Val4)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, _}}, iua_decoder:decode_iua_message_header(Val5))
    ].

decode_establish_msg_test() ->
    CHeader1 = #'CommonMessageHeader'{class = ?CMC_QPTM, type = ?CMT_QPTM_EstablishRequest, length = 24},
    CHeader2 = #'CommonMessageHeader'{class = ?CMC_QPTM, type = ?CMT_QPTM_EstablishConfirm, length = 24},
    CHeader3 = #'CommonMessageHeader'{class = ?CMC_QPTM, type = ?CMT_QPTM_EstablishIndication, length = 24},
    IHeader = #'IUAMessageHeader'{interface_identifier = #'VLParameter'{tag = ?VL_INTEGER, length = 8, value = 123},
                                  dlci = #'VLParameter'{tag = ?VL_DLCI, length = 6, value = #'DLCI'{spr = 1, sapi = 17, tei = 78}}},
    IHeaderBin = <<?VL_INTEGER:16, 8:16, 123:32, <<?VL_DLCI:16, 6:16, <<0:1, 1:1, 17:6, 1:1, 78:7>>/binary, 0,0>>/binary>>,
    Val1 = #'EstablishMsg'{common_header = CHeader1, iua_header = IHeader},
    Val2 = #'EstablishMsg'{common_header = CHeader2, iua_header = IHeader},
    Val3 = #'EstablishMsg'{common_header = CHeader3, iua_header = IHeader},   
    [
     ?assertEqual({Val1, <<>>}, iua_decoder:decode_msg(<<1,0,?CMC_QPTM,?CMT_QPTM_EstablishRequest,0,0,0,24, IHeaderBin/binary>>)),
     ?assertEqual({Val2, <<>>}, iua_decoder:decode_msg(<<1,0,?CMC_QPTM,?CMT_QPTM_EstablishConfirm,0,0,0,24, IHeaderBin/binary>>)),
     ?assertEqual({Val3, <<3,4,5>>}, iua_decoder:decode_msg(<<1,0,?CMC_QPTM,?CMT_QPTM_EstablishIndication,0,0,0,24,IHeaderBin/binary,3,4,5>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, _}},
                  iua_decoder:decode_msg(<<1,0,?CMC_QPTM,5,0,0,0,24>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, _}},
                  iua_decoder:decode_msg(<<1,0,?CMC_QPTM,5,0,0,0,20, IHeaderBin/binary>>)),
     ?assertThrow({decode_error, iua_decoder, _, {invalid_length, _}},
                  iua_decoder:decode_msg(<<1,0,?CMC_QPTM,5,0,0,0,25, IHeaderBin/binary,3,4,5>>))
    ].

decode_reason_msg_test() ->
    CHeader1 = #'CommonMessageHeader'{class = ?CMC_QPTM, type = ?CMT_QPTM_ReleaseRequest, length = 32},
    CHeader2 = #'CommonMessageHeader'{class = ?CMC_QPTM, type = ?CMT_QPTM_ReleaseIndication, length = 32},
    CHeader3 = #'CommonMessageHeader'{class = ?CMC_QPTM, type = ?CMT_QPTM_ReleaseConfirm, length = 24},
    IHeader = #'IUAMessageHeader'{interface_identifier = #'VLParameter'{tag = ?VL_INTEGER, length = 8, value = 123},
                                  dlci = #'VLParameter'{tag = ?VL_DLCI, length = 6, value = #'DLCI'{spr = 1, sapi = 17, tei = 78}}},
    IHeaderBin = <<?VL_INTEGER:16, 8:16, 123:32, <<?VL_DLCI:16, 6:16, <<0:1, 1:1, 17:6, 1:1, 78:7>>/binary, 0,0>>/binary>>,
    Reason1 = #'VLParameter'{tag = ?VL_RELEASE_REASON, length = 8, value = ?RELEASE_MGMT},
    BReason1 = <<?VL_RELEASE_REASON:16, 8:16, ?RELEASE_MGMT:32>>,
    Reason2 = #'VLParameter'{tag = ?VL_RELEASE_REASON, length = 8, value = ?RELEASE_DM},
    BReason2 = <<?VL_RELEASE_REASON:16, 8:16, ?RELEASE_DM:32>>,
    Reason3 = #'VLParameter'{tag = ?VL_RELEASE_REASON, length = 8, value = ?RELEASE_OTHER},
    BReason3 = <<?VL_RELEASE_REASON:16, 8:16, ?RELEASE_OTHER:32>>,
    Reason4 = #'VLParameter'{tag = ?VL_RELEASE_REASON, length = 8, value = ?RELEASE_PHYS},
    BReason4 = <<?VL_RELEASE_REASON:16, 8:16, ?RELEASE_PHYS:32>>,
    BReasonE1 = <<?VL_INTEGER:16, 8:16, 123:32>>,
    Val1 = #'ReleaseMsg'{common_header = CHeader1, iua_header = IHeader, reason = Reason1},
    Val2 = #'ReleaseMsg'{common_header = CHeader2, iua_header = IHeader, reason = Reason2},
    Val3 = #'ReleaseMsg'{common_header = CHeader3, iua_header = IHeader},
    Val4 = #'ReleaseMsg'{common_header = CHeader2, iua_header = IHeader, reason = Reason4},
    Val5 = #'ReleaseMsg'{common_header = CHeader1, iua_header = IHeader, reason = Reason3},
    [
     ?assertEqual({Val1, <<>>}, iua_decoder:decode_msg(<<1,0,?CMC_QPTM,?CMT_QPTM_ReleaseRequest,0,0,0,32, IHeaderBin/binary, BReason1/binary>>)),
     ?assertEqual({Val2, <<>>}, iua_decoder:decode_msg(<<1,0,?CMC_QPTM,?CMT_QPTM_ReleaseIndication,0,0,0,32, IHeaderBin/binary, BReason2/binary>>)),
     ?assertEqual({Val3, <<>>}, iua_decoder:decode_msg(<<1,0,?CMC_QPTM,?CMT_QPTM_ReleaseConfirm,0,0,0,24, IHeaderBin/binary>>)),
     ?assertEqual({Val4, <<>>}, iua_decoder:decode_msg(<<1,0,?CMC_QPTM,?CMT_QPTM_ReleaseIndication,0,0,0,32, IHeaderBin/binary, BReason4/binary>>)),
     ?assertEqual({Val5, <<>>}, iua_decoder:decode_msg(<<1,0,?CMC_QPTM,?CMT_QPTM_ReleaseRequest,0,0,0,32, IHeaderBin/binary, BReason3/binary>>)),
     ?assertThrow({decode_error, iua_decoder, _, {invalid_reason_to_type, _}},
                  iua_decoder:decode_msg(<<1,0,?CMC_QPTM,?CMT_QPTM_ReleaseRequest,0,0,0,32, IHeaderBin/binary, BReason4/binary>>)),
     ?assertThrow({decode_error, iua_decoder, _, {invalid_length, _}},
                  iua_decoder:decode_msg(<<1,0,?CMC_QPTM,?CMT_QPTM_ReleaseConfirm,0,0,0,32, IHeaderBin/binary, 0:64>>)),
     ?assertThrow({decode_error, iua_decoder, _, {invalid_param, _}},
                  iua_decoder:decode_msg(<<1,0,?CMC_QPTM,?CMT_QPTM_ReleaseRequest,0,0,0,32, IHeaderBin/binary, BReasonE1/binary>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, _}},
                  iua_decoder:decode_msg(<<1,0,?CMC_QPTM,?CMT_QPTM_ReleaseConfirm,0,0,0,32, IHeaderBin/binary>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, no_header}}, iua_decoder:decode_msg(no_header))
    ].

decode_data_msg_test() ->
    CHeader1 = #'CommonMessageHeader'{class = ?CMC_QPTM, type = ?CMT_QPTM_DataRequest, length = 32},
    CHeader2 = #'CommonMessageHeader'{class = ?CMC_QPTM, type = ?CMT_QPTM_DataIndication, length = 32},
    CHeader4 = #'CommonMessageHeader'{class = ?CMC_QPTM, type = ?CMT_QPTM_DataIndication, length = 36},
    IHeader = #'IUAMessageHeader'{interface_identifier = #'VLParameter'{tag = ?VL_INTEGER, length = 8, value = 123},
                                  dlci = #'VLParameter'{tag = ?VL_DLCI, length = 6, value = #'DLCI'{spr = 1, sapi = 17, tei = 78}}},
    IHeaderBin = <<?VL_INTEGER:16, 8:16, 123:32, <<?VL_DLCI:16, 6:16, <<0:1, 1:1, 17:6, 1:1, 78:7>>/binary, 0,0>>/binary>>,
    Reason1 = #'VLParameter'{tag = ?VL_PROTOCOL_DATA, length = 8, value = <<1,2,3,4>>},
    BReason1 = <<?VL_PROTOCOL_DATA:16, 8:16, 1,2,3,4>>,
    Reason2 = #'VLParameter'{tag = ?VL_PROTOCOL_DATA, length = 8, value = <<5,6,7,8>>},
    BReason2 = <<?VL_PROTOCOL_DATA:16, 8:16, 5,6,7,8>>,
    Reason3 = #'VLParameter'{tag = ?VL_PROTOCOL_DATA, length = 8, value = <<110,111,112,113>>},
    BReason3 = <<?VL_PROTOCOL_DATA:16, 8:16, 110,111,112,113>>,
    Reason4 = #'VLParameter'{tag = ?VL_PROTOCOL_DATA, length = 9, value = <<200,201,202,203,204>>},
    BReason4 = <<?VL_PROTOCOL_DATA:16, 9:16, 200,201,202,203,204, 0,0,0>>,
    BReasonE1 = <<?VL_INTEGER:16, 8:16, 123:32>>,
    Val1 = #'DataMsg'{common_header = CHeader1, iua_header = IHeader, data = Reason1},
    Val2 = #'DataMsg'{common_header = CHeader2, iua_header = IHeader, data = Reason2},
    Val4 = #'DataMsg'{common_header = CHeader4, iua_header = IHeader, data = Reason4},
    Val5 = #'DataMsg'{common_header = CHeader1, iua_header = IHeader, data = Reason3},
    [
     ?assertEqual({Val1, <<>>}, iua_decoder:decode_msg(<<1,0,?CMC_QPTM,?CMT_QPTM_DataRequest,0,0,0,32, IHeaderBin/binary, BReason1/binary>>)),
     ?assertEqual({Val2, <<>>}, iua_decoder:decode_msg(<<1,0,?CMC_QPTM,?CMT_QPTM_DataIndication,0,0,0,32, IHeaderBin/binary, BReason2/binary>>)),
     ?assertEqual({Val4, <<>>}, iua_decoder:decode_msg(<<1,0,?CMC_QPTM,?CMT_QPTM_DataIndication,0,0,0,36, IHeaderBin/binary, BReason4/binary>>)),
     ?assertEqual({Val5, <<>>}, iua_decoder:decode_msg(<<1,0,?CMC_QPTM,?CMT_QPTM_DataRequest,0,0,0,32, IHeaderBin/binary, BReason3/binary>>)),
     ?assertThrow({decode_error, iua_decoder, _, {invalid_length, _}},
                  iua_decoder:decode_msg(<<1,0,?CMC_QPTM,?CMT_QPTM_DataIndication,0,0,0,40, IHeaderBin/binary, BReason4/binary,0,0,0,0>>)),
     ?assertThrow({decode_error, iua_decoder, _, {invalid_data_tag, _}},
                  iua_decoder:decode_msg(<<1,0,?CMC_QPTM,?CMT_QPTM_DataRequest,0,0,0,32, IHeaderBin/binary, BReasonE1/binary>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, _}},
                  iua_decoder:decode_msg(<<1,0,?CMC_QPTM,?CMT_QPTM_DataRequest,0,0,0,32, IHeaderBin/binary>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, no_header}}, iua_decoder:decode_msg(no_header))
    ].

decode_unit_data_msg_test() ->
    CHeader1 = #'CommonMessageHeader'{class = ?CMC_QPTM, type = ?CMT_QPTM_UnitDataRequest, length = 32},
    CHeader2 = #'CommonMessageHeader'{class = ?CMC_QPTM, type = ?CMT_QPTM_UnitDataIndication, length = 32},
    CHeader4 = #'CommonMessageHeader'{class = ?CMC_QPTM, type = ?CMT_QPTM_UnitDataIndication, length = 36},
    IHeader = #'IUAMessageHeader'{interface_identifier = #'VLParameter'{tag = ?VL_INTEGER, length = 8, value = 123},
                                  dlci = #'VLParameter'{tag = ?VL_DLCI, length = 6, value = #'DLCI'{spr = 1, sapi = 17, tei = 78}}},
    IHeaderBin = <<?VL_INTEGER:16, 8:16, 123:32, <<?VL_DLCI:16, 6:16, <<0:1, 1:1, 17:6, 1:1, 78:7>>/binary, 0,0>>/binary>>,
    Reason1 = #'VLParameter'{tag = ?VL_PROTOCOL_DATA, length = 8, value = <<1,2,3,4>>},
    BReason1 = <<?VL_PROTOCOL_DATA:16, 8:16, 1,2,3,4>>,
    Reason2 = #'VLParameter'{tag = ?VL_PROTOCOL_DATA, length = 8, value = <<5,6,7,8>>},
    BReason2 = <<?VL_PROTOCOL_DATA:16, 8:16, 5,6,7,8>>,
    Reason3 = #'VLParameter'{tag = ?VL_PROTOCOL_DATA, length = 8, value = <<110,111,112,113>>},
    BReason3 = <<?VL_PROTOCOL_DATA:16, 8:16, 110,111,112,113>>,
    Reason4 = #'VLParameter'{tag = ?VL_PROTOCOL_DATA, length = 9, value = <<200,201,202,203,204>>},
    BReason4 = <<?VL_PROTOCOL_DATA:16, 9:16, 200,201,202,203,204, 0,0,0>>,
    BReasonE1 = <<?VL_INTEGER:16, 8:16, 123:32>>,
    Val1 = #'UnitDataMsg'{common_header = CHeader1, iua_header = IHeader, data = Reason1},
    Val2 = #'UnitDataMsg'{common_header = CHeader2, iua_header = IHeader, data = Reason2},
    Val4 = #'UnitDataMsg'{common_header = CHeader4, iua_header = IHeader, data = Reason4},
    Val5 = #'UnitDataMsg'{common_header = CHeader1, iua_header = IHeader, data = Reason3},
    [
     ?assertEqual({Val1, <<>>}, iua_decoder:decode_msg(<<1,0,?CMC_QPTM,?CMT_QPTM_UnitDataRequest,0,0,0,32, IHeaderBin/binary, BReason1/binary>>)),
     ?assertEqual({Val2, <<>>}, iua_decoder:decode_msg(<<1,0,?CMC_QPTM,?CMT_QPTM_UnitDataIndication,0,0,0,32, IHeaderBin/binary, BReason2/binary>>)),
     ?assertEqual({Val4, <<>>}, iua_decoder:decode_msg(<<1,0,?CMC_QPTM,?CMT_QPTM_UnitDataIndication,0,0,0,36, IHeaderBin/binary, BReason4/binary>>)),
     ?assertEqual({Val5, <<>>}, iua_decoder:decode_msg(<<1,0,?CMC_QPTM,?CMT_QPTM_UnitDataRequest,0,0,0,32, IHeaderBin/binary, BReason3/binary>>)),
     ?assertThrow({decode_error, iua_decoder, _, {invalid_length, _}},
                  iua_decoder:decode_msg(<<1,0,?CMC_QPTM,?CMT_QPTM_UnitDataIndication,0,0,0,40, IHeaderBin/binary, BReason4/binary,0,0,0,0>>)),
     ?assertThrow({decode_error, iua_decoder, _, {invalid_data_tag, _}},
                  iua_decoder:decode_msg(<<1,0,?CMC_QPTM,?CMT_QPTM_UnitDataRequest,0,0,0,32, IHeaderBin/binary, BReasonE1/binary>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, _}},
                  iua_decoder:decode_msg(<<1,0,?CMC_QPTM,?CMT_QPTM_UnitDataRequest,0,0,0,32, IHeaderBin/binary>>)),
     ?assertThrow({decode_error, iua_decoder, _, {badarg, no_header}}, iua_decoder:decode_msg(no_header))
    ].

multiplier(Src, Count) when is_binary(Src), is_integer(Count) ->
    erlang:list_to_binary(lists:duplicate(Count, Src)).

-endif.
