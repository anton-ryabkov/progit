%%%-------------------------------------------------------------------
%%% @author Anton N Ryabkov <anton.ryabkov@gmail.com>
%%% @copyright (C) 2011, Eltex
%%% @doc
%%%
%%% @end
%%% Created :  5 Apr 2011 by Anton N Ryabkov <anton.ryabkov@gmail.com>
%%%-------------------------------------------------------------------
-module(iua_mix_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../include/iua.hrl").

-ifdef(EUNIT).

de_octet_test() ->
    Val0 = iua_encoder:encode_octet(0),
    ?assertEqual({0, <<>>}, iua_decoder:decode_octet(Val0)),
    Val1 = iua_encoder:encode_octet(1),
    ?assertEqual({1, <<>>}, iua_decoder:decode_octet(Val1)),
    Val2 = iua_encoder:encode_octet(255),
    ?assertEqual({255, <<>>}, iua_decoder:decode_octet(Val2)),
    Val3 = iua_encoder:encode_octet(175),
    ?assertEqual({175, <<>>}, iua_decoder:decode_octet(Val3)).

de_version_test() ->
    Val0 = iua_encoder:encode_version(0),
    ?assertEqual({0, <<>>}, iua_decoder:decode_version(Val0)),
    Val1 = iua_encoder:encode_version(1),
    ?assertEqual({1, <<>>}, iua_decoder:decode_version(Val1)),
    Val2 = iua_encoder:encode_version(255),
    ?assertEqual({255, <<>>}, iua_decoder:decode_version(Val2)),
    Val3 = iua_encoder:encode_version(175),
    ?assertEqual({175, <<>>}, iua_decoder:decode_version(Val3)).

de_int32_test() ->
    Val0 = iua_encoder:encode_int32(0),
    ?assertEqual({0, <<>>}, iua_decoder:decode_int32(Val0)),
    Val1 = iua_encoder:encode_int32(?MIN_INT32),
    ?assertEqual({?MIN_INT32, <<>>}, iua_decoder:decode_int32(Val1)),
    Val2 = iua_encoder:encode_int32(?MAX_INT32),
    ?assertEqual({?MAX_INT32, <<>>}, iua_decoder:decode_int32(Val2)),
    Val3 = iua_encoder:encode_int32(175),
    ?assertEqual({175, <<>>}, iua_decoder:decode_int32(Val3)).

de_uint32_test() ->
    Val0 = iua_encoder:encode_uint32(0),
    ?assertEqual({0, <<>>}, iua_decoder:decode_uint32(Val0)),
    Val1 = iua_encoder:encode_uint32(45654684),
    ?assertEqual({45654684, <<>>}, iua_decoder:decode_uint32(Val1)),
    Val2 = iua_encoder:encode_uint32(?MAX_UINT32),
    ?assertEqual({?MAX_UINT32, <<>>}, iua_decoder:decode_uint32(Val2)),
    Val3 = iua_encoder:encode_uint32(175),
    ?assertEqual({175, <<>>}, iua_decoder:decode_uint32(Val3)).

de_message_class_test() ->
    Val0 = iua_encoder:encode_message_class(?CMC_MGMT),
    ?assertEqual({?CMC_MGMT, <<>>}, iua_decoder:decode_message_class(Val0)),
    Val1 = iua_encoder:encode_message_class(?CMC_T),
    ?assertEqual({?CMC_T, <<>>}, iua_decoder:decode_message_class(Val1)),
    Val2 = iua_encoder:encode_message_class(255),
    ?assertEqual({255, <<>>}, iua_decoder:decode_message_class(Val2)),
    Val3 = iua_encoder:encode_message_class(175),
    ?assertEqual({175, <<>>}, iua_decoder:decode_message_class(Val3)).

de_message_type_test() ->
    Val0 = iua_encoder:encode_message_type(?CMT_MGMT_ERR),
    ?assertEqual({?CMT_MGMT_ERR, <<>>}, iua_decoder:decode_message_type(Val0)),
    Val1 = iua_encoder:encode_message_type(?CMT_MGMT_TEI_Confirm),
    ?assertEqual({?CMT_MGMT_TEI_Confirm, <<>>}, iua_decoder:decode_message_type(Val1)),
    Val2 = iua_encoder:encode_message_type(255),
    ?assertEqual({255, <<>>}, iua_decoder:decode_message_type(Val2)),
    Val3 = iua_encoder:encode_message_type(175),
    ?assertEqual({175, <<>>}, iua_decoder:decode_message_type(Val3)).

de_common_message_header_test() ->
    Src0 = #'CommonMessageHeader'{class = ?CMC_MGMT, type = ?CMT_MGMT_TEI_Request, length = 15},
    Src1 = #'CommonMessageHeader'{version = 2, class = ?CMC_QPTM, type = ?CMT_QPTM_Reserved, length = 15},
    Src2 = #'CommonMessageHeader'{class = ?CMC_CL, type = 182, length = 15},
    Val0 = iua_encoder:encode_common_message_header(Src0),
    ?assertEqual({Src0, <<>>}, iua_decoder:decode_common_message_header(Val0)),
    Val1 = iua_encoder:encode_common_message_header(Src1),
    ?assertEqual({Src1, <<>>}, iua_decoder:decode_common_message_header(Val1)),
    Val2 = iua_encoder:encode_common_message_header(Src2),
    ?assertEqual({Src2, <<>>}, iua_decoder:decode_common_message_header(Val2)).

-endif.
