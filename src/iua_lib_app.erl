%%%----------------------------------------------------------------
%%% @author Anton N Ryabkov <anton.ryabkov@gmail.com>
%%% @doc
%%%
%%% @end
%%% @copyright 2011 Anton N Ryabkov
%%%----------------------------------------------------------------,
-module(iua_lib_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @private
-spec start(normal | {takeover, node()} | {failover, node()},
            any()) -> {ok, pid()} | {ok, pid(), State::any()} |
                      {error, Reason::any()}.
start(_StartType, _StartArgs) ->
    %TODO
    iua_encoder_tests:test(),
    iua_decoder_tests:test(),
    iua_mix_tests:test(),
    
    case iua_lib_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%% @private
-spec stop(State::any()) -> ok.
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================


