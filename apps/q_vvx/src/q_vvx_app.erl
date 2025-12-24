%%%-------------------------------------------------------------------
%% @doc q_vvx public API
%% @end
%%%-------------------------------------------------------------------

-module(q_vvx_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    q_vvx_worker:stop_all(),
    q_vvx_sup:start_link().

stop(_State) ->
    q_vvx_worker:stop_all(),
    ok.

%% internal functions
