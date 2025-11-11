%%%-------------------------------------------------------------------
%% @doc q_vvx public API
%% @end
%%%-------------------------------------------------------------------

-module(q_vvx_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    q_vvx_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
