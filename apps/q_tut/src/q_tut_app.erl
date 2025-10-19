%%%-------------------------------------------------------------------
%% @doc q_tut public API
%% @end
%%%-------------------------------------------------------------------

-module(q_tut_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    q_tut_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
