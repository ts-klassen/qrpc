%%%-------------------------------------------------------------------
%% @doc q_vvx public API
%% @end
%%%-------------------------------------------------------------------

-module(q_vvx_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    q_vvx_worker:stop_all(),
    case q_vvx_core:load_style_map() of
        {ok, _} ->
            q_vvx_sup:start_link();
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    q_vvx_worker:stop_all(),
    ok.

%% internal functions
