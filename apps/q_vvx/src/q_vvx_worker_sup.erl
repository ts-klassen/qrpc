-module(q_vvx_worker_sup).

-behaviour(supervisor).

-include_lib("qrpc/include/qrpc.hrl").

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    WorkerCount = worker_count(),
    ChildSpecs = worker_specs(WorkerCount),
    SupFlags = #{
        strategy => one_for_one,
        intensity => erlang:max(1, WorkerCount) + 1,
        period => 5
    },
    {ok, {SupFlags, ChildSpecs}}.

worker_count() ->
    case ?QRPC_SUBCONF_LOOKUP(worker_count) of
        {value, Count} when is_integer(Count), Count >= 0 ->
            Count;
        _ ->
            default_worker_count()
    end.

default_worker_count() ->
    case erlang:system_info(logical_processors_online) of
        Count when is_integer(Count), Count > 0 ->
            Count;
        _ ->
            case erlang:system_info(schedulers_online) of
                Count when is_integer(Count), Count > 0 ->
                    Count;
                _ ->
                    1
            end
    end.

worker_specs(Count) when is_integer(Count), Count > 0 ->
    lists:map(fun(Index) -> worker_spec(Index) end, lists:seq(1, Count));
worker_specs(_Count) ->
    [].

worker_spec(Index) ->
    #{
        id => {q_vvx_worker, Index}
      , start => {q_vvx_worker, start_link, [Index]}
      , restart => permanent
      , shutdown => 5000
      , type => worker
    }.
