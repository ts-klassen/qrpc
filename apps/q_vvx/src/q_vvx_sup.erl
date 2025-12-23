%%%-------------------------------------------------------------------
%% @doc q_vvx top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(q_vvx_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    ChildSpecs = [
        #{
            id => q_vvx_worker_sup
          , start => {q_vvx_worker_sup, start_link, []}
          , restart => permanent
          , type => supervisor
        }
    ],
    SupFlags = #{
        strategy => one_for_one,
        intensity => length(ChildSpecs) + 1,
        period => 1
    },
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
