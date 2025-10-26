-module(qrpc_counter).

-define(GC_INTERVAL, 60).

-behaviour(gen_server).

-export([
        add/3
      , set/3
      , get/1
      , parse_exp/1
    ]).

-export_type([
        id/0
      , exp/0
    ]).

%% package private
-export([
        start_link/0
      , init/1
      , handle_call/3
      , handle_cast/2
      , gc/0
    ]).

-type id() :: term().

-type time_slot() :: secondly
                   | minutely
                   | hourly
                   | daily
                   .

-type exp() :: integer() %% umix time (second)
             | {ttl, integer()} %% time to live (second)
             | {slot, integer() | time_slot()} %% second
             .

-spec add(id(), exp(), integer()) -> integer().
add(Id, Exp, Num) when is_integer(Num) ->
    gen_server:call(?MODULE, {add, Id, parse_exp(Exp), Num}).

-spec set(id(), exp(), integer()) -> integer().
set(Id, Exp, Num) when is_integer(Num) ->
    gen_server:call(?MODULE, {set, Id, parse_exp(Exp), Num}).

-spec get(id()) -> integer().
get(Id) ->
    gen_server:call(?MODULE, {get, Id}).

%% package private

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Settings) ->
    State = #{
        last_gc => erlang:system_time(second)
      , counter_map => #{}
    },
    {ok, State}.

handle_call({get, Id}, _From, State) ->
    gc(),
    TimeNow = erlang:system_time(second),
    Current = case klsn_map:lookup([counter_map, Id, exp], State) of
        {value, CurrentExp} when CurrentExp =< TimeNow ->
            0;
        _ ->
            klsn_map:get([counter_map, Id, count], State, 0)
    end,
    {reply, Current, State};
handle_call({add, Id, Exp, Num}, _From, State0) ->
    gc(),
    TimeNow = erlang:system_time(second),
    Current = case klsn_map:lookup([counter_map, Id, exp], State0) of
        {value, CurrentExp} when CurrentExp =< TimeNow ->
            0;
        _ ->
            klsn_map:get([counter_map, Id, count], State0, 0)
    end,
    Count = Current + Num,
    Map = #{
        count => Count
      , exp => Exp
    },
    State = klsn_map:upsert([counter_map, Id], Map, State0),
    {reply, Count, State};
handle_call({set, Id, Exp, Num}, _From, State0) ->
    gc(),
    Map = #{
        count => Num
      , exp => Exp
    },
    State = klsn_map:upsert([counter_map, Id], Map, State0),
    {reply, Num, State};
handle_call(_, _From, State) ->
    {reply, State, State}.

handle_cast(gc, State0) ->
    State = gc(State0),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

gc() ->
    gen_server:cast(?MODULE, gc).

gc(State) ->
    TimeNow = erlang:system_time(second),
    gc(State, TimeNow).

gc(State=#{last_gc:=LastGC}, TimeNow) when TimeNow - LastGC > ?GC_INTERVAL ->
    State#{
        last_gc := TimeNow
      , counter_map := maps:filter(fun
            (_, #{exp:=Exp}) when Exp =< TimeNow ->
                false;
            (_, _) ->
                true
        end, maps:get(counter_map, State))
    };
gc(State, _) ->
    State.

parse_exp({ttl, TTL}) when is_integer(TTL), TTL >= 0 ->
    erlang:system_time(second) + TTL;
parse_exp({slot, secondly}) ->
    parse_exp({slot, 1});
parse_exp({slot, minutely}) ->
    parse_exp({slot, 60});
parse_exp({slot, hourly}) ->
    parse_exp({slot, 3600});
parse_exp({slot, daily}) ->
    parse_exp({slot, 86400});
parse_exp({slot, Slot}) when is_integer(Slot), Slot > 0 ->
    (erlang:system_time(second) div Slot) * Slot + Slot;
parse_exp(EXP) when is_integer(EXP) ->
    EXP;
parse_exp(Arg1) ->
    erlang:error(badarg, [Arg1]).

