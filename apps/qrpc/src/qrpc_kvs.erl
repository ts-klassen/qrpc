-module(qrpc_kvs).
-include_lib("qrpc/include/qrpc.hrl").

-define(GC_INTERVAL, 60).
-define(DEFAULT_DETS_PATH, "/opt/qrpc/var/dets/qrpc_kvs").

-behaviour(gen_server).

-export([
        insert/3
      , update/3
      , upsert/3
      , get/1
      , get/2
      , lookup/1
      , get_direct/1
      , get_direct/2
      , lookup_direct/1
    ]).

-export_type([
        key/0
      , value/0
      , exp/0
      , time_slot/0
    ]).

%% package private
-export([
        start_link/0
      , init/1
      , handle_call/3
      , handle_cast/2
      , terminate/2
      , gc/0
    ]).

-type key() :: term().
-type value() :: term().

-type time_slot() :: secondly
                   | minutely
                   | hourly
                   | daily
                   .

-type exp() :: integer() %% unix time (second)
             | {ttl, integer()} %% time to live (second)
             | {slot, integer() | time_slot()} %% second
             .

%% DETS entry format: {Key, Value, Exp}
%% keypos = 1

-spec insert(key(), exp(), value()) -> ok.
insert(Key, Exp, Value) ->
    case gen_server:call(?MODULE, {insert, Key, parse_exp(Exp), Value}) of
        ok ->
            ok;
        {error, exists} ->
            ?QRPC_ERROR(#{
                id => [qrpc, kvs, insert, exists]
              , fault_source => server
              , message => <<"Key already exists in KVS.">>
              , message_ja => <<"KVSにキーが既に存在します。"/utf8>>
              , is_known => true
              , is_retryable => false
              , detail => #{
                    key => Key
                }
              , version => 1
            })
    end.

-spec update(key(), exp(), value()) -> ok.
update(Key, Exp, Value) ->
    case gen_server:call(?MODULE, {update, Key, parse_exp(Exp), Value}) of
        ok ->
            ok;
        {error, enoent} ->
            ?QRPC_ERROR(#{
                id => [qrpc, kvs, update, enoent]
              , fault_source => server
              , message => <<"Key not found or expired in KVS.">>
              , message_ja => <<"KVSにキーが存在しないか、有効期限切れです。"/utf8>>
              , is_known => true
              , is_retryable => false
              , detail => #{
                    key => Key
                }
              , version => 1
            })
    end.

-spec upsert(key(), exp(), value()) -> ok.
upsert(Key, Exp, Value) ->
    gen_server:call(?MODULE, {upsert, Key, parse_exp(Exp), Value}).

-spec get(key()) -> value().
get(Key) ->
    case gen_server:call(?MODULE, {lookup, Key}) of
        {value, Value} ->
            Value;
        none ->
            ?QRPC_ERROR(#{
                id => [qrpc, kvs, get, enoent]
              , fault_source => server
              , message => <<"Key not found in KVS.">>
              , message_ja => <<"KVSにキーが存在しません。"/utf8>>
              , is_known => true
              , is_retryable => false
              , detail => #{
                    key => Key
                }
              , version => 1
            })
    end.

-spec get(key(), value()) -> value().
get(Key, Default) ->
    case gen_server:call(?MODULE, {lookup, Key}) of
        {value, Value} ->
            Value;
        none ->
            Default
    end.

-spec lookup(key()) -> klsn:optnl(value()).
lookup(Key) ->
    gen_server:call(?MODULE, {lookup, Key}).

-spec get_direct(key()) -> value().
get_direct(Key) ->
    case lookup_direct(Key) of
        {value, Value} ->
            Value;
        none ->
            ?QRPC_ERROR(#{
                id => [qrpc, kvs, get_direct, enoent]
              , fault_source => server
              , message => <<"Key not found in KVS.">>
              , message_ja => <<"KVSにキーが存在しません。"/utf8>>
              , is_known => true
              , is_retryable => false
              , detail => #{
                    key => Key
                }
              , version => 1
            })
    end.

-spec get_direct(key(), value()) -> value().
get_direct(Key, Default) ->
    case lookup_direct(Key) of
        {value, Value} ->
            Value;
        none ->
            Default
    end.

-spec lookup_direct(key()) -> klsn:optnl(value()).
lookup_direct(Key) ->
    TimeNow = erlang:system_time(second),
    case dets:lookup(?MODULE, Key) of
        [{_, Value, Exp}] when Exp > TimeNow ->
            {value, Value};
        _ ->
            none
    end.

%% package private

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Settings) ->
    DetsPath = qrpc_conf:get(kvs_dets_path, ?DEFAULT_DETS_PATH),
    ok = filelib:ensure_dir(DetsPath),
    {ok, ?MODULE} = dets:open_file(?MODULE, [
            {file, DetsPath}
          , {type, set}
          , {keypos, 1}
          , {repair, true}
          , {auto_save, 60000}
        ]),
    gc_now(),
    State = #{
        last_gc => erlang:system_time(second)
    },
    {ok, State}.

handle_call({insert, Key, Exp, Value}, _From, State) ->
    gc(),
    TimeNow = erlang:system_time(second),
    Result = case dets:lookup(?MODULE, Key) of
        [{_, _, CurrentExp}] when CurrentExp > TimeNow ->
            {error, exists};
        _ ->
            ok = dets:insert(?MODULE, {Key, Value, Exp}),
            ok
    end,
    {reply, Result, State};
handle_call({update, Key, Exp, Value}, _From, State) ->
    gc(),
    TimeNow = erlang:system_time(second),
    Result = case dets:lookup(?MODULE, Key) of
        [{_, _, CurrentExp}] when CurrentExp > TimeNow ->
            ok = dets:insert(?MODULE, {Key, Value, Exp}),
            ok;
        _ ->
            {error, enoent}
    end,
    {reply, Result, State};
handle_call({upsert, Key, Exp, Value}, _From, State) ->
    gc(),
    ok = dets:insert(?MODULE, {Key, Value, Exp}),
    {reply, ok, State};
handle_call({lookup, Key}, _From, State) ->
    gc(),
    TimeNow = erlang:system_time(second),
    Result = case dets:lookup(?MODULE, Key) of
        [{_, Value, Exp}] when Exp > TimeNow ->
            {value, Value};
        _ ->
            none
    end,
    {reply, Result, State};
handle_call(_, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(gc, State0) ->
    State = gc(State0),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    dets:close(?MODULE),
    ok.

gc() ->
    gen_server:cast(?MODULE, gc).

gc(State) ->
    TimeNow = erlang:system_time(second),
    gc(State, TimeNow).

gc(State = #{last_gc := LastGC}, TimeNow) when TimeNow - LastGC > ?GC_INTERVAL ->
    gc_now(),
    State#{last_gc := TimeNow};
gc(State, _) ->
    State.

gc_now() ->
    TimeNow = erlang:system_time(second),
    dets:select_delete(?MODULE, [{
        {'_', '_', '$1'},
        [{'=<', '$1', TimeNow}],
        [true]
    }]),
    ok.

%% TODO: Move parse_exp to a shared time utility module
%%       so qrpc_counter, qrpc_kvs, and qrpc_jwt can use it.
-spec parse_exp(exp()) -> integer().
parse_exp(Exp) ->
    qrpc_counter:parse_exp(Exp).
