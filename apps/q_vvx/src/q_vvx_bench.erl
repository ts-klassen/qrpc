-module(q_vvx_bench).

-export([synthesize/1]).

synthesize(#{scenario := ScenarioPath, result := ResultPath}) ->
    Items = load_scenario(ScenarioPath),
    StartMs = erlang:monotonic_time(millisecond),
    Ref = make_ref(),
    Parent = self(),
    spawn_items(Items, StartMs, Ref, Parent),
    Results = collect_results(length(Items), Ref, #{}),
    ok = file:write_file(ResultPath, iolist_to_binary(json:encode(Results))),
    Results.

spawn_items([], _StartMs, _Ref, _Parent) ->
    ok;
spawn_items(Items, StartMs, Ref, Parent) ->
    Indexed = lists:zip(lists:seq(1, length(Items)), Items),
    lists:foreach(fun({Index, Item}) ->
        spawn(fun() ->
            run_item(StartMs, Ref, Parent, Index, Item)
        end)
    end, Indexed).

run_item(StartMs, Ref, Parent, Index, #{
        <<"offset_ms">> := Offset,
        <<"style_id">> := StyleId,
        <<"text">> := Text
    }) ->
    Delay = erlang:max(0, Offset - elapsed_ms(StartMs)),
    timer:sleep(Delay),
    Response = synthesize_request(StyleId, Text),
    #{payload := #{event_namespace := EventNamespace, event_id := EventId}} = Response,
    EventTimes = wait_for_events(EventNamespace, EventId, StartMs),
    RequestEndMs = maps:get(<<"finish_ms">>, EventTimes, elapsed_ms(StartMs)),
    Parent ! {Ref, Index, #{
        <<"offset_ms">> => Offset,
        <<"style_id">> => StyleId,
        <<"text">> => Text,
        <<"worker_id">> => maps:get(<<"worker_id">>, EventTimes, null),
        <<"syn_start_ms">> => maps:get(<<"syn_start_ms">>, EventTimes, null),
        <<"syn_end_ms">> => maps:get(<<"syn_end_ms">>, EventTimes, null),
        <<"finish_ms">> => RequestEndMs
    }}.

collect_results(0, _Ref, Acc) ->
    lists:map(fun(Index) ->
        maps:get(Index, Acc)
    end, lists:seq(1, map_size(Acc)));
collect_results(Count, Ref, Acc) ->
    receive
        {Ref, Index, Result} ->
            collect_results(Count - 1, Ref, maps:put(Index, Result, Acc))
    end.

elapsed_ms(StartMs) ->
    erlang:monotonic_time(millisecond) - StartMs.

synthesize_request(StyleId, Text) ->
    Req = #{
        metadata => #{
            client_name => <<"q_vvx_bench">>,
            module => q_vvx_core,
            function => synthesize,
            arity => 1
        },
        payload => #{
            <<"style_id">> => StyleId,
            <<"text">> => Text
        }
    },
    qrpc:rpc(Req).

wait_for_events(Namespace, EventId, StartMs) ->
    Targets = #{
        finish => EventId,
        syn_start => synth_event_id(EventId, <<"synth_start">>),
        syn_end => synth_event_id(EventId, <<"synth_end">>)
    },
    wait_for_events(Namespace, Targets, StartMs, #{}).

wait_for_events(Namespace, Targets, StartMs, Acc) ->
    case maps:is_key(<<"finish_ms">>, Acc) of
        true ->
            Acc;
        false ->
            Events = fetch_events(Namespace),
            NowMs = elapsed_ms(StartMs),
            Acc1 = lists:foldl(fun(Event, Acc0) ->
                record_event(Event, Targets, NowMs, Acc0)
            end, Acc, Events),
            wait_for_events(Namespace, Targets, StartMs, Acc1)
    end.

fetch_events(Namespace) ->
    try qrpc_event:fetch(#{
        payload => #{
            namespace => Namespace,
            timeout => 60000
        }
    }) of
        #{payload := Events} ->
            Events
    catch
        _:_ ->
            timer:sleep(1000),
            fetch_events(Namespace)
    end.

record_event(Event, Targets, NowMs, Acc) ->
    EventId = maps:get(id, Event),
    FinishId = maps:get(finish, Targets),
    SynStartId = maps:get(syn_start, Targets),
    SynEndId = maps:get(syn_end, Targets),
    Acc1 = case maps:get(worker_id, Event, undefined) of
        undefined ->
            Acc;
        WorkerId ->
            maybe_put(<<"worker_id">>, WorkerId, Acc)
    end,
    case EventId of
        _ when EventId =:= FinishId ->
            maybe_put(<<"finish_ms">>, NowMs, Acc1);
        _ when EventId =:= SynStartId ->
            maybe_put(<<"syn_start_ms">>, NowMs, Acc1);
        _ when EventId =:= SynEndId ->
            maybe_put(<<"syn_end_ms">>, NowMs, Acc1);
        _ ->
            Acc1
    end.

maybe_put(Key, Value, Acc) ->
    case maps:is_key(Key, Acc) of
        true ->
            Acc;
        false ->
            maps:put(Key, Value, Acc)
    end.

synth_event_id(EventId, Suffix) ->
    <<EventId/binary, ":", Suffix/binary>>.

load_scenario(Path) ->
    {ok, Bin} = file:read_file(Path),
    parse_scenario(Bin).

parse_scenario(Bin) ->
    json:decode(Bin).
