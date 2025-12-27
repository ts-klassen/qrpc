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
    wait_for_event(EventNamespace, EventId),
    RequestEndMs = erlang:monotonic_time(millisecond),
    Parent ! {Ref, Index, #{
        <<"offset_ms">> => Offset,
        <<"style_id">> => StyleId,
        <<"text">> => Text,
        <<"finish_ms">> => RequestEndMs - StartMs
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

wait_for_event(Namespace, EventId) ->
    #{payload := Events} = qrpc_event:fetch(#{
        payload => #{
            namespace => Namespace,
            timeout => 3600000
        }
    }),
    case lists:search(fun(Event) ->
        maps:get(id, Event) =:= EventId
    end, Events) of
        {value, _Event} ->
            ok;
        false ->
            wait_for_event(Namespace, EventId)
    end.

load_scenario(Path) ->
    {ok, Bin} = file:read_file(Path),
    parse_scenario(Bin).

parse_scenario(Bin) ->
    json:decode(Bin).
