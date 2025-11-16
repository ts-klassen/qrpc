-module(qrpc_sanitizer).

-include_lib("qrpc/include/qrpc.hrl").

-export([
        validate/2
      , normalize/2
      , normalize/3
    ]).

-export_type([
        rule/0
      , reason/0
    ]).

-type rule() :: any
              | integer
              | float
              | number
              | binstr
              | existing_atom
              | {atom, [atom()]}
              | #{ atom() := {r|required | o|optional, rule()} }
              | {list, rule()}
              | {tuple, [rule()]} % Also allow {tuple, {rule(), ...}}
              | {optnl, rule()}
              .

-type reason() :: {invalid, rule(), term()}
                | {invalid_field, atom(), reason()}
                | {missing_required_field, atom(), term()}
                | {invalid_list_element, pos_integer(), reason()}
                | {invalid_tuple_element, pos_integer(), reason()}
                | {invalid_optnl_value, reason()}
                .

-spec validate(rule(), term()) -> boolean().
validate(Rule, Input) ->
    try parse(Rule, Input) of
        _ ->
            true
    catch
        throw:{?MODULE, _} ->
            false;
        Class:Reason:Stack ->
            ?QRPC_ERROR(#{
                id => [qrpc, sanitizer, validate, catch_all]
              , fault_source => server
              , message => <<"Unexpected error on validation">>
              , message_ja => <<"入出力検証中にエラーが発生しました"/utf8>>
              , detail => #{
                    rule => Rule
                  , input => Input
                }
              , is_known => false
              , is_retryable => false
              , class => Class
              , reason => Reason
              , stacktrace => Stack
              , version => 1
            })
    end.

-spec normalize(rule(), term()) -> term().
normalize(Rule, Input) ->
    normalize(client, Rule, Input).

-spec normalize(client | server | external, rule(), term()) -> term().
normalize(Source, Rule, Input) ->
    try parse(Rule, Input) catch
        throw:{?MODULE, Reason} ->
            FaultSource = case Source of
                client -> client;
                server -> server;
                external -> external;
                _ -> server % Shouldn't happen, but can't let it fail here.
            end,
            {MsgEn, MsgJa, IsKnown} = case FaultSource of
                client ->
                    { <<"Invalid client input">>
                    , <<"入力値が不正です"/utf8>>
                    , true}; % I don't want client to flood my log
                server ->
                    { <<"Invalid server output">>
                    , <<"サーバーから不正な出力がありました"/utf8>>
                    , false};
                external ->
                    { <<"Invalid external value">>
                    , <<"外部から不正な値を受信しました"/utf8>>
                    , false}
            end,
            ?QRPC_ERROR(#{
                id => [qrpc, sanitizer, normalize, invalid]
              , fault_source => FaultSource
              , message => MsgEn
              , message_ja => MsgJa
              , detail => #{
                    rule => Rule
                  , input => Input
                  , source => Source
                  , reason => Reason
                }
              , is_known => IsKnown
              , is_retryable => false
              , version => 1
            });
        Class:Reason:Stack ->
            ?QRPC_ERROR(#{
                id => [qrpc, sanitizer, normalize, catch_all]
              , fault_source => server
              , message => <<"Unexpected error on validation">>
              , message_ja => <<"入出力検証中にエラーが発生しました"/utf8>>
              , detail => #{
                    rule => Rule
                  , input => Input
                }
              , is_known => false
              , is_retryable => false
              , class => Class
              , reason => Reason
              , stacktrace => Stack
              , version => 1
            })
    end.

parse(integer, Valid) when is_integer(Valid) ->
    Valid;
parse(float, Valid) when is_float(Valid) ->
    Valid;
parse(number, Valid) when is_number(Valid) ->
    Valid;
parse(binstr, Valid) when is_binary(Valid) ->
    Valid;
parse(existing_atom, Valid) when is_atom(Valid) ->
    Valid;
parse({atom, AllowedAtoms}, Atom) when is_atom(Atom) ->
    case lists:member(Atom, AllowedAtoms) of
        true ->
            Atom;
        false ->
            throw({?MODULE, {invalid, {atom, AllowedAtoms}, Atom}})
    end;
parse(Rule, Map) when is_map(Rule), is_map(Map) ->
    maps:filtermap(fun
        (Key, {R, ValRule}) when is_atom(Key), (R=:=r orelse R=:=required) ->
            case lookup_map(Key, Map) of
                {value, Value} ->
                   try {true, parse(ValRule, Value)} catch
                       throw:{?MODULE, Reason} ->
                           throw({?MODULE, {invalid_field, Key, Reason}})
                   end;
                none ->
                    throw({?MODULE, {missing_required_field, Key, Map}})
            end;
        (Key, {O, ValRule}) when is_atom(Key), (O=:=o orelse O=:=optional) ->
            case lookup_map(Key, Map) of
                {value, Value} ->
                   try {true, parse(ValRule, Value)} catch
                       throw:{?MODULE, Reason} ->
                           throw({?MODULE, {invalid_field, Key, Reason}})
                   end;
                none ->
                    false
            end;
        (_, _) ->
            throw({?MODULE, {invalid, Rule, Map}})
    end, Rule);
parse({list, Rule}, List) when is_list(List) ->
    lists:map(fun({I, Element}) ->
        try parse(Rule, Element) catch
            throw:{?MODULE, Reason} ->
                throw({?MODULE, {invalid_list_element, I, Reason}})
        end
    end, lists:zip(lists:seq(1, length(List)), List));
parse({tuple, Rules0}, Tuple) when (is_list(Rules0) orelse is_tuple(Rules0)), is_tuple(Tuple) ->
    Rules = case is_list(Rules0) of
        true -> Rules0;
        false -> tuple_to_list(Rules0)
    end,
    case length(Rules) =:= size(Tuple) of
        true -> ok;
        false -> throw({?MODULE, {invalid, {tuple, Rules0}, Tuple}})
    end,
    List = lists:zip(Rules, tuple_to_list(Tuple)),
    list_to_tuple(lists:map(fun({I, {Rule, Element}}) ->
        try parse(Rule, Element) catch
            throw:{?MODULE, Reason} ->
                throw({?MODULE, {invalid_tuple_element, I, Reason}})
        end
    end, lists:zip(lists:seq(1, length(List)), List)));
parse({optnl, Rule}, {value, Value}) ->
    try {value, parse(Rule, Value)} catch
        throw:{?MODULE, Reason} ->
            throw({?MODULE, {invalid_optnl_value, Reason}})
    end;
parse({optnl, _}, none) ->
    none;
parse(any, Term) ->
    Term;
parse(Rule, Invalid) ->
    throw({?MODULE, {invalid, Rule, Invalid}}).

lookup_map(Key, Map) ->
    case klsn_map:lookup([Key], Map) of
        none ->
            klsn_map:lookup([klsn_binstr:from_any(Key)], Map);
        Other ->
            Other
    end.
