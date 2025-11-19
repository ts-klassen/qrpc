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
              | to_integer
              | float
              | to_float
              | number
              | to_number
              | binstr
              | to_binstr
              | existing_atom
              | to_existing_atom
              | {atom, [atom()]}
              | {to_atom, [atom()]}
              | #{ atom() := {r|required | o|optional, rule()} }
              | {map, rule(), rule()}
              | {list, rule()}
              | {tuple, [rule()]} % Also allow {tuple, {rule(), ...}}
              | {optnl, rule()}
              | timeout
              | boolean
              .

-type reason() :: {invalid, rule(), term()}
                | {invalid_field, atom(), reason()}
                | {missing_required_field, atom(), term()}
                | {invalid_list_element, pos_integer(), reason()}
                | {invalid_tuple_element, pos_integer(), reason()}
                | {invalid_optnl_value, reason()}
                | {duplicated_map_keys, [term()]}
                | {invalid_map_key, reason()}
                | {invalid_map_value, term(), reason()}
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
parse(to_integer, Valid) when is_integer(Valid) ->
    Valid;
parse(to_integer, Binary) when is_binary(Binary) ->
    try binary_to_integer(Binary) catch
        _:_ ->
            throw({?MODULE, {invalid, to_integer, Binary}})
    end;
parse(to_integer, List) when is_list(List) ->
    try list_to_integer(List) catch
        _:_ ->
            throw({?MODULE, {invalid, to_integer, List}})
    end;
parse(float, Valid) when is_float(Valid) ->
    Valid;
parse(to_float, Valid) when is_float(Valid) ->
    Valid;
parse(to_float, Binary) when is_binary(Binary) ->
    try binary_to_float(Binary) catch
        _:_ ->
            throw({?MODULE, {invalid, to_float, Binary}})
    end;
parse(to_float, List) when is_list(List) ->
    try list_to_float(List) catch
        _:_ ->
            throw({?MODULE, {invalid, to_float, List}})
    end;
parse(number, Valid) when is_number(Valid) ->
    Valid;
parse(to_number, Valid) when is_number(Valid) ->
    Valid;
parse(to_number, Binary) when is_binary(Binary) ->
    try binary_to_integer(Binary) catch
        _:_ ->
            try binary_to_float(Binary) catch
                _:_ ->
                    throw({?MODULE, {invalid, to_number, Binary}})
            end
    end;
parse(to_number, List) when is_list(List) ->
    try list_to_integer(List) catch
        _:_ ->
            try list_to_float(List) catch
                _:_ ->
                    throw({?MODULE, {invalid, to_number, List}})
            end
    end;
parse(timeout, Valid) when is_integer(Valid), Valid >= 0 ->
    Valid;
parse(timeout, infinity) ->
    infinity;
parse(boolean, true) ->
    true;
parse(boolean, false) ->
    false;
parse(binstr, Valid) when is_binary(Valid) ->
    Valid;
parse(to_binstr, Valid) when is_binary(Valid) ->
    Valid;
parse(to_binstr, Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
parse(to_binstr, Int) when is_integer(Int) ->
    integer_to_binary(Int);
parse(to_binstr, Float) when is_float(Float) ->
    float_to_binary(Float);
parse(to_binstr, List) when is_list(List) ->
    try list_to_binary(List) catch
        _:_ ->
            throw({?MODULE, {invalid, to_binstr, List}})
    end;
parse(existing_atom, Valid) when is_atom(Valid) ->
    Valid;
parse(to_existing_atom, Valid) when is_atom(Valid) ->
    Valid;
parse(to_existing_atom, Binary) when is_binary(Binary) ->
    try binary_to_existing_atom(Binary, utf8) catch
        _:_ ->
            throw({?MODULE, {invalid, to_existing_atom, Binary}})
    end;
parse(to_existing_atom, List) when is_list(List) ->
    try binary_to_existing_atom(list_to_binary(List), utf8) catch
        _:_ ->
            throw({?MODULE, {invalid, to_existing_atom, List}})
    end;
parse({atom, AllowedAtoms}, Atom) when is_atom(Atom) ->
    case lists:member(Atom, AllowedAtoms) of
        true ->
            Atom;
        false ->
            throw({?MODULE, {invalid, {atom, AllowedAtoms}, Atom}})
    end;
parse({to_atom, AllowedAtoms}, Atom) when is_atom(Atom) ->
    case lists:member(Atom, AllowedAtoms) of
        true ->
            Atom;
        false ->
            throw({?MODULE, {invalid, {to_atom, AllowedAtoms}, Atom}})
    end;
parse({to_atom, AllowedAtoms}, Binary) when is_binary(Binary) ->
    try binary_to_existing_atom(Binary, utf8) of
        Atom ->
            case lists:member(Atom, AllowedAtoms) of
                true ->
                    Atom;
                false ->
                    throw({?MODULE, {invalid, {to_atom, AllowedAtoms}, Binary}})
            end
    catch
        _:_ ->
            throw({?MODULE, {invalid, {to_atom, AllowedAtoms}, Binary}})
    end;
parse({to_atom, AllowedAtoms}, List) when is_list(List) ->
    try binary_to_existing_atom(list_to_binary(List), utf8) of
        Atom ->
            case lists:member(Atom, AllowedAtoms) of
                true ->
                    Atom;
                false ->
                    throw({?MODULE, {invalid, {to_atom, AllowedAtoms}, List}})
            end
    catch
        _:_ ->
            throw({?MODULE, {invalid, {to_atom, AllowedAtoms}, List}})
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
parse({map, KeyRule, ValRule}, Map) when is_map(Map) ->
    {ResultMap, _SeenKeys, DuplicatedKeys} =
        lists:foldl(fun({Key0, Value0}, {AccMap, SeenKeys, DupKeys}) ->
            Key = try parse(KeyRule, Key0) catch
                throw:{?MODULE, Reason0} ->
                    throw({?MODULE, {invalid_map_key, Reason0}})
            end,
            case lists:member(Key, SeenKeys) orelse maps:is_key(Key, AccMap) of
                true ->
                    {AccMap, SeenKeys, [Key | DupKeys]};
                false ->
                    Value = try parse(ValRule, Value0) catch
                        throw:{?MODULE, Reason} ->
                            throw({?MODULE, {invalid_map_value, Key, Reason}})
                    end,
                    {maps:put(Key, Value, AccMap), [Key | SeenKeys], DupKeys}
            end
        end, {#{}, [], []}, maps:to_list(Map)),
    case DuplicatedKeys of
        [] ->
            ResultMap;
        _ ->
            throw({?MODULE, {invalid_map_key, {duplicated_map_keys, DuplicatedKeys}}})
    end;
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
