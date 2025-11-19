-module(qrpc_sanitizer_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("qrpc/include/qrpc.hrl").

any_test() ->
    Rule = any,
    Valid = [],
    ?assertEqual(true, qrpc_sanitizer:validate(Rule, Valid)),
    ?assertEqual(Valid, qrpc_sanitizer:normalize(Rule, Valid)).

integer_test() ->
    Rule = integer,
    Valid = 42,
    Invalid = 3.14,
    ?assertEqual(true, qrpc_sanitizer:validate(Rule, Valid)),
    ?assertEqual(false, qrpc_sanitizer:validate(Rule, Invalid)),
    ?assertEqual(Valid, qrpc_sanitizer:normalize(Rule, Valid)),
    ?assertEqual({invalid, Rule, Invalid}, invalid_reason(Rule, Invalid)).

to_integer_test() ->
    Rule = to_integer,
    ?assertEqual(42, qrpc_sanitizer:normalize(Rule, <<"42">>)),
    ?assertEqual({invalid, Rule, <<"a">>}, invalid_reason(Rule, <<"a">>)),
    ?assertEqual(42, qrpc_sanitizer:normalize(Rule, "42")),
    ?assertEqual({invalid, Rule, "a"}, invalid_reason(Rule, "a")).

float_test() ->
    Rule = float,
    Valid = 3.14,
    Invalid = 42,
    ?assertEqual(true, qrpc_sanitizer:validate(Rule, Valid)),
    ?assertEqual(false, qrpc_sanitizer:validate(Rule, Invalid)),
    ?assertEqual(Valid, qrpc_sanitizer:normalize(Rule, Valid)),
    ?assertEqual({invalid, Rule, Invalid}, invalid_reason(Rule, Invalid)).

to_float_test() ->
    Rule = to_float,
    ?assertEqual(3.14, qrpc_sanitizer:normalize(Rule, <<"3.14">>)),
    ?assertEqual({invalid, Rule, <<"a">>}, invalid_reason(Rule, <<"a">>)),
    ?assertEqual(3.14, qrpc_sanitizer:normalize(Rule, "3.14")),
    ?assertEqual({invalid, Rule, "a"}, invalid_reason(Rule, "a")).

number_test() ->
    Rule = number,
    Valid = 3.14,
    Invalid = <<"42">>,
    ?assertEqual(true, qrpc_sanitizer:validate(Rule, Valid)),
    ?assertEqual(false, qrpc_sanitizer:validate(Rule, Invalid)),
    ?assertEqual(Valid, qrpc_sanitizer:normalize(Rule, Valid)),
    ?assertEqual({invalid, Rule, Invalid}, invalid_reason(Rule, Invalid)).

to_number_test() ->
    Rule = to_number,
    ?assertEqual(42, qrpc_sanitizer:normalize(Rule, <<"42">>)),
    ?assertEqual(3.14, qrpc_sanitizer:normalize(Rule, <<"3.14">>)),
    ?assertEqual({invalid, Rule, <<"a">>}, invalid_reason(Rule, <<"a">>)),
    ?assertEqual(42, qrpc_sanitizer:normalize(Rule, "42")),
    ?assertEqual(3.14, qrpc_sanitizer:normalize(Rule, "3.14")),
    ?assertEqual({invalid, Rule, "a"}, invalid_reason(Rule, "a")),
    ?assertEqual(42, qrpc_sanitizer:normalize(Rule, 42)),
    ?assertEqual(3.14, qrpc_sanitizer:normalize(Rule, 3.14)).

binstr_test() ->
    Rule = binstr,
    Valid = <<"hello world">>,
    Invalid = "hello world",
    ?assertEqual(true, qrpc_sanitizer:validate(Rule, Valid)),
    ?assertEqual(false, qrpc_sanitizer:validate(Rule, Invalid)),
    ?assertEqual(Valid, qrpc_sanitizer:normalize(Rule, Valid)),
    ?assertEqual({invalid, Rule, Invalid}, invalid_reason(Rule, Invalid)).

to_binstr_test() ->
    Rule = to_binstr,
    ?assertEqual(<<"hello">>, qrpc_sanitizer:normalize(Rule, <<"hello">>)),
    ?assertEqual(<<"hello">>, qrpc_sanitizer:normalize(Rule, "hello")),
    ?assertEqual(<<"atom">>, qrpc_sanitizer:normalize(Rule, atom)),
    ?assertEqual(<<"42">>, qrpc_sanitizer:normalize(Rule, 42)),
    ?assertMatch(<<"3.1", _/binary>>, qrpc_sanitizer:normalize(Rule, 3.14)),
    ?assertEqual({invalid, Rule, {1,2,3}}, invalid_reason(Rule, {1,2,3})),
    ?assertEqual({invalid, Rule, ['not', a, string]}, invalid_reason(Rule, ['not', a, string])).

existing_atom_test() ->
    Rule = existing_atom,
    Valid = atom,
    Invalid = <<"binstr">>,
    ?assertEqual(true, qrpc_sanitizer:validate(Rule, Valid)),
    ?assertEqual(false, qrpc_sanitizer:validate(Rule, Invalid)),
    ?assertEqual(Valid, qrpc_sanitizer:normalize(Rule, Valid)),
    ?assertEqual({invalid, Rule, Invalid}, invalid_reason(Rule, Invalid)).

to_existing_atom_test() ->
    Rule = to_existing_atom,
    NonExisting = crypto:strong_rand_bytes(16),
    NonExistingList = binary_to_list(NonExisting),
    ?assertEqual(atom, qrpc_sanitizer:normalize(Rule, atom)),
    ?assertEqual(atom, qrpc_sanitizer:normalize(Rule, <<"atom">>)),
    ?assertEqual(atom, qrpc_sanitizer:normalize(Rule, "atom")),
    ?assertEqual({invalid, Rule, 42}, invalid_reason(Rule, 42)),
    ?assertEqual({invalid, Rule, NonExisting}, invalid_reason(Rule, NonExisting)),
    ?assertEqual({invalid, Rule, NonExistingList}, invalid_reason(Rule, NonExistingList)).

atom_test() ->
    Rule = {atom, [atom1, atom2]},
    Valid = atom2,
    Invalid = atom3,
    ?assertEqual(true, qrpc_sanitizer:validate(Rule, Valid)),
    ?assertEqual(false, qrpc_sanitizer:validate(Rule, Invalid)),
    ?assertEqual(Valid, qrpc_sanitizer:normalize(Rule, Valid)),
    ?assertEqual({invalid, Rule, Invalid}, invalid_reason(Rule, Invalid)).

to_atom_test() ->
    Rule = {to_atom, [atom1, atom2]},
    Valid = atom2,
    Invalid = atom3,
    NonExisting = crypto:strong_rand_bytes(16),
    NonExistingList = binary_to_list(NonExisting),
    ?assertEqual(true, qrpc_sanitizer:validate(Rule, Valid)),
    ?assertEqual(false, qrpc_sanitizer:validate(Rule, Invalid)),
    ?assertEqual(Valid, qrpc_sanitizer:normalize(Rule, Valid)),
    ?assertEqual(Valid, qrpc_sanitizer:normalize(Rule, <<"atom2">>)),
    ?assertEqual(Valid, qrpc_sanitizer:normalize(Rule, "atom2")),
    ?assertEqual({invalid, Rule, Invalid}, invalid_reason(Rule, Invalid)),
    ?assertEqual({invalid, Rule, <<"atom3">>}, invalid_reason(Rule, <<"atom3">>)),
    ?assertEqual({invalid, Rule, "atom3"}, invalid_reason(Rule, "atom3")),
    ?assertEqual({invalid, Rule, NonExisting}, invalid_reason(Rule, NonExisting)),
    ?assertEqual({invalid, Rule, NonExistingList}, invalid_reason(Rule, NonExistingList)).

map_test() ->
    Rule = #{
        required_field => {required, integer}
      , r_field => {r, integer}
      , optional_field => {optional, integer}
      , o_field => {o, integer}
      , optional_missing_field => {optional, integer}
      , o_missing_field => {o, integer}
    },
    Valid = #{
        required_field => 1
      , <<"r_field">> => 2
      , optional_field => 3
      , <<"o_field">> => 4
    },
    Expected = #{
        required_field => 1
      , r_field => 2
      , optional_field => 3
      , o_field => 4
    },
    Invalid = #{
        required_field => 1
      % <<"r_field">> => 2
      , optional_field => 3
      , <<"o_field">> => 4
    },
    ?assertEqual(true, qrpc_sanitizer:validate(Rule, Valid)),
    ?assertEqual(false, qrpc_sanitizer:validate(Rule, Invalid)),
    ?assertEqual(Expected, qrpc_sanitizer:normalize(Rule, Valid)),
    ?assertEqual({missing_required_field, r_field, Invalid}, invalid_reason(Rule, Invalid)).

map_key_value_test() ->
    Rule = {map, integer, integer},
    Valid = #{
        1 => 2
      , 3 => 6
      , 4 => 8
    },
    InvalidKey = #{
        1 => 2
      , 3 => 6
      , <<"4">> => 8
    },
    InvalidValue = #{
        1 => 2
      , 3 => 6
      , 4 => <<"8">>
    },
    ?assertEqual(true, qrpc_sanitizer:validate(Rule, Valid)),
    ?assertEqual(false, qrpc_sanitizer:validate(Rule, InvalidKey)),
    ?assertEqual(false, qrpc_sanitizer:validate(Rule, InvalidValue)),
    ?assertEqual(Valid, qrpc_sanitizer:normalize(Rule, Valid)),
    ?assertEqual({invalid_map_key, {invalid, integer, <<"4">>}}, invalid_reason(Rule, InvalidKey)),
    ?assertEqual({invalid_map_value, 4, {invalid, integer, <<"8">>}}, invalid_reason(Rule, InvalidValue)).

list_test() ->
    Rule = {list, integer},
    Valid = [1, 2, 3],
    Invalid = [1, 2.3, 4],
    ?assertEqual(true, qrpc_sanitizer:validate(Rule, Valid)),
    ?assertEqual(false, qrpc_sanitizer:validate(Rule, Invalid)),
    ?assertEqual(Valid, qrpc_sanitizer:normalize(Rule, Valid)),
    ?assertEqual({invalid_list_element, 2, {invalid, integer, 2.3}}, invalid_reason(Rule, Invalid)).

tuple_test() ->
    Rule = {tuple, {integer, float}},
    Valid = {42, 3.14},
    Invalid = {42, 123},
    ?assertEqual(true, qrpc_sanitizer:validate(Rule, Valid)),
    ?assertEqual(false, qrpc_sanitizer:validate(Rule, Invalid)),
    ?assertEqual(Valid, qrpc_sanitizer:normalize(Rule, Valid)),
    ?assertEqual({invalid_tuple_element, 2, {invalid, float, 123}}, invalid_reason(Rule, Invalid)).

optnl_test() ->
    Rule = {optnl, integer},
    Valid = {value, 42},
    Invalid = {value, 3.14},
    ?assertEqual(true, qrpc_sanitizer:validate(Rule, Valid)),
    ?assertEqual(false, qrpc_sanitizer:validate(Rule, Invalid)),
    ?assertEqual(Valid, qrpc_sanitizer:normalize(Rule, Valid)),
    ?assertEqual({invalid_optnl_value, {invalid, integer, 3.14}}, invalid_reason(Rule, Invalid)).

map_key_collision_test() ->
    Rule = {map, to_integer, integer},

    %% 正常系: 文字列/バイナリの数値が整数キーに正規化される
    Valid = #{
        <<"1">> => 10
      , "2"      => 20
    },
    ?assertEqual(true, qrpc_sanitizer:validate(Rule, Valid)),
    ?assertEqual(#{1 => 10, 2 => 20}, qrpc_sanitizer:normalize(Rule, Valid)),

    %% 異常系1: 1 と <<"1">> がどちらも 1 に正規化されるので衝突する
    Colliding1 = #{
        1 => 10
      , <<"1">> => 20
    },
    ?assertEqual(false, qrpc_sanitizer:validate(Rule, Colliding1)),
    ?assertEqual({invalid_map_key, {duplicated_map_keys, [1]}}, invalid_reason(Rule, Colliding1)),

    %% 異常系2: 1/<<"1">> と 2/<<"2">> の双方が衝突するケース
    Colliding2 = #{
        1 => 10
      , <<"1">> => 20
      , 2 => 30
      , <<"2">> => 40
    },
    ?assertEqual(false, qrpc_sanitizer:validate(Rule, Colliding2)),
    {invalid_map_key, {duplicated_map_keys, Keys}} = invalid_reason(Rule, Colliding2),
    ?assertEqual([1, 2], lists:sort(Keys)).

timeout_test() ->
    Rule = timeout,
    ValidInt = 1000,
    ValidInf = infinity,
    InvalidNeg = -1,
    InvalidType = <<"1000">>,
    ?assertEqual(true, qrpc_sanitizer:validate(Rule, ValidInt)),
    ?assertEqual(true, qrpc_sanitizer:validate(Rule, ValidInf)),
    ?assertEqual(false, qrpc_sanitizer:validate(Rule, InvalidNeg)),
    ?assertEqual(false, qrpc_sanitizer:validate(Rule, InvalidType)),
    ?assertEqual(ValidInt, qrpc_sanitizer:normalize(Rule, ValidInt)),
    ?assertEqual(ValidInf, qrpc_sanitizer:normalize(Rule, ValidInf)),
    ?assertEqual({invalid, Rule, InvalidNeg}, invalid_reason(Rule, InvalidNeg)),
    ?assertEqual({invalid, Rule, InvalidType}, invalid_reason(Rule, InvalidType)).

boolean_test() ->
    Rule = boolean,
    ValidTrue = true,
    ValidFalse = false,
    Invalid = <<"true">>,
    ?assertEqual(true, qrpc_sanitizer:validate(Rule, ValidTrue)),
    ?assertEqual(true, qrpc_sanitizer:validate(Rule, ValidFalse)),
    ?assertEqual(false, qrpc_sanitizer:validate(Rule, Invalid)),
    ?assertEqual(ValidTrue, qrpc_sanitizer:normalize(Rule, ValidTrue)),
    ?assertEqual(ValidFalse, qrpc_sanitizer:normalize(Rule, ValidFalse)),
    ?assertEqual({invalid, Rule, Invalid}, invalid_reason(Rule, Invalid)).

invalid_reason(Rule, Value) ->
    try qrpc_sanitizer:normalize(Rule, Value) of
        _ ->
            error(unexpected_success)
    catch
        ?QRPC_CATCH(#{payload:=#{
            id := [qrpc, sanitizer, normalize, invalid]
          , detail := #{ reason := Reason }
        }}) ->
            Reason
    end.
