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

float_test() ->
    Rule = float,
    Valid = 3.14,
    Invalid = 42,
    ?assertEqual(true, qrpc_sanitizer:validate(Rule, Valid)),
    ?assertEqual(false, qrpc_sanitizer:validate(Rule, Invalid)),
    ?assertEqual(Valid, qrpc_sanitizer:normalize(Rule, Valid)),
    ?assertEqual({invalid, Rule, Invalid}, invalid_reason(Rule, Invalid)).

number_test() ->
    Rule = number,
    Valid = 3.14,
    Invalid = <<"42">>,
    ?assertEqual(true, qrpc_sanitizer:validate(Rule, Valid)),
    ?assertEqual(false, qrpc_sanitizer:validate(Rule, Invalid)),
    ?assertEqual(Valid, qrpc_sanitizer:normalize(Rule, Valid)),
    ?assertEqual({invalid, Rule, Invalid}, invalid_reason(Rule, Invalid)).

binstr_test() ->
    Rule = binstr,
    Valid = <<"hello world">>,
    Invalid = "hello world",
    ?assertEqual(true, qrpc_sanitizer:validate(Rule, Valid)),
    ?assertEqual(false, qrpc_sanitizer:validate(Rule, Invalid)),
    ?assertEqual(Valid, qrpc_sanitizer:normalize(Rule, Valid)),
    ?assertEqual({invalid, Rule, Invalid}, invalid_reason(Rule, Invalid)).

existing_atom_test() ->
    Rule = existing_atom,
    Valid = atom,
    Invalid = <<"binstr">>,
    ?assertEqual(true, qrpc_sanitizer:validate(Rule, Valid)),
    ?assertEqual(false, qrpc_sanitizer:validate(Rule, Invalid)),
    ?assertEqual(Valid, qrpc_sanitizer:normalize(Rule, Valid)),
    ?assertEqual({invalid, Rule, Invalid}, invalid_reason(Rule, Invalid)).

atom_test() ->
    Rule = {atom, [atom1, atom2]},
    Valid = atom2,
    Invalid = atom3,
    ?assertEqual(true, qrpc_sanitizer:validate(Rule, Valid)),
    ?assertEqual(false, qrpc_sanitizer:validate(Rule, Invalid)),
    ?assertEqual(Valid, qrpc_sanitizer:normalize(Rule, Valid)),
    ?assertEqual({invalid, Rule, Invalid}, invalid_reason(Rule, Invalid)).

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
