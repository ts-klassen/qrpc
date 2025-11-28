-module(qrpc_counter_tests).
-include_lib("eunit/include/eunit.hrl").

parse_exp_ttl_test() ->
    TimeNow = erlang:system_time(second),
    Exp = qrpc_counter:parse_exp({ttl, 60}),
    ?assert(Exp >= TimeNow + 60),
    ?assert(Exp =< TimeNow + 61).

parse_exp_ttl_zero_test() ->
    TimeNow = erlang:system_time(second),
    Exp = qrpc_counter:parse_exp({ttl, 0}),
    ?assertEqual(TimeNow, Exp).

parse_exp_slot_secondly_test() ->
    TimeNow = erlang:system_time(second),
    Exp = qrpc_counter:parse_exp({slot, secondly}),
    ?assert(Exp > TimeNow),
    ?assert(Exp =< TimeNow + 1).

parse_exp_slot_minutely_test() ->
    TimeNow = erlang:system_time(second),
    Exp = qrpc_counter:parse_exp({slot, minutely}),
    ?assert(Exp > TimeNow),
    ?assert(Exp =< TimeNow + 60).

parse_exp_slot_hourly_test() ->
    TimeNow = erlang:system_time(second),
    Exp = qrpc_counter:parse_exp({slot, hourly}),
    ?assert(Exp > TimeNow),
    ?assert(Exp =< TimeNow + 3600).

parse_exp_slot_daily_test() ->
    TimeNow = erlang:system_time(second),
    Exp = qrpc_counter:parse_exp({slot, daily}),
    ?assert(Exp > TimeNow),
    ?assert(Exp =< TimeNow + 86400).

parse_exp_slot_integer_test() ->
    TimeNow = erlang:system_time(second),
    Exp = qrpc_counter:parse_exp({slot, 10}),
    ?assert(Exp > TimeNow),
    ?assert(Exp =< TimeNow + 10),
    ?assertEqual(0, Exp rem 10).

parse_exp_unix_test() ->
    ?assertEqual(1234567890, qrpc_counter:parse_exp(1234567890)),
    ?assertEqual(0, qrpc_counter:parse_exp(0)),
    ?assertEqual(-100, qrpc_counter:parse_exp(-100)).

parse_exp_badarg_test() ->
    ?assertError(badarg, qrpc_counter:parse_exp(invalid)),
    ?assertError(badarg, qrpc_counter:parse_exp({ttl, -1})),
    ?assertError(badarg, qrpc_counter:parse_exp({slot, 0})),
    ?assertError(badarg, qrpc_counter:parse_exp({slot, -1})).
