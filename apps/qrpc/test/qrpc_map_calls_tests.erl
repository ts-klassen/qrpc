-module(qrpc_map_calls_tests).
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    Map = #{
        qrpc_map_calls_tests => #{module => ?MODULE, pos => head}
      , other_field => any
      , sample_number => 10
    },
    ?assertEqual([Map], Map:sample_function()),
    ?assertEqual([Map, arg1], Map:sample_function(arg1)),
    ?assertEqual([Map, arg1, arg2], Map:sample_function(arg1, arg2)),
    ?assertEqual(11, Map:sample_add_function(1)),
    ?assertEqual(12, Map:sample_add_function(2)),
    ok.

chain_test() ->
    Map = #{
        qrpc_map_calls_tests => #{module => ?MODULE, pos => head}
      , other_field => any
      , sample_number => 10
    },
    Chain1 = Map:chain_add_function(1),
    ?assertEqual(Map#{sample_number := 11}, Chain1),
    Chain2 = (Map
                 :chain_add_function(1))
                 :chain_add_function(2),
    ?assertEqual(Map#{sample_number := 13}, Chain2),
    Chain3 = ((Map
                 :chain_add_function(1))
                 :chain_add_function(2))
                 :chain_add_function(3),
    ?assertEqual(Map#{sample_number := 16}, Chain3).

sample_function(Arg1) ->
    [Arg1].

sample_function(Arg1, Arg2) ->
    [Arg1, Arg2].

sample_function(Arg1, Arg2, Arg3) ->
    [Arg1, Arg2, Arg3].

sample_add_function(#{sample_number := A}, B) ->
    A + B.

chain_add_function(#{sample_number := A}=Map, B) ->
    Map#{sample_number => A + B}.

