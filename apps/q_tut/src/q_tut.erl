-module(q_tut).

-export([
        hello_world/1
      , hello_admin/1
    ]).

hello_world(_) ->
    #{ payload => <<"hello world">> }.

hello_admin(Rpc) ->
    #{ payload => Rpc:get([payload]) }.


