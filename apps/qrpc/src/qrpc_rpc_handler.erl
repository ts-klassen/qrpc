-module(qrpc_rpc_handler).
-behavior(cowboy_rest).

-export([init/2]).

init(Req0, State) ->
    io:format("~p~n", [Req0]),
    Req = cowboy_req:reply(403, #{
        <<"content-type">> => <<"text/html; charset=utf-8">>
    }, <<"<h1>403 Forbidden</h1>\n">>, Req0),
    {ok, Req, State}.

