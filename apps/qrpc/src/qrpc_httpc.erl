-module(qrpc_httpc).

-include_lib("qrpc/include/qrpc.hrl").

-export([
        request/1
    ]).

-export_type([
        method/0
      , request/0
      , response/0
    ]).

-type method() :: head
                | get
                | put
                | patch
                | post
                | trace
                | options
                | delete
                .

-type request() :: #{
        method => method()
      , url := klsn:binstr()
      , header => maps:map(klsn:binstr(), klsn:binstr())
      , content_type => klsn:binstr()
      , body => klsn:binstr()
        %% Options from httpc: We only add what we use.
        %% Not every existing options.
        %% arg 3. HttpOptions
      , timeout => erlang:timeout()
      , connect_timeout => erlang:timeout()
        %% arg 4. Options
      , headers_as_is => boolean()
    }.

-type response() :: #{
        version := klsn:binstr()
      , status := integer()
      , phrase := klsn:binstr()
      , header := maps:map(klsn:binstr(), klsn:binstr())
      , body := klsn:binstr()
    }.

-spec request_rule() -> qrpc_sanitizer:rule().
request_rule() ->
    #{
        method => {o, {atom, [
            head, get, put, patch, post, trace, options, delete
        ]}}
      , url => {r, binstr}
      , header => {o, {map, binstr, binstr}}
      , content_type => {o, binstr}
      , body => {o, binstr}
      , timeout => {o, timeout}
      , connect_timeout => {o, timeout}
      , headers_as_is => {o, boolean}
    }.

-spec request(request()) -> response().
request(Req0) ->
    Req = qrpc_sanitizer:normalize(server, request_rule(), Req0),
    Method = maps:get(method, Req, get),
    Url = maps:get(url, Req),
    Headers = lists:map(fun({K,V}) ->
        {binary_to_list(K), V}
    end, maps:to_list(maps:get(header, Req, #{}))),
    RequestTuple = case Req of
        #{body := Body} ->
            ContentType = binary_to_list(maps:get(content_type, Req, <<"application/octet-stream">>)),
            {Url, Headers, ContentType, Body};
        _ ->
            {Url, Headers}
    end,
    Timeout = maps:get(timeout, Req, 60000),
    CTimeout = maps:get(connect_timeout, Req, Timeout),
    HttpOpt = [
        {timeout, Timeout}
      , {connect_timeout, CTimeout}
    ],
    Opt = [
        {body_format, binary}
      , {headers_as_is, maps:get(headers_as_is, Req, false)}
    ],
    try httpc:request(Method, RequestTuple, HttpOpt, Opt) of
        {ok, {{ResV, ResS, ResP}, ResH, ResB}} ->
            parse_response_ok(#{
                version => ResV
              , status => ResS
              , phrase => ResP
              , header => ResH
              , body => ResB
            }, Req);
        {error, HttpcError} ->
            parse_response_error(HttpcError, Req);
        Res ->
            ?QRPC_ERROR(#{
                id => [qrpc, httpc, request, unexpected]
              , fault_source => server
              , message => <<"Unexpected error on http request">>                                                             
              , message_ja => <<"http リクエスト中に不明なエラーがー発生しました"/utf8>>                                                
              , detail => #{
                    req => Req
                  , res => Res
                }
              , is_known => false
              , is_retryable => false
              , version => 1
            })
    catch
        Class:Reason:Stack ->
            ?QRPC_ERROR(#{
                id => [qrpc, httpc, request, unexpected]
              , fault_source => server
              , message => <<"Unexpected error on http request">>                                                             
              , message_ja => <<"http リクエスト中に不明なエラーがー発生しました"/utf8>>                                                
              , detail => #{
                    req => Req
                }
              , is_known => false
              , is_retryable => false
              , class => Class
              , reason => Reason
              , stacktrace => Stack
              , version => 1
            })
    end.

parse_response_ok(Res0, _Req) ->
    Res = qrpc_sanitizer:normalize(external, #{
        version => {r, to_binstr}
      , status => {r, integer}
      , phrase => {r, to_binstr}
      , header => {r, {list, {tuple, [to_binstr, to_binstr]}}}
      , body => {r, binstr}
    }, Res0),
    qrpc_sanitizer:normalize(server, #{
        version => {r, binstr}
      , status => {r, integer}
      , phrase => {r, binstr}
      , header => {r, {map, binstr, binstr}}
      , body => {r, binstr}
    }, Res#{
        header := maps:from_list(maps:get(header, Res))
    }).

parse_response_error(UnknownError, Req) ->
    ?QRPC_ERROR(#{
        id => [qrpc, httpc, request, unexpected]
      , fault_source => server
      , message => <<"Unexpected error on http request">>                                                             
      , message_ja => <<"http リクエスト中に不明なエラーがー発生しました"/utf8>>                                                
      , detail => #{
            req => Req
          , res => {error, UnknownError}
        }
      , is_known => false
      , is_retryable => false
      , version => 1
    }).

