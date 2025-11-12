-module(qrpc_dlog).

-export([
        open/1
      , log/2
      , alog/2
    ]).

-export_type([
        name/0
      , entry/0
    ]).

-type name() :: atom().

-type entry() :: term().

-spec open(name()) -> disk_log:open_ret().
open(Name) ->
    Config = qrpc_conf:get([dlog_config, Name], []),
    Dir = qrpc_conf:get(dlog_dir, "/opt/qrpc/var/log/dlog"),
    filelib:ensure_path(Dir),
    File = filename:join(Dir, atom_to_list(Name)),
    disk_log:open([{name, {?MODULE, Name}}, {file, File}|Config]).

-spec log(name(), entry()) -> ok.
log(Name, Entry) ->
    Res = case disk_log:log({?MODULE, Name}, Entry) of
        {error, no_such_log} ->
            case open(Name) of
                {ok, _} ->
                    ok;
                OpenFailed ->
                    logger:warning("dlog open for ~p failed with reason ~p", [Name, OpenFailed])
            end,
            disk_log:log({?MODULE, Name}, Entry);
        Other ->
            Other
    end,
    case Res of
        ok ->
            ok;
        _ ->
            logger:critical("dlog for ~p failed with reason ~p~n~p~n",
                [Name, Res, Entry])
    end.

-spec alog(name(), entry()) -> ok.
alog(Name, Entry) ->
    spawn(fun() ->
       log(Name, Entry)
    end),
    ok.

