-module(qrpc_clog).

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

-type entry() :: klsn:binstr().

-spec open(name()) -> disk_log:open_ret().
open(Name) ->
    gc(Name),
    DateTime = calendar:local_time(),
    BaseDir = qrpc_conf:get(clog_dir, "/opt/qrpc/var/log/qrpc"),
    File = filename:join([BaseDir, atom_to_list(Name), hourly_log_file_name(DateTime)]),
    filelib:ensure_path(filename:dirname(File)),
    disk_log:open([
        {name, hourly_log_name(Name, DateTime)}
      , {file, File}
      , {format, external}
    ]).

-spec log(name(), entry()) -> ok.
log(Name, Entry0) ->
    Entry = klsn_binstr:from_any(Entry0),
    Res = case disk_log:blog(hourly_log_name(Name), Entry) of
        {error, no_such_log} ->
            open(Name),
            case disk_log:blog(hourly_log_name(Name), Entry) of
                {error, no_such_log} ->
                    %% Do it once more because it may be HH:00:00 problem.
                    case open(Name) of
                        {ok, _} ->
                            ok;
                        OpenFailed ->
                            logger:warning("clog open for ~p failed with reason ~p", [Name, OpenFailed])
                    end,
                    disk_log:blog(hourly_log_name(Name), Entry);
                Other0 ->
                    Other0
            end;
        Other ->
            Other
    end,
    case Res of
        ok ->
            ok;
        _ ->
            logger:critical("clog for ~p failed with reason ~p~n~ts~n",
                [Name, Res, Entry])
    end.

-spec alog(name(), entry()) -> ok.
alog(Name, Entry) ->
    spawn(fun() ->
       log(Name, Entry)
    end),
    ok.

-spec gc(name()) -> ok.
gc(Name) ->
    Now = erlang:system_time(second),
    Current = hourly_log_name(Name, calendar:system_time_to_local_time(Now, second)),
    Previous = hourly_log_name(Name, calendar:system_time_to_local_time(Now - 3600, second)),
    spawn(fun() ->
        lists:foreach(fun
            (Log) when Log =:= Current; Log =:= Previous ->
                keep;
            ({?MODULE, Name0, _Y, _M, _D, _H}=LogToClose) when Name0 =:= Name ->
                disk_log:close(LogToClose);
            (_) ->
                keep
        end, disk_log:all())
    end).

-spec hourly_log_name(name()) -> disk_log:log().
hourly_log_name(Name) ->
    hourly_log_name(Name, calendar:local_time()).

-spec hourly_log_name(name(), calendar:datetime()) -> disk_log:log().
hourly_log_name(Name, {{Year, Month, Day}, {Hour, _Minute, _Second}}) ->
    {?MODULE, Name, Year, Month, Day, Hour}.

-spec hourly_log_file_name(calendar:datetime()) -> string().
hourly_log_file_name({{Year, Month, Day}, {Hour, _Minute, _Second}}) ->
    lists:flatten(
        io_lib:format("~4..0B~2..0B/~2..0B-~2..0B.log",
                      [Year, Month, Day, Hour])).
