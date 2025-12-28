-module(q_vvx_worker).

-behaviour(gen_server).

-include_lib("qrpc/include/qrpc.hrl").

-export([
        start_link/1
      , stop_all/0
    ]).
-export([
        init/1
      , handle_call/3
      , handle_cast/2
      , handle_info/2
      , terminate/2
      , code_change/3
    ]).

start_link(Index) ->
    gen_server:start_link(?MODULE, #{index => Index}, []).

init(State0 = #{index := Index}) ->
    process_flag(trap_exit, true),
    Port = open_worker_port(Index),
    qrpc_clog:alog(?MODULE, io_lib:format("worker ~p started~n", [Index])),
    {ok, State0#{port => Port}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unsupported}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Port, {data, Data}}, State = #{port := Port, index := Index}) ->
    Prefix = io_lib:format("worker ~p | ", [Index]),
    qrpc_clog:alog(?MODULE, [Prefix, Data]),
    {noreply, State};
handle_info({Port, {exit_status, Status}}, State = #{port := Port, index := Index}) ->
    qrpc_clog:alog(?MODULE, io_lib:format("worker ~p exit status ~p~n", [Index, Status])),
    {stop, {port_exit_status, Status}, State};
handle_info({'EXIT', Port, Reason}, State = #{port := Port, index := Index}) ->
    qrpc_clog:alog(?MODULE, io_lib:format("worker ~p port exit ~p~n", [Index, Reason])),
    {stop, {port_exit, Reason}, State};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #{port := Port}) ->
    catch port_close(Port),
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

open_worker_port(Index) ->
    Executable = worker_executable(),
    open_port({spawn_executable, Executable}, port_options(Index)).

worker_executable() ->
    PrivDir = case code:priv_dir(q_vvx) of
        {error, Reason} ->
            error({missing_priv_dir, Reason});
        Dir when is_list(Dir) ->
            Dir;
        Dir when is_binary(Dir) ->
            binary_to_list(Dir)
    end,
    Path = filename:join([PrivDir, "bin", "q_vvx_worker"]),
    case filelib:is_regular(Path) of
        true ->
            Path;
        false ->
            error({missing_worker_binary, Path})
    end.

port_options(Index) ->
    [binary, exit_status, use_stdio, stderr_to_stdout, {args, worker_args(Index)}].

worker_args(Index) ->
    TagsJson = iolist_to_binary(json:encode(#{
        q_vvx_worker_index => klsn_binstr:from_any(Index)
      , qrpc_server_name => qrpc_conf:get(server_name)
      , q_vvx_vsn => case application:get_key(q_vvx, vsn) of
            {ok, Vsn} ->
                klsn_binstr:from_any(Vsn);
            undefined ->
                <<"0.0.0">>
        end
    })),
    Args0 = ["--tlgrf_tags", binary_to_list(TagsJson)],
    case ?QRPC_SUBCONF_GET(use_gpu, false) of
        true ->
            Args0 ++ ["--gpu"];
        _ ->
            Args0
    end.

stop_all() ->
    Port = open_port({spawn_executable, os:find_executable("pkill")}, [
        {args, ["-f", worker_executable()]},
        exit_status
    ]),
    wait_for_port_exit(Port),
    ok.

wait_for_port_exit(Port) ->
    receive
        {Port, {data, _Data}} ->
            wait_for_port_exit(Port);
        {Port, {exit_status, _Status}} ->
            ok;
        {'EXIT', Port, _Reason} ->
            ok
    end.
