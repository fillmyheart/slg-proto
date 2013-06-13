%% ===================================================================
%% @author zhuoyikang
%% 监听端口建立新的连接进程.
%% ===================================================================
-module(conn_acceptor).
-export([start_link/1, start_acceptor/1]).

%% 启动进程.
start_link(Parent) when is_pid(Parent) ->
  proc_lib:start_link(?MODULE, start_acceptor, [Parent]).

%% 监听进程执行函数入口.
start_acceptor(Parent) ->
  register(acceptor, self()),
  case listen_port() of
    {ok, ListenSocket} ->
      %% 通知父级成功初始化.
      proc_lib:init_ack(Parent, {ok, self()}),
      acceptor_loop(ListenSocket);
    {error, Reason} ->
      throw({error, {listen, Reason}})
  end.

%% 执行监听.
listen_port() ->
  Options = [binary, {packet, 0}, {reuseaddr, true},
             {backlog, 1024}, {active, false}],
  gen_tcp:listen(4000, Options).

%% 监听循环.
acceptor_loop(ListenSocket) ->
  case (catch gen_tcp:accept(ListenSocket, 50000)) of
    {ok, Socket} -> handle_connection(Socket);
    {error, Reason} -> handle_error(Reason);
    {'EXIT', Reason} -> handle_error({exit, Reason})
  end,
  acceptor_loop(ListenSocket).

current_connections() ->
  [{specs, _}, {active, _}, {supervisors, _}, {workers, X}] =
    supervisor:count_children(conn_super),
  X.

handle_connection(Socket) ->
  case current_connections() >= 80000 of
    true -> gen_tcp:close(Socket);
    false -> do_handle_connection(Socket)
  end.

%% 新的客户端连接建立.
do_handle_connection(Socket) ->
  case conn_super:start_player(Socket) of
    {ok, PlayerPID} -> gen_tcp:controlling_process(Socket, PlayerPID);
    {error, Reason}->  io:format("error ~p~n", [Reason])
  end.

handle_error(timeout) -> ok;
%% Out of sockets...
handle_error({enfile, _}) -> timer:sleep(200);
%% Too many open files -> Out of sockets...
handle_error(emfile) -> timer:sleep(200);
handle_error(closed) ->
  exit(normal);
%% This will only happen when the client is terminated abnormaly
%% and is not a problem for the server, so we want
%% to terminate normal so that we can restart without any
%% error messages.
handle_error(econnreset) -> exit(normal);
handle_error(econnaborted) -> ok;
handle_error({exit, Reason}) ->
  String = lists:flatten(io_lib:format("Accept exit: ~p", [Reason])),
  accept_failed(String);
%% rest errors
handle_error(Reason) ->
  String = lists:flatten(io_lib:format("Accept error: ~p", [Reason])),
  accept_failed(String).
accept_failed(String) -> exit({accept_failed, String}).
