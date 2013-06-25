%%% ==================================================================
%%% @author zhuoyikang
%%% 网络端口进程监督者
%%% ==================================================================
-module(conn_super).
-behaviour(supervisor).

%% API
-export([init/1, start_player/1]).

-define(MAX_RESTART, 5000000).
-define(MAX_TIME, 60).

%% 开启一个连接服务进程.
start_player(Port) ->
  supervisor:start_child(?MODULE, [Port]).

heartbeat() ->
  All = supervisor:which_children(?MODULE),
  lists:foreach(fun({undefined, Pid, worker, []}) ->
    conn:heart_beat(Pid) end, All), 
  timer:apply_after(5000, io, format, ["~nHello World!~n", []]),
  heartbeat().


init([]) ->
  {ok,
   {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
    [{undefined, {conn, start_link, []}, temporary, 2000, worker, []}]
   }
  }.
