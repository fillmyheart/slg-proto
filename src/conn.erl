%% ===================================================================
%% 实现玩家进程逻辑
%% ===================================================================
-module(conn).
-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/1,
         send/2,
         stop/1]).

%% 连接进程状态
-record(state, {
          recv_cnt = 0,                  %% 读取到的数据个数
          socket,                        %% 与进程关联的 socket
          player_id,                     %% 玩家ID
          keep_still_times = 0           %% 超越次数
  	}).

-include("proto.hrl").

%% 启动gen_server
start_link(Socket) ->
  gen_server:start_link(?MODULE, [Socket], []).

%% 停止gen_server
stop(Ref) ->
  gen_server:cast(Ref, stop).

%% 检查连接的数据包
heart_beat(Pid) -> 
  gen_server:call(Pid, heart_beat).

init([Socket]) ->
  process_flag(trap_exit, true),
  inet:setopts(Socket, [{active, once}, {packet, 2}, binary]),
  State = #state{socket = Socket},
  erlang:put(socket, Socket),
  {ok, State}.

handle_cast(Cast, State) ->
  State1 = conn_config:exec(cast, [Cast, State]),
  {noreply, State1}.

%% 检查连接是否超时仍为有数据通信
handle_call(heart_beat, _From, State = #state{socket = Socket,
  recv_cnt = ReceiveBags, keep_still_times = LostTimes}) ->
  NowConnBags = inet:getstat(Socket, [recv_cnt]),
  case ReceiveBags == NowConnBags of
    true -> NewSate = #state{socket = Socket, keep_still_times = LostTimes + 1};
    false -> NewState = #state{socket = Socket, recv_cnt = NowConnBags, keep_still_times = 0}
  end,
  case LostTimes >= ?LOSTTIME of
    true -> gen_tcp:close(Socket),
      {stop, timeout, NewState};
    false -> {noreply, NewState}
  end.

handle_call(Call, From, State) ->
  State1 = conn_config:exec(cast, [Call, From, State]),
  {reply, ok, State1}.

handle_info({tcp_closed, Socket}, State)
  when Socket == State#state.socket ->
  {stop, normal, State};

handle_info({tcp_error, _Port, _Reason}, State) ->
  {noreply, State};


handle_info({tcp, Socket, Bin}, State) ->
  inet:setopts(Socket, [{active, once}, {packet, 2}, binary]),
  <<Type:?HWORD, RequestData/binary>> = Bin,
  ?M(Payload) = proto_decoder:decode(Type, RequestData),
  {M, F} = proto_route:route(Type),
  M:F(Payload),
  keep_alive_or_close(keep_alive, State);

handle_info(Info, State) ->
  conn_config:exec(info, [Info, State]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
terminate(Reason, State) ->
  conn_config:exec(quit, [Reason, State]),
  ok.

keep_alive_or_close(Keep, State) ->
  if
    Keep /= keep_alive -> gen_tcp:close(State#state.socket),
                          {stop, normal, State};
    true -> {noreply, State}
  end.

%% 一些副作用代码
send(TypeAtom, Paylod) ->
  Socket = erlang:get(socket),
  Bin = proto_encoder:encode(TypeAtom, Paylod),
  gen_tcp:send(Socket, Bin),
  ok.

  
  
