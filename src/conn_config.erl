%% 连接参数配置，主要维护一张配置表.
-module(conn_config).
-compile([export_all]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/0, callback/1]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% 设置回调模块处理.
callback(Module) ->
  gen_server:call(?MODULE, {callback, Module}).

%% 执行.
exec(Fun, Param) ->
  case ets:lookup(?MODULE, callback) of
    [] -> do_nothing;
    [{callback, Module}] ->
      Export = Module:module_info(exports),
      case lists:member({Fun, length(Param)}, Export) of
        false -> do_nothing;
        true -> apply(Module, Fun, Param)
      end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% gen_server api

init([]) ->
  ets:new(?MODULE, [named_table, public, set]),
  {ok, {}}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(_, State) ->
  {noreply, State}.

%% 设置回调模块.
handle_call({callback, Module}, _From, State) ->
  ets:insert(?MODULE, {callback, Module}),
  {reply, ok, State};
handle_call(_Msg, _From, State) ->
  {reply, ok, State}.

handle_info(write, State) ->
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
terminate(_Reason, _State) ->
  ok.
