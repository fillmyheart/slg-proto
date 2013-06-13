
-module(slg_proto_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  Acceptor = {
    conn_acceptor,
    {conn_acceptor, start_link, [self()]},
    transient,
    infinity,
    worker,
    [acceptor]
   },
  %% 玩家进程监督进程。
  ConnSup={
    conn_super,
    {supervisor, start_link, [{local, conn_super}, conn_super, []]},
    permanent,
    infinity,
    supervisor,
    []
   },
  {ok, { {one_for_one, 5, 10}, [Acceptor, ConnSup]} }.

