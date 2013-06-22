%% 登陆和注册的sql流程必须在一个事务完成.
%% 用户直接使用角色名登陆，如果角色名不存在，服务器则创建一个。
%% 登陆的数据初始化必须在一个事务完成.
%%
-module(conn_test).

-export([login_req/1]).
-include("proto.hrl").

%% 用户登陆
login_req(Pt = #pt_account{}) ->
  io:format("account ~p~n", [Pt]).
