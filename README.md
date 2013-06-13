刚开始学习erlang编程的时候，我对客户端与服务的通信协议一筹莫展，四处需找也无法找到一些具体的资料，但是如今这个部分我已经非常熟悉了，所以分享给大家，希望能减少大家学习和使用erlang痛苦.

# 连接处理

因为erlang使用了轻量级进程，所以连接处理代码比较简单，主要分为以下三个部分:

* conn_acceptor:监听某端口，在连接建立起时使用conn.erl开启一个新进程，并设置这个进程和新建的socket关联。
* conn.erl:连接通信实体进程，通过handle_info({tcp, _Socket, Bin}, State)匹配处理socket发来的数据。
* conn_super.erl：conn进程的监督者，用来启动conn而已。

本项目使用tcp长连接，主要从[hotwheels](git@github.com:tolbrino/hotwheels.git)移植过来，`hotwheels`是erlang中tcp连接处理的典范。

如果你使用过其他语言如C/C++，连接处理就不需要自己实现epoll或select之类的io多路复用，这些在erlang底层都帮你处理了，所以使用erlang编写这部分代码非常简单易懂。

有了传输层代码，下面介绍协议层，即传输数据包设计.

# 包设计

每一个数据包都由三部分组成： `包长度(2字节)+包类型(2字节)+包数据(剩余字节)`。

* 每个正常包的前两个字节都是包长，也就是你必须先收两个字节，确定之后的包的长度，等待完整包收完后再处理。
* 包类型为2个字节，最多有65535个不同的包，完全够用了。
* 包的剩余部分是包内容，是被序列化为2进制的数据结构，有很多同类型的产品(google protobuff ,apache thrift)，但是slg_proto足够简单，足够适合游戏。

erlang语言本身在建立socket的时候可以指定参数:

    inet:setopts(Socket, [{active, once}, {packet, 2}, binary]),

## 包长度

其中`{packet, 2}`制定后，对于所有的gen_tcp:send操作，erlang将自动在send的内容之间加上2个字节的内容长度，也就是协议中包长度部分。

## 包类型

一个完整的包收好后，将通过包类型来决定如何处理，包类型的描述我定义在了`proto/api.txt`文件中，每个包有以下5个属性需要配置:

* packet_type:包类型，必须是一个唯一的正数，10000以上为系统功能错误码，10000以下为逻辑错误码
* name:包名字，毕竟数字在代码里不是很友好
* payload:包内容，这说明包类型确定后，包内容也确定了
* desc:一句注释而已.
* module:请求处理模块，只对以_req为后缀的包名字的包有用，可以直接映射到处理模块。

## 包内容

数据包的内容我称其为payload，他们全部在`proto/protocal.txt`中被定义，包内容定义比较复杂，以下是基础类型，你可以通过基础类型组合成自定义类型：

* integer：数字类型
* float:浮点类型
* string:字符串类型，实际上处理为erlang的binary类型
* boolean：布尔类型，只占1个字节
* short：端整型
* pkid：主键类型，大整数，erlang层是数字，因为erlang支持大数，但是序列化为字符串.

有了基本类型，你可以自定义一个用户类型：

    pt_user=
    name string
    sex boolean
    ===

非常简单，你可以定义一个账号类型，它嵌套了用户类型:

    pt_account=
    user pt_user
    money integer
    ===

很多情况下我们需要数组，你可以这样定义一个含有数组的类型：

    pt_test=
    ids array integer
    users array pt_user
    ===

## 错误处理

游戏中有很多错误码，什么金钱不足，等级不足之类的，建议在`proto/error_code.txt`中列出你所有的错误码，大概如下：

    10000-ok-成功
    10001-inner_error-内部错误
    10002-bad_param-参数错误

在api.txt中定义了code_ack作为统一的错误接口，90%的错误提示可以使用该包完成，在代码中你使用起来像这样：

    conn:code_ack(timeout)

## 请求分发

协议中制定了一个测试模块，login_req的处理模块为：`conn_test`，你可以在conn_test.erl中找到与包类型名同名的处理函数:

    %% 用户登陆
    login_req(Pt = #pt_account{}) ->
    io:format("account ~p~n", [Pt]).

使用这种约定可直接完成请求处理代码的编写。

`conn_test`模块中已经有个登陆包的例子，你可以可以通过`make s`将slg_proto运行起来，它会打开一个本机的端口3001进行监听，然后通过`make e`新建一个erlang节点，执行`conn_t:start().`，它将对本机的3001端口发起tcp连接，连上之后发送协议登陆包，如果成功，屏幕会打印`account xxx`。

# 集成到其他项目.

但是本项目主要还是为了集成到其他项目中设计的[slg-server](http://42.121.105.8/slg-server)，你需要完成以下两步：

*在rebar.conf里添加依赖项*

    {slg_proto, ".*", {git, "git@42.121.105.8:slg-proto.git", {tag, "HEAD"}}}

*添加erl_opts的i参数，指定其其头文件路径*

    {erl_opts, [debug_info, fail_on_warning, {i, "deps/slg_proto/include/"}]}.

*修改make g*

在你项目的makefile里新增make g命令：

    g:
	cd deps/slg_proto/src && ruby ./proto_gen.rb $(shell pwd)/
	cp include/proto*  deps/slg_proto/include

# 使用规泛

使用将规范在slg-server中描述。
