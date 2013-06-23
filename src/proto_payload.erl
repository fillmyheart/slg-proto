%%% ==================================================================
%%% @author:zhuoyikang
%%% @doc 基础类型协议解析
%%% @end
%%% ==================================================================

-module(proto_payload).
-compile(export_all).

-include("proto_record.hrl").

encode_integer(Int) -> <<Int:32/signed-big-integer>>.
decode_integer(<<Integer:32/signed-big-integer, Data/binary>>) ->
  {Integer, Data}.

%% 解析short
encode_short(Short) -> <<Short:16/signed-big-integer>>.
decode_short(<<Short:16/signed-big-integer, Data/binary>>) ->
  {Short, Data}.

%% 解析short
encode_char(Short) -> <<Short:8/signed-big-integer>>.
decode_char(<<Short:8/signed-big-integer, Data/binary>>) ->
  {Short, Data}.

%% 解析short
encode_uinteger(Short) -> <<Short:32/unsigned-big-integer>>.
decode_uinteger(<<Short:32/unsigned-big-integer, Data/binary>>) ->
  {Short, Data}.

%% 解析short
encode_ushort(Short) -> <<Short:16/unsigned-big-integer>>.
decode_ushort(<<Short:16/unsigned-big-integer, Data/binary>>) ->
  {Short, Data}.

%% 解析short
encode_uchar(Short) -> <<Short:8/unsigned-big-integer>>.
decode_uchar(<<Short:8/unsigned-big-integer, Data/binary>>) ->
  {Short, Data}.

%% 解析float
encode_float(Float) when is_float(Float) ->
  <<Float:32/float>>.
decode_float(<<Float:32/float, Data/binary>>)  ->
  {Float, Data}.

encode_string(<<BinS/binary>>) ->
  L = byte_size(BinS),
  list_to_binary([<<L:16>>, BinS]).
decode_string(<<Length:16/unsigned-big-integer,Data/binary>>) ->
  {StringData, StringLeftData} = split_binary(Data,Length),
  {StringData, StringLeftData}.

%% 解析short
encode_boolean(Bool) when is_boolean(Bool) ->
  case Bool of
    true -> <<1:8>>;
    false -> <<0:8>>
  end.
decode_boolean(<<BoolVal:8, Data/binary>>) ->
  case BoolVal of
    0 -> {false, Data};
    _ -> {true, Data}
  end.

%% 解析pkid
encode_pkid(<<Pkid/binary>>)  ->
  encode_string(Pkid);
encode_pkid(Pkid) when is_integer(Pkid) ->
  L = list_to_binary(integer_to_list(Pkid)),
  encode_string(L).

decode_pkid(<<Length:16/unsigned-big-integer, Data/binary>>) ->
  {StringData, StringLeftData} = split_binary(Data,Length),
  String = binary_to_list(StringData),
  {list_to_integer(String), StringLeftData}.


%% 解析数组编码
encode_array_item([],_Fun) ->
  [];
encode_array_item([H|T],Fun) ->
  [proto_payload:Fun(H) | encode_array_item(T,Fun)].

encode_array(Array,Fun) when is_list(Array) ->
  List = encode_array_item(Array,Fun),
  ListData = list_to_binary(List),
  ListLen = length(List),
  list_to_binary([<<ListLen:16/unsigned-big-integer>>, ListData]).

decode_array_item(0, <<Data/binary>>, _Fun) ->
  [Data];
decode_array_item(N, <<Data/binary>>, Fun) ->
  {Item, ItemDataLeft} = proto_payload:Fun(Data),
  [Item | decode_array_item(N-1, ItemDataLeft, Fun)].

decode_array(<<ArrayLen:16/unsigned-big-integer, Data/binary>>, Fun) ->
  ArrayItem = decode_array_item(ArrayLen, Data, Fun),
  Length = length(ArrayItem),
  {Array, [ArrayDataLeft]} = lists:split(Length-1, ArrayItem),
  {Array, ArrayDataLeft}.

-include("proto_indian.hrl").
