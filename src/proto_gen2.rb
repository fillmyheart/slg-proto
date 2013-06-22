# -*- coding: utf-8 -*-

# proto_dir  svn_dir code_dir
if ARGV.length >= 3
  $proto_path = ARGV[0]+"/"
  $svn_path = ARGV[1]+"/"
  $code_path = ARGV[2]+"/"
else
  $proto_path = "./proto/"
  $svn_path = "./proto/svn/"
  $code_path = "./proto/code/"
end

# 假定该路径下存放/proto，将过滤的旧协议内容生成在/proto/svn/路径
# 将头文件生成在include ，将自动代码生成在/proto/code.
puts("path is #{$proj_path} argv:#{ARGV}")

# 协议生成器
class String
  def caplize
    array = self.split("_").map do |a| a.capitalize end
    array.join("")
  end
end

# 各种类型默认值
$default_v = {
  "integer" => "0",
  "string" => "<<\"\">>",
  "pkid" => "0"
}

$default_t = {
  "integer" => "integer()",
  "string" => "binary()",
  "pkid" => "integer()"
}

def record_default(type, default = nil)
  if $default_v[type].nil?
    return "##{type}{} :: ##{type}{}"
  else
    return "#{default or $default_v[type]} :: #{$default_t[type]}"
  end
end

include_path = "include/"

# 解析错误码error_code.txt
def parse_error(path)
  file = open(path, "r")
  error_list = []
  file.lines.each do |line|
    line.strip!
    next if line[0,1]=="#"
    array=line.split("-")
    error_list << {"code" => array[0], "atom" => array[1], "desc" => array[2]}
  end
  error_list
end

# 解析api.txt，将其返回为api的列表.
def parse_api(path)
  file = open(path, "r")
  api = {}
  api_list = []
  file.lines.each do |line|
    line.strip!
    next if line[0,1]=="#"
    array=line.split(":")
    api[array[0]] = array[1]
    if line == ""
      api_list << api unless api["packet_type"].nil?
      api = {}
    end
  end
  api_list << api
end

# 解析proto.txt，返回为payload的列表
def parse_proto(path)
  file = open(path, "r")
  proto = {}
  proto["items"] = []
  proto["comments"] = []
  proto_list = []
  file.lines.each do |line|
    line.strip!
    if line[0,1]=="#"
      proto["comments"] << line
      next
    end
    if (s = /(\w+)=/.match(line))
      proto["name"] = s[1]
    elsif (s = /^(\w+)\s+(\w+)$/.match(line))
      proto["items"] << {"name" => s[1], "type" => s[2]}
    elsif (s = /^(\w+)\s+(\w+)\s+\$masked$/.match(line))
      proto["items"] << {"name" => s[1], "type" => s[2],
        "addtion" => "", "masked" => true}
    elsif (s = /^(\w+)\s+(\w+)\s+\$array$/.match(line))
      proto["items"] << {"name" => s[1], "type" => s[2],
        "addtion" => "array", "array" => true}
    elsif (s = /^(\w+)\s+(\w+)\s+\$default\s+([^\s]+)$/.match(line))
      proto["items"] << {"name" => s[1], "type" => s[2],
        "default" => s[3]}
    elsif (s = /^(\w+)\s+(\w+)\s+\$masked\s+\$default\s+([^\s]+)$/.match(line))
      proto["items"] << {"name" => s[1], "type" => s[2],
        "default" => s[3], "masked" => true}
    else
    end
    if line == "===" and proto["name"].nil? == false
      proto_list << proto
      proto = {}
      proto["items"] = []
      proto["comments"] = []
    end
  end
  proto_list
end

# 输出客户端可是识别的协议格式
def svn_api(api_list)
  file = open($svn_path + "api.txt", "w")
  api_list.each_with_index do |api, index|
    file.write("packet_type:#{api["packet_type"]}\n")
    file.write("name:#{api["name"]}\n")
    file.write("payload:#{api["payload"]}\n")
    file.write("desc:#{api["desc"]}\n")
    file.write("module:#{api["module"]}\n") unless api["module"].nil?
    file.write("\n") if index != api_list.length-1
  end
  file.close
end

# 输出客户端可识别的协议格式
def svn_proto(proto_list)
  file = open($svn_path + "protocal.txt", "w")
  proto_list.each_with_index do |proto, index|
    proto["comments"].each do |t|
      file.write(t + "\n")
    end
    file.write("#{proto["name"]}=\n")
    proto["items"].each do |t|
      unless t["addtion"].nil?
        file.write("#{t["name"]} #{t["addtion"]} #{t["type"]}\n")
      else
        file.write("#{t["name"]} #{t["type"]}\n")
      end
    end
    file.write("===\n")
    file.write("\n") if index != proto_list.length-1
  end
  file.close
end

# 生成record定义
def gen_record_hrl(proto_list)
  file = open($code_path + "proto_record.hrl", "w")
  proto_list.each do |proto|
    proto["comments"].each do |c|
      file.write("%% #{c.strip()}\n")
    end
    file.write("-record(#{proto["name"]}, {\n")
    proto["items"].each_with_index do |i, index|
      unless index == proto["items"].length - 1
        file.write("          #{i["name"]} = " +
                   "#{record_default(i["type"], i["default"])},\n")
      else
        file.write("          #{i["name"]} = " +
                   "#{record_default(i["type"], i["default"])}\n")
      end
    end
    file.write("         }).\n\n")
  end
  file.close()
end

# 生成encode函数.
def gen_indian_encode(file, proto)
  proto_name = proto["name"].caplize
  file.write("encode_#{proto["name"]}(#{proto_name} =" +
             " ##{proto["name"]}{}) ->\n")
  list = []
  proto["items"].each do |i|
    name = i["name"].caplize
    list << name
    if i["masked"]
      content = $default_v[i["type"]]
    else
      content = "#{proto_name}##{proto["name"]}.#{i["name"]}"
    end
    if i["array"]
      file.write("   #{name} = encode_array(#{content}, encode_#{i["type"]}),\n")
    else
      file.write("   #{name} = encode_#{i["type"]}(#{content}),\n")
    end
  end
  file.write("   list_to_binary([#{list.join(", ")}]).\n")
  file.write("\n\n")
end

def gen_indian_decode(file, proto)
  proto_name = proto["name"].caplize
  c = "<" + "<Data/binary>>"
  file.write("decode_#{proto["name"]}(#{c}) ->\n")
  list = []
  left_data = ""
  proto["items"].each_with_index do |i, index|
    if index == 0
      bin_data = "Data"
    else
      bin_data = left_data
    end
    name = i["name"].caplize
    left_data = name + "Left"
    if i["array"]
      file.write("  {#{name}, #{left_data}} = decode_array(#{bin_data}, decode_#{i["type"]}),\n")
    else
      file.write("  {#{name}, #{left_data}} = decode_#{i["type"]}(#{bin_data}),\n")
    end
    list << [name, i["name"]]
  end
  c = list.map do |l| "#{l[1]}=#{l[0]} "  end.join(",")
  file.write("  #{proto_name} = ##{proto["name"]}{#{c}},\n")
  file.write("  {#{proto_name}, #{left_data}}.\n")
  file.write("\n\n")
end

# 生成协议打解包代码
def gen_indian(proto_list)
  file = open($code_path + "proto_indian.hrl", "w")
  proto_list.each do |proto|
    file.write("%% #{proto["name"]}\n")
    gen_indian_encode(file, proto)
    gen_indian_decode(file, proto)
  end
  file.close
end

# 生成encoder
def gen_encoder(api_list)
  file = open($code_path + "proto_encoder.erl", "w")
  file.write("-module(proto_encoder).\n")
  file.write("-compile(export_all).\n\n")
  file.write("-include(\"proto_const.hrl\").\n")
  file.write("-include(\"proto_record.hrl\").\n\n\n")
  api_list.each_with_index do |api, index|
    name = api["name"].caplize
    file.write("%% #{api["desc"]}\n")
    # file.write("encode(#{api["name"]}, #{name}=##{api["payload"]}{}) ->\n")
    file.write("encode(#{api["name"]}, #{name}) ->\n")
    file.write("  Bin = proto_payload:encode_#{api["payload"]}(#{name}),\n")
    file.write("  list_to_binary([<<#{api["packet_type"]}:?HWORD>>, Bin]);\n")
    file.write("encode(#{api["packet_type"]}, #{name}) ->\n")
    file.write("  Bin = proto_payload:encode_#{api["payload"]}(#{name}),\n")
    if index == api_list.length-1
      file.write("  list_to_binary([<<#{api["packet_type"]}:?HWORD>>, Bin]).\n\n")
    else
      file.write("  list_to_binary([<<#{api["packet_type"]}:?HWORD>>, Bin]);\n\n")
    end

  end
end

# 生成encoder
def gen_decoder(api_list)
  file = open($code_path + "proto_decoder.erl", "w")
  file.write("-module(proto_decoder).\n")
  file.write("-compile(export_all).\n\n")
  file.write("-include(\"proto_const.hrl\").\n")
  file.write("-include(\"proto_record.hrl\").\n\n\n")
  api_list.each_with_index do |api, index|
    file.write("%% #{api["desc"]}\n")
    file.write("decode(#{api["name"]}, Data) ->\n")
    file.write("  proto_payload:decode_#{api["payload"]}(Data);\n\n")
    file.write("decode(#{api["packet_type"]}, Data) ->\n")
    if index == api_list.length-1
      file.write("  proto_payload:decode_#{api["payload"]}(Data).\n\n")
    else
      file.write("  proto_payload:decode_#{api["payload"]}(Data);\n\n")
    end
  end
end

# 生成路由代码
def gen_route(api_list)
  file = open($code_path + "proto_route.erl", "w")
  file.write("-module(proto_route).\n")
  file.write("-export([route/1]).\n\n")
  request_list = api_list.select {|api| /_req$/ =~ api["name"]}
  request_list.each_with_index do |x, index|
    if index == request_list.length - 1
      file.write("route(#{x["packet_type"]}) -> {#{x["module"]}, #{x["name"]}}.")
    else
      file.write("route(#{x["packet_type"]}) -> {#{x["module"]}, #{x["name"]}};")
    end
  end
  file.close()
end

# 生成错误码映射.
def gen_error(error_list)
  file = open($code_path + "proto_error.erl", "w")
  file.write("-module(proto_error).\n")
  file.write("-export([key/1]).\n\n")
  error_list.each_with_index do |error, index|
    file.write("key(#{error["code"]}) -> #{error["atom"]};\n")
    if index == error_list.length-1
      file.write("key(#{error["atom"]}) -> #{error["code"]}.\n")
    else
      file.write("key(#{error["atom"]}) -> #{error["code"]};\n")
    end
  end
  file.close()
end

api_list = parse_api($proto_path  + "api.txt")
proto_list = parse_proto($proto_path + "protocal.txt")
error_list = parse_error($proto_path + "error_code.txt")

svn_api(api_list)
svn_proto(proto_list)
gen_record_hrl(proto_list)
gen_indian(proto_list)
gen_encoder(api_list)
gen_decoder(api_list)
gen_route(api_list)
gen_error(error_list)

system("cp " + $proto_path + "error_code.txt " + $svn_path + "error_code.txt")
