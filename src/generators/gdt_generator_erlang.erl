-module(gdt_generator_erlang).
-compile(export_all).

-include_lib("gdt_tree.hrl").
-import(gdt_utils, [join/2, to_list/1]).
-import(gdt_tree,  [type_name/1]).
-import(gdt_utils, [log/3]).

all(Tree) ->
    hrl(Tree) ++
    erl(Tree).

%% Erlang header file.
hrl(#g_tree{module=#g_module{name=Name, exprs=Exprs}, imports=Imports}) ->
    Module = module_name(Name),
    FileName = to_list(Module) ++ ".hrl",
    FileData = 
        ifdef_guard(Module, join("\n", [
            [import(Import) || Import <- Imports],
            [expr(Expr) || Expr <- Exprs]
        ])),
    [{FileName, FileData}].

ifdef_guard(Module, Body) ->
    [
        "-ifndef(_", Module, "_included).\n",
        "-define(_", Module, "_included, yeah).\n",
        "\n",
        Body,
        "\n",
        "-endif.\n"
    ].

import(#g_import{name=Name}) ->
    include(module_name(Name)).

include(Module) ->
    ["-include(\"", Module, ".hrl\").\n"].

expr(Expr = #g_data{}) -> data(Expr);
expr(Expr            ) -> log(warn, "Skipping unknown expr ~p", [Expr]), [].

data(#g_data{name=Name, variants=Variants}) ->
    [
        [data_record(N, R) || #g_variant{constructor=N, value=(R=#g_variant_record{})} <- Variants],
        "-type(", type(Name), " :: ", join(" | ", [data_variant(V) || V <- Variants]), ").\n"
    ].

data_variant(#g_variant{constructor=Name, value=#g_variant_record{}}) ->
    ["#", atoml(type_name(Name)), "{}"]; %% TODO
data_variant(#g_variant{constructor=Name, value=#g_variant_simple{fields=Fields}}) ->
    ["{", atoml(Name), ", ", join(", ", [type(T) || T <- Fields]), "}"];
data_variant(#g_variant{constructor=Name, value=#g_variant_single{}}) ->
    [atoml(type_name(Name))].

data_record(Name, #g_variant_record{fields=Fields}) ->
    ["-record(", atoml(type_name(Name)), ", {", join(",", [data_record_field(F) || F <- Fields]), "\n}).\n"].

data_record_field(#g_field{name=Name, type=Type}) ->
    ["\n\t", atoml(Name), " :: ", type(Type)].



%% Erlang implementation file.
erl(#g_tree{module=#g_module{name=Name, exprs=Exprs}}) ->
    Module = atoml(Name),
    FileName = binary_to_list(Module) ++ ".erl",
    FileData = [
        "-module(", Module, ").\n",
        include(Module),
        "\n",
        "%% API\n"
        "-export([encode/2, decode/2]).\n",
        "\n",
        "%% internal\n"
        "-export([encode_/2, decode_/2]).\n",
        "\n",
        "encode(Type, Value) ->\n",
        "\tmsgpack:pack(encode_(Type, Value)).\n",
        "\n",
        "decode(Type, Data) ->\n",
        "\t{ok, Obj} = msgpack:unpack(Data),\n"
        "\tdecode_(Type, Obj).\n",
        "\n",
        encoders(Exprs),
        "\n",
        decoders(Exprs),
        "\n",
        "-compile({inline,[find_field/2]}).\n",
        "find_field(Field, {PL}) -> proplists:get_value(Field, PL).\n",
        "\n",
        "-compile({inline,[find_variant_tag/2]}).\n",
        "find_variant_tag(Field, {PL}) -> proplists:get_value(Field, PL);\n",
        "find_variant_tag(_, V) -> V.\n",
        "\n",
        "-compile({inline,[required/1]}).\n",
        "required(undefined) -> error(required_field_is_absent);\n",
        "required(V) -> V.\n",
        "\n",
        "-compile({inline,[optional/1]}).\n",
        "optional(V) -> V.\n",
        "\n"
    ],
    [{FileName, FileData}].


%% encoding
encoders(Exprs) ->
    [
        [encode_type(Expr) || Expr <- Exprs], "\n",
        "encode_(string, String) when is_list(String) ->\n"
        "\tlist_to_binary(String);\n"
        "encode_(string, String) when is_binary(String) ->\n"
        "\tString;\n"
        "encode_(boolean, Boolean) when (Boolean =:= true) or (Boolean =:= false) ->\n"
        "\tBoolean;\n"
        "encode_(integer, Integer) when is_integer(Integer) ->\n"
        "\tInteger;\n"
        "encode_(float, Float) when is_float(Float) ->\n"
        "\tFloat;\n"
        "encode_(binary, Binary) when is_binary(Binary) ->\n"
        "\tBinary;\n"
        "encode_({list, [A]}, List) when is_list(List) ->\n"
        "\t[encode_(A, E) || E <- List];\n"
        "encode_({map, [A, B]}, Map) when is_list(Map) ->\n"
        "\t[{encode_(A, K), encode_(B, V)} || {K, V} <- Map];\n"
        "\n"
        "encode_(Type, _) ->\n",
        "\terror({bad_type, Type}).\n"
    ].

encode_type(#g_data{name=Name, variants=Variants}) ->
    [
        "encode_(", type_tag(Name), ", ", type_var(Name), ") ->\n",
        "\tcase ", type_var(Name), " of\n",
        [encode_type_variant(V) || V <- Variants],
        "\t\t_ ->\n",
        "\t\t\terror({bad_data, ", type_var(Name), "})\n",
        "\tend;\n"
    ];
encode_type(V) ->
    log(warn, "Skipping unknown expr ~p", [V]),
    [].


encode_type_variant(#g_variant{constructor=Name, value=#g_variant_record{fields=Fields}}) ->
    [
        "\t\t{", atoml(Name), [[", ", type_var(FName)] || #g_field{name=FName} <- Fields], "} -> \n",
        "\t\t\t{[{0, ", binary(Name), "},\n",
        join(",\n", [ 
            ["\t\t\t\t{", binary(FName), ", ", to_list(FReq), "(encode_(", type_tag(FType), ", ", type_var(FName), "))}"]
            || #g_field{name=FName, requirement=FReq, type=FType} <- Fields
        ]), "\n",
        "\t\t\t]};\n"
    ];
encode_type_variant(#g_variant{constructor=Name, value=#g_variant_simple{fields=Fields}}) ->
    [
        "\t\t{", atoml(Name), [[", A", to_list(N)] || N <- lists:seq(1, length(Fields))], "} ->\n",
        "\t\t\t{[{0, ", binary(Name), "},\n",
        join(",\n", [
            ["\t\t\t\t{", to_list(N), ", required(encode_(", type_tag(lists:nth(N, Fields)), ", ", "A", to_list(N), "))}"]
            || N <- lists:seq(1, length(Fields))
        ]), "\n",
        "\t\t\t]};\n"
    ];
encode_type_variant(#g_variant{constructor=Name, value=#g_variant_single{}}) ->
    [
        "\t\t", atoml(Name), " ->\n",
        "\t\t\t", binary(Name), ";\n"
    ].


%% decoding
decoders(Exprs) ->
    [
        [decode_type(Expr) || Expr <- Exprs], "\n",
        "decode_(string, String) when is_binary(String) ->\n",
        "\tString;\n",
        "decode_(boolean, Boolean) when (Boolean =:= true) or (Boolean =:= false) ->\n",
        "\tBoolean;\n",
        "decode_(integer, Integer) when is_integer(Integer) ->\n",
        "\tInteger;\n",
        "decode_(float, Float) when is_float(Float) ->\n",
        "\tFloat;\n",
        "decode_(binary, Binary) when is_binary(Binary) ->\n",
        "\tBinary;\n",
        "decode_({list, [A]}, List) when is_list(List) ->\n",
        "\t[decode_(A, E) || E <- List];\n",
        "decode_({map, [A, B]}, Map) when is_list(Map) ->\n",
        "\t[{decode_(A, K), decode_(B, V)} || {K, V} <- Map];\n",
        "\n",
        "decode_(Type, Data) ->\n",
        "\terror({bad_type, Type, Data}).\n"
    ].

decode_type(#g_data{name=Name, variants=Variants}) ->
    [
        "decode_(", type_tag(Name), ", ", type_var(Name), ") ->\n",
        "\tcase find_variant_tag(0, ", type_var(Name), ") of\n",
        [decode_type_variant(Name, V) || V <- Variants],
        "\t\t_ ->\n",
        "\t\t\terror({bad_data, ", type_var(Name), "})\n",
        "\tend;\n"
    ];
decode_type(V) ->
    log(warn, "Skipping unknown expr ~p", [V]),
    [].

decode_type_variant(TypeName, #g_variant{constructor=Name, value=#g_variant_record{fields=Fields}}) ->
    [
        "\t\t", binary(Name), " ->\n",
        "\t\t\t#", atoml(Name), "{\n",
        join(",\n", [ 
            ["\t\t\t\t", atoml(FName), " = decode_(", type_tag(FType), ", ", to_list(FReq), "(find_field(", binary(FName), ", ", type_var(TypeName), ")))"]
            || #g_field{name=FName, requirement=FReq, type=FType} <- Fields
        ]), "\n",
        "\t\t\t};\n"
    ];
decode_type_variant(TypeName, #g_variant{constructor=Name, value=#g_variant_simple{fields=Fields}}) ->
    [
        "\t\t", binary(Name), " ->\n",
        "\t\t\t{", atoml(Name), ", \n",
        % decode_msgpack(string, required(find_field(1, Attr)))
        join(",\n", [
            ["\t\t\t\tdecode_(", type_tag(lists:nth(N, Fields)), ", required(find_field(", to_list(N) , ", ", type_var(TypeName), ")))"]
            || N <- lists:seq(1, length(Fields))
        ]), "\n",
        "\t\t\t};\n"
    ];
decode_type_variant(_, #g_variant{constructor=Name, value=#g_variant_single{}}) ->
    [
        "\t\t", binary(Name), " ->\n",
        "\t\t\t", atoml(Name), ";\n"
    ].


%% utils
%% представляет эрланг тип для g-типа
%% как для параметризованных, так и конкретных типов
type({type, <<"String">> }) -> "string() | binary()";
type({Type, Params}) -> [atoml(Type), "(", join(", ", [type(Param) || Param <- Params]), ")"];
type(TypeOrLabel) ->
    case {gdt_tree:is_label(TypeOrLabel), TypeOrLabel} of
        {true, Label} -> var(Label);
        {false, Type} -> [atoml(Type), "()"]
    end.

%% представляет тэг для g-типа, которые передаются в ф-цию сериализации
type_tag({Type, Params}) -> ["{", atoml(Type), ", [", join(", ", [type_tag(Param) || Param <- Params]), "]}"];
type_tag(TypeOrLabel) ->
    case {gdt_tree:is_label(TypeOrLabel), TypeOrLabel} of
        {true, Label} -> var(Label);
        {false, Type} -> atoml(Type)
    end.

type_var(Name) ->
    var(type_name(Name)).

%% представление базовых элементов эрланга (атом, стока, бинарь, "переменная")
%% атом с переводом первой буквы в lower-case
atoml      (Str) when is_binary(Str) -> gdt_utils:lower(Str).
% atom       (Str) when is_binary(Str) -> ["'", Str, "'"].
string     (Str) when is_binary(Str) -> ["\"", Str, "\""].
binary     (Str) when is_binary(Str) -> ["<<\"", Str, "\">>"].
var        (Str) when is_binary(Str) -> gdt_utils:upper(Str).
record_name(Str) when is_binary(Str) -> atoml(Str). 
module_name(Str) when is_binary(Str) -> atoml(Str). 

