-module(gdtg_erlang).
-compile(export_all).

-include_lib("gdt_tree.hrl").

all(Tree) ->
    hrl(Tree) ++
    erl(Tree).

%% Erlang header file.
hrl(#g_tree{module=#g_module{name=Name, exprs=Exprs}, imports=Imports}) ->
    Module = atom(Name),
    FileName = binary_to_list(Module) ++ ".hrl",
    FileData = 
        ifdef_guard(Module, join("\n", [
            imports(Imports),
            datatypes(Exprs)
        ])),
    [{FileName, FileData}].

ifdef_guard(Module, Body) ->
    [
        "-ifndef(_", Module, "_included).\n",
        "-define(_", Module, "_included, yeah).\n",
        "\n",
        Body,
        "\n"
        "-endif.\n"
    ].

imports(Imports) ->
    [import(Import) || Import <- Imports].
import(#g_import{name=Name}) ->
    include(atom(Name)).

include(Module) ->
    ["-include(\"", Module, ".hrl\").\n"].

datatypes(Exprs) ->
    [[type_decl(Expr) || Expr <- Exprs]].

type_decl(#g_data{name=Name, variants=Variants}) ->
    [
        [record(N, R) || #g_variant{constructor=N, value=(R=#g_variant_record{})} <- Variants],
        "-type(", atom(Name), "() :: ", join(" | ", [type_decl_variant(V) || V <- Variants]), ").\n"
    ].

type_decl_variant(#g_variant{constructor=Name, value=#g_variant_record{}}) ->
    ["#", atom(Name), "{}"];
type_decl_variant(#g_variant{constructor=Name, value=#g_variant_simple{fields=Fields}}) ->
    ["{", atom(Name), ", ", join(", ", [type(T) || T <- Fields]), "}"];
type_decl_variant(#g_variant{constructor=Name, value=#g_variant_single{}}) ->
    [atom(Name)].

record(Name, #g_variant_record{fields=Fields}) ->
    ["-record(", atom(Name), ", {", join(",", [record_field(F) || F <- Fields]), "\n}).\n"].

record_field(#g_field{name=Name, type=Type}) ->
    ["\n\t", atom(Name), " :: ", type(Type)].

type(<<"String">> ) -> "string() | binary()";
type(<<"Boolean">>) -> "boolean()";
type(<<"Integer">>) -> "integer()";
type(<<"Binary">> ) -> "binary()";
type(<<"Float">>  ) -> "float()";
type([<<"List">>, Type]) -> ["list(", type(Type), ")"];
type([<<"Map">>, T1, T2]) -> ["list({", type(T1), ",", type(T2), "})"];
type(Type) -> [atom(Type), "()"].


%% Erlang implementation file.
erl(#g_tree{module=#g_module{name=Name, exprs=Exprs}}) ->
    Module = atom(Name),
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
        "encode_({list, A}, List) when is_list(List) ->\n"
        "\t[encode_(A, E) || E <- List];\n"
        "encode_({map, A, B}, Map) when is_list(Map) ->\n"
        "\t[{encode_(A, K), encode_(B, V)} || {K, V} <- Map];\n"
        "\n"
        "encode_(Type, _) ->\n",
        "\terror({bad_type, Type}).\n"
    ].

encode_type(#g_data{name=Name, variants=Variants}) ->
    [
        "encode_(", atom(Name), ", ", var(Name), ") ->\n",
        "\tcase ", var(Name), " of\n",
        [encode_type_variant(V) || V <- Variants],
        "\t_ ->\n",
        "\t\terror(bad_data, ", var(Name), ")\n",
        "\tend;\n"
    ].

encode_type_variant(#g_variant{constructor=Name, value=#g_variant_record{fields=Fields}}) ->
    [
        "\t\t{", atom(Name), [[", ", var(FName)] || #g_field{name=FName} <- Fields], "} -> \n",
        "\t\t\t{[{0, ", binary(Name), "},\n",
        join(",\n", [ 
            ["\t\t\t\t{", binary(FName), ", ", to_list(FReq), "(encode_(", type_tag(FType), ", ", var(FName), "))}"]
            || #g_field{name=FName, requirement=FReq, type=FType} <- Fields
        ]), "\n",
        "\t\t\t]};\n"
    ];
encode_type_variant(#g_variant{constructor=Name, value=#g_variant_simple{fields=Fields}}) ->
    [
        "\t\t{", atom(Name), [[", A", to_list(N)] || N <- lists:seq(1, length(Fields))], "} ->\n",
        "\t\t\t{[{0, ", binary(Name), "},\n",
        join(",\n", [
            ["\t\t\t\t{", to_list(N), ", required(encode_(", type_tag(lists:nth(N, Fields)), ", ", "A", to_list(N), "))}"]
            || N <- lists:seq(1, length(Fields))
        ]), "\n",
        "\t\t\t]};\n"
    ];
encode_type_variant(#g_variant{constructor=Name, value=#g_variant_single{}}) ->
    [
        "\t\t", atom(Name), " ->\n",
        "\t\t\t", binary(Name), ";\n"
    ].


%% decoding
decoders(Exprs) ->
    [
        [decode_type(Expr) || Expr <- Exprs], "\n",
        "decode_(string, String) when is_binary(String) ->\n",
        "\tString;\n",
        "decode_(bool, Boolean) when (Boolean =:= true) or (Boolean =:= false) ->\n",
        "\tBoolean;\n",
        "decode_(integer, Integer) when is_integer(Integer) ->\n",
        "\tInteger;\n",
        "decode_(float, Float) when is_float(Float) ->\n",
        "\tFloat;\n",
        "decode_(binary, Binary) when is_binary(Binary) ->\n",
        "\tBinary;\n",
        "decode_({list, A}, List) when is_list(List) ->\n",
        "\t[decode_(A, E) || E <- List];\n",
        "decode_({map, A, B}, Map) when is_list(Map) ->\n",
        "\t[{decode_(A, K), decode_(B, V)} || {K, V} <- Map];\n",
        "\n",
        "decode_(Type, Data) ->\n",
        "\terror({bad_type, Type, Data}).\n"
    ].

decode_type(#g_data{name=Name, variants=Variants}) ->
    [
        "decode_(", atom(Name), ", ", var(Name), ") ->\n",
        "\tcase find_variant_tag(0, ", Name, ") of\n",
        [decode_type_variant(Name, V) || V <- Variants],
        "\t_ ->\n",
        "\t\terror(bad_data, ", var(Name), ")\n",
        "\tend;\n"
    ].

decode_type_variant(TypeName, #g_variant{constructor=Name, value=#g_variant_record{fields=Fields}}) ->
    [
        "\t\t", binary(Name), " ->\n",
        "\t\t\t#", atom(Name), "{\n",
        join(",\n", [ 
            ["\t\t\t\t", atom(FName), " = decode_(", type_tag(FType), ", ", to_list(FReq), "(find_field(", binary(FName), ", ", TypeName, ")))"]
            || #g_field{name=FName, requirement=FReq, type=FType} <- Fields
        ]), "\n",
        "\t\t\t};\n"
    ];
decode_type_variant(TypeName, #g_variant{constructor=Name, value=#g_variant_simple{fields=Fields}}) ->
    [
        "\t\t", binary(Name), " ->\n",
        "\t\t\t{", atom(Name), ", \n",
        %decode_msgpack(string, required(find_field(1, Attr)))
        join(",\n", [
            ["\t\t\t\tdecode_(", type_tag(lists:nth(N, Fields)), ", required(find_field(", to_list(N) , ", ", TypeName, ")))"]
            || N <- lists:seq(1, length(Fields))
        ]), "\n",
        "\t\t\t};\n"
    ];
decode_type_variant(_, #g_variant{constructor=Name, value=#g_variant_single{}}) ->
    [
        "\t\t", binary(Name), " ->\n",
        "\t\t\t", atom(Name), ";\n"
    ].


%% utils
type_tag([T0, T1, T2]) -> ["{", atom(T0), ", ", type_tag(T1), ", ", type_tag(T2), "}"];
type_tag([T0, T1]) -> ["{", atom(T0), ", ", type_tag(T1), "}"];
type_tag(Type) -> atom(Type).

atom(Str)   -> lower(Str).
string(Str) -> ["\"", Str, "\""].
binary(Str) -> ["<<\"", Str, "\">>"].
var(Str)    -> upper(Str).

join(_    , [H|[]]) -> H;
join(Delim, [H|T] ) -> [H, Delim, join(Delim, T)].

to_list(A) when is_list(A)      -> A;
to_list(A) when is_atom(A)      -> atom_to_list(A);
to_list(A) when is_integer(A)   -> integer_to_list(A);
to_list(A) when is_float(A)     -> float_to_list(A);
to_list(A) when is_binary(A)    -> binary_to_list(A).

lower(Str) when is_binary(Str) ->
    list_to_binary(lower(binary_to_list(Str)));
lower([H|T]) ->
    string:to_lower([H]) ++ T.

upper(Str) when is_binary(Str) ->
    list_to_binary(upper(binary_to_list(Str)));
upper([H|T]) ->
    string:to_upper([H]) ++ T.
