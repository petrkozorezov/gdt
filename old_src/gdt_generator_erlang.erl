-module(gdt_generator_erlang).

-export([generate/1]).



generate(Tree) ->
    ok = generate_hrl(Tree),
    ok = generate_erl(Tree).

generate_hrl({module, Name, Exprs}) ->
    Data = [
        "-ifndef(_", to_lower(Name), "_included).\n"
        "-define(_", to_lower(Name), "_included, yeah).\n",
        "\n",
        generate_datatypes(Exprs),
        "-endif.\n"
    ],
    ok = file:write_file("./" ++ to_lower(Name)++".hrl", Data).

generate_erl({module, Name, Exprs}) ->
    Data = [
        ["-module(", to_lower(Name), ").\n"],
        ["-include(\"", to_lower(Name), ".hrl\").\n"],
        "\n",
        "-export([encode/2, decode/2]).\n",
        "\n",
        generate_encoders(Exprs),
        "\n",
        generate_decoders(Exprs),
        []
    ],
    ok = file:write_file("./" ++ to_lower(Name) ++ ".erl", Data).

%% data types generation
generate_datatypes(Exprs) ->
    [
        [gen_type(Expr) || Expr <- Exprs],
        "\n",
        [gen_record(Expr) || Expr <- Exprs],
        "\n"
    ].

gen_type({data, [TypeName], Variants}) ->
    ["-type(", to_lower(TypeName), "() :: ",
        join(" | ", [gen_type_variant(V) || {variant, _, V} <- Variants]), ").\n"].

gen_type_variant({record, Name, _Fields}) ->
    ["#", to_lower(Name), "{}"];
gen_type_variant([Name]) ->
    to_lower(Name).

gen_record({data, _, Variants}) ->
    [gen_record_variant(V) || {variant, _, V} <- Variants].

gen_record_variant({record, Name, Fields}) ->
    ["-record(", to_lower(Name), ", {", join(",", [gen_record_field(F) || F <- Fields]), "\n}).\n"];
gen_record_variant([_]) ->
    [].

gen_record_field({field, _, _, Name, [{type_name, Type}]}) ->
    ["\n\t", to_lower(Name), " :: ", type(Type)].

type("String" ) -> "(string() | binary())";
type("Boolean") -> "boolean()";
type("Integer") -> "integer()";
type("Binary" ) -> "binary()";
type("Float"  ) -> "float()";
type("List"   ) -> "list()";
type(T        ) -> [to_lower(T), "()"].

%% encoders/decoders generation
-define(DEFAULT_ENCODERS,"
encode(string, String) when is_list(String) ->
\tlist_to_binary(String);
encode(string, String) when is_binary(String) ->
\tString;
encode(bool, Boolean) when (Boolean =:= true) or (Boolean =:= false) ->
\tBoolean;
encode(integer, Integer) when is_integer(Integer) ->
\tInteger;
encode(float, Float) when is_float(Float) ->
\tFloat;
encode(binary, Binary) when is_binary(Binary) ->
\tBinary;
encode({list, A}, List) when is_list(List) ->
\t[encode(A, E) || E <- List];
encode({map, A, B}, Map) when is_list(Map) ->
\t[{encode(A, K), encode(B, V)} || {K, V} <- Map];

encode(Type, _) -> error({bad_type, Type}).
").

generate_encoders(Exprs) ->
    [
        [generate_encoder(Expr) || Expr <- Exprs],
        ?DEFAULT_ENCODERS
    ].

generate_encoder({data, [TypeName], _Variants}) ->
    [
        "encode(", to_lower(TypeName), ", ", TypeName, ") ->\n",
        "\tcase ", TypeName, " of\n",
        "\t\t_ -> error(bad_data, ", TypeName, ")\n",
        "\tend;\n"
    ].

generate_decoders(Exprs) ->
    [].




%% utils
to_lower([H|T]) ->
    string:to_lower([H]) ++ T.

to_upper([H|T]) ->
    string:to_upper([H]) ++ T.

join(_    , [H|[]]) -> H;
join(Delim, [H|T] ) -> [H, Delim, join(Delim, T)].

