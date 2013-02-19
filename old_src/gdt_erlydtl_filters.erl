-module(gdt_erlydtl_filters).

-export([
    lowfirst/1,
    % to_binary/1,
    make_type/1,
    make_types/1
]).

-define(LOG(S), io:format(S, [])).
-define(LOG(S, A), io:format(">>> "++S++"~n", A)).

-define(EXPORT_FUN(Name, Arg),
Name(A) when is_binary(A) ->
    io_list_to_binary(Name(binary_to_list(Arg)))
).

lowfirst(Str) when is_binary(Str) ->
     = binary_to_list(Str),
    list_to_binary().

lowfirst_([H|T]) ->
    [string:to_lower([H]), T].

% to_binary(Str) when is_binary(Str) ->
%     "<<\"" ++ Str ++ "\">>".

make_type(P=[Type|Params]) when is_list(Type) ->
    ?LOG("~p", [P]),
    lowfirst(Type) ++ "(" ++ join(",", [make_type(Param) || Param <- Params]) ++ ")";

make_type([<<"List">>, Param]) ->
    "list(", make_type(Param), || Param <- Params]) ++ ")";
make_type(Type) ->
    lowfirst(Type).

make_types(Types) ->
    [make_type(Type) || Type <- Types].

%% locals
join(_    , [H|[]]) -> H;
join(Delim, [H|T] ) -> [H, Delim, join(Delim, T)].
