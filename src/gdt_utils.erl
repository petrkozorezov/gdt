-module(gdt_utils).
-compile(export_all).

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

is_upper(Str) ->
    upper(Str) == Str.

is_lower(Str) ->
    lower(Str) == Str.

check_sucess(ok, _) -> ok;
check_sucess({ok, V1}, _) -> V1;
check_sucess({ok, V1, V2}, _) -> {V1, V2};
check_sucess({ok, V1, V2, V3}, _) -> {V1, V2, V3};
check_sucess({error, R1}, Error) -> throw({Error, R1});
check_sucess({error, R1, R2}, Error) -> throw({Error, R1, R2});
check_sucess({error, R1, R2, R3}, Error) -> throw({Error, R1, R2, R3}).

%% logging
set_log_level(QuiteMode, Verbose) ->
    set_log_level(log_level(QuiteMode, Verbose)).

set_log_level(Level) when is_atom(Level) ->
    put(gdt_log_level, Level).

log(Level, Str) ->
    log(Level, Str, []).

log(Level, Str, Args) when is_atom(Level) ->
    LogLevel = get(gdt_log_level),
    case should_log(LogLevel, Level) of
        true ->
            io:format(log_prefix(Level) ++ Str ++ "~n", Args);
        false ->
            ok
    end.

%% log_level(QuiteMode, Verbose)
log_level(true , _    ) -> error;
log_level(false, false) -> info;
log_level(false, true ) -> debug.

should_log(undefined, _) -> false;
should_log(debug, _    ) -> true;
should_log(info , debug) -> false;
should_log(info , _    ) -> true;
should_log(warn , warn ) -> true;
should_log(warn , error) -> true;
should_log(error, error) -> true;
should_log(_    , _    ) -> false.

log_prefix(debug) -> "DEBUG: ";
log_prefix(info ) -> "--> ";
log_prefix(warn ) -> "WARN : ";
log_prefix(error) -> "ERROR: ".
