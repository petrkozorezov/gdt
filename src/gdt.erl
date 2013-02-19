-module(gdt).

-include("gdt_tree.hrl").

-export([main/1]).
-export([log/2, log/3]).
-export([get_option/1, get_option/2]).


%% entry point
main(Args) ->
    OptSpecList =
    [
        {verbose      , $v       , "verbose"   , undefined         , "Verbose mode (debug will be printed)."},
        {quite        , $q       , "quite"     , undefined         , "Quite mode (only errros will be printed)."},
        {module       , $m       , "module"    , string            , "Additional erlang source files, which can be used as a generator and/or by a generator (by option for example)."},
        {dry_mode     , $d       , "dry_mode"  , undefined         , "Generate code to memory only (can be useful with verbose fo debugging)."},
        {output_dir   , $o       , "output_dir", {string, "."}     , "Output direcory."},
        {gdt_file     , undefined, undefined   , string            , "Input gdt file."},
        {generator    , undefined, undefined   , string            , "Output code generator, can be 'module' or 'module:function'."}
    ],
    try
        {Options, GeneratorArgs} = check_sucess(
            getopt:parse(OptSpecList, Args), 
            invalid_run_options
        ),
        set_log_level(
            proplists:get_value(quite  , Options, false),
            proplists:get_value(verbose, Options, false)
        ),

        File = proplists:get_value(gdt_file, Options),
        (File /= undefined) orelse throw(file_is_undefined),
        Generator = proplists:get_value(generator, Options),
        (Generator /= undefined) orelse throw(generator_is_undefined),

        Tree = parse_gdt_file(File),
        load_modules(Options),
        write_files(
            proplists:get_value(output_dir, Options),
            generate(Generator, GeneratorArgs, Tree),
            proplists:get_value(dry_mode, Options, false)
        )
    catch
        throw:Error -> print_error(Error, OptSpecList)
    end.

print_error(Error, OptSpecList) ->
    log(error, "~p", [Error]),
    getopt:usage(OptSpecList, ?MODULE_STRING, "[option_name=option_value] [...]").


%% gdt file parsing
parse_gdt_file(FileName) ->
    log(info, "Parsing source file ~s...", [FileName]),

    FileData = check_sucess(
        file:read_file(FileName),
        {gdt_file_reading_failed, FileName}
    ),
    FileDataString = binary_to_list(FileData),
    log(debug, "GDT file: ~n~s", [FileDataString]),

    Tokens = case gdt_scanner:string(FileDataString) of
        {ok, V, _} -> V;
        {error, {L0, _, Msg0}, _} ->
            throw({gdt_file_scanning_failed, L0, lists:flatten(gdt_scanner:format_error(Msg0))})
    end,
    log(debug, "GDT file tokens: ~n~p", [Tokens]),

    Tree = case gdt_parser:parse(Tokens) of
        {ok, V1} -> V1;
        {error, {L1, _, Msg1}} ->
            throw({gdt_file_parsing_failed, L1, lists:flatten(gdt_parser:format_error(Msg1))})
    end,
    log(debug, "GDT tree: ~n~p", [Tree]),
    Tree.


%% additional modules loading
load_modules(Options) ->
    Files = [File || {module, File} <- Options],
    [load_module(File) || File <- Files].

load_module(File) ->
    log(info, "Compiling module ~s...", [File]),
    {Module, Binary} = check_sucess(
        compile:file(File, [binary, return_errors, report_errors, report_warnings]),
        module_compilation_error
    ),
    case code:load_binary(Module, File, Binary) of
        {module, Module} -> ok;
        {error, Reason} -> throw({module_loading_error, File, Reason})
    end.


%% code generating
generate(Generator, Args, Tree) ->
    Optinos = parse_options(Args),
    log(debug, "Generator options is: ~n~p", [Optinos]),
    set_options(Optinos),
    {M, F} =
        case string:tokens(Generator, ":") of
            [Module] -> {list_to_atom(Module), all};
            [Module, Function] -> {list_to_atom(Module), list_to_atom(Function)}
        end,
    log(info, "Generating code using ~p:~p function...", [M,F]),
    M:F(Tree).

parse_options(Args) ->
    [parse_option(Arg) || Arg <- Args].

parse_option(Arg) ->
    case string:tokens(Arg, "=") of
        [Key, Value] -> {Key, Value};
        _ -> throw({invalid_generator_option, Arg})
    end.

set_options(Options) ->
    put(generator_options, Options).

get_option(Key) ->
    get_option(Key, undefined).

get_option(Key, Default) ->
    Options = get(generator_options),
    proplists:get_value(Key, Options, Default).


%% write output files
write_files(Path, Files, DryModeFlag) ->
    [write_file(filename:join(Path, Name), Data, DryModeFlag) || {Name, Data} <- Files].

write_file(Name, Data, DryModeFlag) ->
    log(debug, "Generated file: ~s~n~s", [Name, binary_to_list(iolist_to_binary(Data))]),
    case DryModeFlag of
        false ->
            log(info , "Writing generated file ~s...", [Name]),
            ok = check_sucess(
                file:write_file(Name, Data),
                {output_file_saving_failed, Name}
            );
        true ->
            ok
    end.


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
should_log(error, error) -> true;
should_log(_    , _    ) -> false.

log_prefix(debug) -> "DEBUG: ";
log_prefix(info ) -> "--> ";
log_prefix(error) -> "ERROR: ".


%% utils
check_sucess(ok, _) -> ok;
check_sucess({ok, V1}, _) -> V1;
check_sucess({ok, V1, V2}, _) -> {V1, V2};
check_sucess({ok, V1, V2, V3}, _) -> {V1, V2, V3};
check_sucess({error, R1}, Error) -> throw({Error, R1});
check_sucess({error, R1, R2}, Error) -> throw({Error, R1, R2});
check_sucess({error, R1, R2, R3}, Error) -> throw({Error, R1, R2, R3}).
