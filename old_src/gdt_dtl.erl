-module(gdt).

-include("gdt.hrl").

-export([main/1]).
-export([log/1, log/2]).

%% internal
-export([read_file/1]).

main(Args) ->
    OptSpecList =
    [
        {gdt_file     , undefined, undefined, string            , "Input gdt file"},
        {verbose      , $v       , "verbose", undefined         , "Verbose mode (debug will be printed)"},
        {quite        , $q       , "quite"  , undefined         , "Quite mode (only errros will be printed)"},
        {escape       , $e       , "escape" , undefined         , "Escaping mode, removing all \"\\.\" string from file apart of \"\\\\\""}
    ],
    try
        {Opts, InOutFileNames} = check_sucess(getopt:parse(OptSpecList, Args), invalid_run_options),
        set_log_level(proplists:get_value(quite, Opts, false), proplists:get_value(verbose, Opts, false)),
        set_escaping_mode(proplists:get_value(escape, Opts, false)),

        File = proplists:get_value(gdt_file, Opts),
        (File /= undefined) orelse throw(file_is_undefined),
        (length(InOutFileNames) rem 2 =:= 0) orelse throw(is_not_in_out_paired_filenames),

        Tree = prepare_for_rendering(parse_gdt_file(File)),
        log(debug, "Prepared tree: ~n~p", [Tree]),
        [Render(Tree) || Render <- make_dtl_renders(InOutFileNames)]
    catch
        throw:Error -> print_error(Error, OptSpecList)
    end.

print_error(Error, OptSpecList) ->
    log(error, "~p", [Error]),
    getopt:usage(OptSpecList, ?MODULE_STRING, "[dtl_template_file output_file] [...]").

%% main parsing
parse_gdt_file(FileName) ->
    log(info, "Parsing source file ~s...", [FileName]),

    FileData = check_sucess(file:read_file(FileName),           {gdt_file_reading_failed, FileName}),
    FileDataString = binary_to_list(FileData),
    log(debug, "File: ~n~s", [FileDataString]),

    {Tokens, _} = check_sucess(gdt_scanner:string(FileDataString), gdt_file_scanning_failed),
    log(debug, "Tokens: ~n~p", [Tokens]),

    Tree = check_sucess(gdt_parser:parse(Tokens),           gdt_file_parsing_failed),
    log(debug, "Tree: ~n~p", [Tree]),
    Tree.

%% templates and rendering
make_dtl_renders([]) ->
    [];
make_dtl_renders([In, Out | InOutFileNames]) ->
    Render = fun(Tree) ->
        log(info, "Rendering template ~s to ~s...", [In, Out]),
        CompileOpts = [
            {reader, {?MODULE, read_file}},
            {custom_filters_modules, [gdt_erlydtl_filters]},
            {binary_strings, true}
        ],
        ok   = check_sucess(erlydtl:compile(In, dtl_render, CompileOpts), dtl_compilation_failed),
        Data = check_sucess(dtl_render:render(Tree), {dtl_rendering_failed, In}),
        log(debug, "Render: ~n~s", [binary_to_list(iolist_to_binary(Data))]),
        ok   = check_sucess(file:write_file(Out, Data), {output_file_saving_failed, Out}),

        code:purge(dtl_render)
    end,
    [Render|make_dtl_renders(InOutFileNames)].

read_file(FileName) ->
    {ok, Data} = file:read_file(FileName),
    Data1 = escape(Data),
    log(debug, "Template: ~n~s", [Data1]),
    {ok, Data1}.

% hack!!!
escape(Data) ->
    case get(gdt_escape) of
        true ->
            Data1 = re:replace(Data, "\\\\","\\", [global]),
            Data2 = re:replace(Data1, "\\\\.", "",[dotall, global]),
            iolist_to_binary(Data2);
        false ->
            Data
    end.

set_escaping_mode(Flag) ->
    put(gdt_escape, Flag).

%% making proplist with vars for dtl rendering
prepare_for_rendering(Tree=#tree{}) ->
    F = fun
            (I = #import{}, {Imports, Module}) ->
                {[prepare_for_rendering(I)|Imports], Module};
            (M = #modul{}, {Imports, undefined}) ->
                {Imports, prepare_for_rendering(M)}
        end,
    {Imports, Modules} = lists:foldr(F, {[], undefined}, Tree#tree.exprs),
    [{imports, Imports}, {module, Modules}];
prepare_for_rendering(Import=#import{}) ->
    [{name, Import#import.name}];
prepare_for_rendering(Module = #modul{}) ->
    F = fun
            (D=#data{}, {Datatypes}) ->
                {[prepare_for_rendering(D)|Datatypes]}
        end,
    {Datatypes} = lists:foldr(F, {[]}, Module#modul.exprs),
    [{name, Module#modul.name}, {datatypes, Datatypes}];
prepare_for_rendering(Data = #data{}) ->
    F = fun prepare_for_rendering/1,
    [{name, Data#data.name}, {variants, lists:map(F, Data#data.variants)}];
prepare_for_rendering(Variant = #variant{}) ->
    [
        {id, Variant#variant.id},
        {constructor, Variant#variant.constructor},
        {value, prepare_for_rendering(Variant#variant.value)}
    ];
prepare_for_rendering(Rec = #rec{}) ->
    F = fun prepare_for_rendering/1,
    [{type, "record"}, {fields, lists:map(F, Rec#rec.fields)}];
prepare_for_rendering(Simple = #simple{}) ->
    [{type, "simple"}, {fields, Simple#simple.fields}];
prepare_for_rendering(#single{}) ->
    [{type, "single"}];
prepare_for_rendering(Field = #field{}) ->
    [
        {id, Field#field.id},
        {requirement, Field#field.requirement},
        {name, Field#field.name},
        {type, Field#field.type}
    ].

%% utils
check_sucess(ok, _) -> ok;
check_sucess({ok, V1}, _) -> V1;
check_sucess({ok, V1, V2}, _) -> {V1, V2};
check_sucess({ok, V1, V2, V3}, _) -> {V1, V2, V3};
check_sucess({error, Reason}, Error) -> throw({Error, Reason}).


%% logging
set_log_level(QuiteMode, Verbose) ->
    set_log_level(log_level(QuiteMode, Verbose)).

set_log_level(Level) when is_atom(Level) ->
    put(gdt_log_level, Level).

% log(Level, Str) ->
%     log(Str, []).

log(Level, Str, Args) when is_atom(Level) ->
    LogLevel = get(gdt_log_level),
    case should_log(LogLevel, Level) of
        true ->
            io:format(log_prefix(Level) ++ Str ++ "~n", Args);
        false ->
            ok
    end.

log_level(true , _    ) -> error;
log_level(false, false) -> info;
log_level(false, true ) -> debug.

should_log(debug, _    ) -> true;
should_log(info , debug) -> false;
should_log(info , _    ) -> true;
should_log(error, error) -> true;
should_log(_    , _    ) -> false.


log_prefix(debug) -> "DEBUG: ";
log_prefix(info ) -> "";
log_prefix(error) -> "ERROR: ".
