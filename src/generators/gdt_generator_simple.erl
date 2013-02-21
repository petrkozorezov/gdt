-module(gdt_generator_simple).
-compile(export_all).

-include_lib("gdt_tree.hrl").

all(Tree) ->
    hello (Tree) ++
    screen(Tree) ++
    plain (Tree) .

hello(#g_tree{module=#g_module{name=Name}}) ->
    FileName = "hello.txt",
    FileData = ["Hello ", Name,"!!!"],
    [{FileName, FileData}].

plain(Tree) ->
    FileName = "plain.txt",
    FileData = io_lib:format("~p~n", [Tree]),
    [{FileName, FileData}].

screen(Tree) ->
    gdt_utils:log(info, "GDT tree:~n~p", [Tree]),
    [].