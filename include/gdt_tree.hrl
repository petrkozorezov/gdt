-ifndef(_gdt_tree_included).
-define(_gdt_tree_included, yeah).

-record(g_tree, {imports :: g_imports(), module :: g_module()}).
-type(g_tree() :: #g_tree{}).

-record(g_import, {name :: binary()}).
-type(g_import() :: #g_import{}).
-type(g_imports() :: list(g_import())).

-record(g_module, {name :: binary(), exprs::g_exprs()}).
-type(g_module() :: #g_module{}).


-type(g_expr() :: g_data()). % | type() | rpc()
-type(g_exprs() :: list(g_expr())).

-record(g_data, {name::binary(), variants::g_variants()}).
-type(g_data() :: #g_data{}).

-record(g_variant, {id::integer(), constructor::binary(), value::(g_variant_simple() | g_variant_record() | g_variant_single())}).
-type(g_variant() :: #g_variant{}).
-type(g_variants() :: list(g_variant())).

-record(g_variant_single, {}).
-type(g_variant_single() :: #g_variant_single{}).

-record(g_variant_simple, {fields::list(binary())}).
-type(g_variant_simple() :: #g_variant_simple{}).

-record(g_variant_record, {fields::list(g_field())}).
-type(g_variant_record() :: #g_variant_record{}).

-type(requirement() :: (required | optional)).
-record(g_field, {id::integer(), requirement::requirement(), name::binary(), type}). %% TODO "type" type
-type(g_field() :: #g_field{}).

-endif.