-ifndef(_gdt_tree_included).
-define(_gdt_tree_included, yeah).


%% Root exprs
-record(g_tree, {imports :: g_imports(), module :: g_module()}).
-type(g_tree() :: #g_tree{}).

-type(g_module_name() :: binary()).

-record(g_import, {name :: g_module_name()}).
-type(g_import() :: #g_import{}).
-type(g_imports() :: list(g_import())).

-record(g_module, {name :: g_module_name(), exprs::g_exprs()}).
-type(g_module() :: #g_module{}).

-type(g_expr() :: g_data() | g_typedef()| g_rpc()).
-type(g_exprs() :: list(g_expr())).


%% Types
-type(field_id() :: integer()).
-type(variant_id() :: integer()).
-type(g_label() :: binary()).
-type(g_type_name() :: binary()).
-type(g_type() :: 
       g_type_name() | g_label()
    | {g_type_name() | g_label(), list(g_type())}
).
-type(g_types() :: list(g_type())).

-type(g_constructor() :: binary()).

-record(g_data, {name::g_type_name(), variants::g_variants()}).
-type(g_data() :: #g_data{}).

-record(g_variant, {id::variant_id(), constructor::g_constructor(), value::(g_variant_simple() | g_variant_record() | g_variant_single())}).
-type(g_variant() :: #g_variant{}).
-type(g_variants() :: list(g_variant())).

-record(g_variant_single, {}).
-type(g_variant_single() :: #g_variant_single{}).

-record(g_variant_simple, {fields::list(g_type())}).
-type(g_variant_simple() :: #g_variant_simple{}).

-record(g_variant_record, {fields::list(g_field())}).
-type(g_variant_record() :: #g_variant_record{}).

-type(requirement() :: (required | optional)).
-record(g_field, {id::field_id(), requirement::requirement(), name::g_label(), default::g_value(), type::g_type()}).
-type(g_field() :: #g_field{}).

-record(g_typedef, {name::g_type(), value::g_type()}).
-type(g_typedef() :: #g_typedef{}).


%% Values
-type(g_value() ::
      integer()
    | float()
    | string()
    | binary()
    | g_simple_value()
    | g_record_value()
).

-record(g_simple_value, {constructor::g_constructor(), fields::list(g_value())}).
-type(g_simple_value() :: #g_simple_value{}).

-record(g_record_value, {constructor::g_constructor(), fields::list(g_record_value_field())}).
-type(g_record_value() :: #g_record_value{}).

-record(g_record_value_field, {name::g_label(), value::g_value()}).
-type(g_record_value_field() :: #g_record_value_field{}).

%% RPC
-type(g_rpc_direction() :: c2s | s2c).
-record(g_rpc, {name::g_label(), direction::g_rpc_direction(), client_values::g_types(), server_values::g_types()}).
-type(g_rpc() :: #g_rpc{}).

-endif.