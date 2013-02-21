Nonterminals g_tree g_import g_imports g_module g_exprs g_expr g_variant g_variant_record g_variant_simple g_variant_param g_variant_record_fields g_variant_record_field g_requirement g_field_number g_default g_type_def g_type_params g_type_use g_type_use_params g_constructor g_type_uses g_label g_value_expr g_value_list_exprs g_value_map_expr g_value_map_exprs g_value_exprs g_simple_value g_simple_value_fields g_record_value g_record_value_field g_record_value_fields g_rpc_direction g_rpc_params.

Terminals import module where data type rpc required optional '::' '|' '=' '{' '}' ':' '[' ']' '<' '>' ',' '(' ')' '<-' '->' uname lname integer float string.

Rootsymbol g_tree.

g_tree -> g_imports g_module                                                        : #g_tree{imports='$1', module='$2'}.

g_import -> import uname                                                            : #g_import{name=module_name('$2')}.
g_imports -> g_import g_imports                                                     : ['$1'|'$2'].
g_imports -> '$empty'                                                               : [].
g_module -> module uname where g_exprs                                              : #g_module{name=module_name('$2'), exprs='$4'}.

g_exprs -> g_expr                                                                   : ['$1'].
g_exprs -> g_expr g_exprs                                                           : ['$1'|'$2'].

g_expr -> data g_type_def '=' g_variant                                             : #g_data{name='$2', variants='$4'}.
%g_expr -> g_label '=' g_value_expr                                                  : #g_var{name='$', value='$3'}.
g_expr -> type g_type_def '=' g_type_use                                            : #g_typedef{name='$2', value='$4'}.
g_expr -> rpc g_label '::' g_rpc_params g_rpc_direction g_rpc_params                : #g_rpc{name='$2', direction='$5', client_values='$4', server_values='$6'}.


g_variant -> g_field_number g_constructor g_variant_param                           : [#g_variant{id='$1', constructor='$2', value='$3'}].
g_variant -> g_variant '|' g_variant                                                : '$1' ++ '$3'.

g_variant_param -> g_variant_record                                                 : '$1'.
g_variant_param -> g_variant_simple                                                 : '$1'.
g_variant_param -> '$empty'                                                         : #g_variant_single{}.

g_variant_record -> '{' g_variant_record_fields '}'                                 : #g_variant_record{fields='$2'}.
g_variant_record -> '{' '}'                                                         : #g_variant_record{fields=[]}.
g_variant_record_fields -> g_variant_record_field                                   : ['$1'].
g_variant_record_fields -> g_variant_record_field ',' g_variant_record_fields       : ['$1'|'$3'].
g_variant_record_field  -> g_field_number g_requirement lname g_default '::' g_type_use : #g_field{id='$1', requirement='$2', name=constructor('$3'), default='$4', type='$6'}.

g_variant_simple -> g_type_use_params                                               : #g_variant_simple{fields='$1'}.

g_constructor -> uname                             : constructor('$1').

g_requirement -> required                          : required.
g_requirement -> optional                          : optional.
g_requirement -> '$empty'                          : optional.

g_field_number -> integer ':'                      : constant('$1').
g_field_number -> '$empty'                         : undefined.

g_default -> '=' g_value_expr                      : '$2'.
g_default -> '$empty'                              : undefined.

g_type_def -> uname                                : type('$1').
g_type_def -> uname g_type_params                  : {type('$1'), '$2'}.
g_type_params -> lname                             : [label('$1')].
g_type_params -> lname g_type_params               : [label('$1')|'$2'].


g_type_use -> '(' g_type_uses ')'                  : '$2'.
g_type_use -> '[' g_type_use ']'                   : {<<"List">>, ['$2']}.
g_type_use -> '<' g_type_use ':' g_type_use '>'    : {<<"Map">>, ['$2', '$4']}.
g_type_use -> uname                                : type('$1').
g_type_use -> lname                                : label('$1').
g_type_use -> uname g_type_use_params              : {type('$1'), '$2'}.
g_type_use -> lname g_type_use_params              : {label('$1'), '$2'}.
g_type_use_params -> lname                         : [label('$1')].
g_type_use_params -> uname                         : [type('$1')].
g_type_use_params -> lname g_type_use_params       : [label('$1')|'$2'].
g_type_use_params -> uname g_type_use_params       : [type('$1')|'$2'].

g_type_uses -> g_type_use                          : ['$1'].
g_type_uses -> g_type_use g_type_uses              : ['$1'|'$2'].

g_label -> lname                                   : label('$1').

%% Value
g_value_expr -> integer                                                   : constant('$1').
g_value_expr -> float                                                     : constant('$1').
g_value_expr -> string                                                    : constant('$1').
g_value_expr -> g_simple_value                                            : '$1'.
g_value_expr -> g_record_value                                            : '$1'.
g_value_expr -> '<' g_value_map_exprs '>'                                 : '$2'.
g_value_expr -> '<' '>'                                                   : [].
g_value_expr -> '[' g_value_list_exprs ']'                                : '$2'.
g_value_expr -> '[' ']'                                                   : [].
g_value_expr -> '(' g_value_expr ')'                                      : '$2'.

g_value_list_exprs -> g_value_expr                                        : ['$1'].
g_value_list_exprs -> g_value_expr ',' g_value_list_exprs                 : ['$1'|'$3'].

g_value_map_expr -> g_value_expr ':' g_value_expr                         : {'$1', '$3'}.
g_value_map_exprs -> g_value_map_expr                                     : ['$1'].
g_value_map_exprs -> g_value_map_expr ',' g_value_map_exprs               : ['$1'|'$3'].

g_value_exprs -> g_value_expr                                             : ['$1'].
g_value_exprs -> g_value_expr g_value_exprs                               : ['$1'|'$2'].

g_simple_value -> g_constructor g_simple_value_fields                     : #g_simple_value{constructor='$1', fields='$2'}.
g_simple_value_fields -> g_value_exprs                                    : '$1'.
g_simple_value_fields -> '$empty'                                         : [].

g_record_value -> g_constructor '{' g_record_value_fields '}'             : #g_record_value{constructor='$1', fields='$3'}.
g_record_value_field -> g_label ':' g_value_expr                          : #g_record_value_field{name='$1', value='$3'}.
g_record_value_fields -> g_record_value_field                             : ['$1'].
g_record_value_fields -> g_record_value_field ',' g_record_value_fields   : ['$1'|'$3'].


%% RPC
g_rpc_direction -> '->'                                                   : c2s.
g_rpc_direction -> '<-'                                                   : s2c.

g_rpc_params -> '$empty'                                                  : [].
g_rpc_params -> g_type_use                                                : ['$1'].
g_rpc_params -> g_type_use ',' g_rpc_params                               : ['$1'|'$3'].



Erlang code.

-include("gdt_tree.hrl").

type({_,_,V}) -> V.
label({_,_,V}) -> V.
constructor({_,_,V}) -> V.
constant({_,_,V}) -> V.
module_name({_,_,V}) -> V.

%binary_to_atom(V) when is_binary(V) -> list_to_atom(binary_to_list(V));
%binary_to_atom(_) -> error(badarg).
