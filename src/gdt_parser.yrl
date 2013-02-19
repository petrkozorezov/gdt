Nonterminals g_tree g_import g_imports g_module g_exprs g_expr g_variant g_variant_record g_variant_simple g_variant_param g_variant_record_fields g_variant_record_field g_requirement g_field_number g_type_def g_type_use g_constructor g_type_uses.

Terminals import module where data required optional '::' '|' '=' '{' '}' ':' '[' ']' '<' '>' ',' '(' ')' uname lname integer.

Rootsymbol g_tree.

% TODO 
%  - default values
%  - "type"
%  - rpc

g_tree -> g_imports g_module                                                        : #g_tree{imports='$1', module='$2'}.

g_import -> import uname                                                            : #g_import{name=unwrap3_v('$2')}.
g_imports -> g_import g_imports                                                     : ['$1'|'$2'].
g_imports -> '$empty'                                                               : [].
g_module -> module uname where g_exprs                                              : #g_module{name=unwrap3_v('$2'), exprs='$4'}.

g_exprs -> g_expr                                                                   : ['$1'].
g_exprs -> g_expr g_exprs                                                           : ['$1'|'$2'].

g_expr -> data g_type_def '=' g_variant                                             : #g_data{name='$2', variants='$4'}.
% type ??
% rpc

g_variant -> g_field_number g_constructor g_variant_param                           : [#g_variant{id='$1', constructor='$2', value='$3'}].
g_variant -> g_variant '|' g_variant                                                : '$1' ++ '$3'.

g_variant_param -> g_variant_record                                                 : '$1'.
g_variant_param -> g_variant_simple                                                 : '$1'.
g_variant_param -> '$empty'                                                         : #g_variant_single{}.

g_variant_record -> '{' g_variant_record_fields '}'                                 : #g_variant_record{fields='$2'}.
g_variant_record_fields -> g_variant_record_field                                   : ['$1'].
g_variant_record_fields -> g_variant_record_field ',' g_variant_record_fields       : ['$1'|'$3'].
g_variant_record_field  -> g_field_number g_requirement lname '::' g_type_use       : #g_field{id='$1', requirement='$2', name=unwrap3_v('$3'), type='$5'}.

g_variant_simple -> g_type_uses                                                     : #g_variant_simple{fields='$1'}.

g_constructor -> uname                             : unwrap3_v('$1').

g_requirement -> required                          : required.
g_requirement -> optional                          : optional.
g_requirement -> '$empty'                          : optional.

g_field_number -> integer ':'                      : unwrap3_v('$1').
g_field_number -> '$empty'                         : undefined.

g_type_def -> uname                                : unwrap3_v('$1').

g_type_use -> '(' g_type_uses ')'                  : '$2'.
g_type_use -> '[' g_type_use ']'                   : [<<"List">>, '$2'].
g_type_use -> '<' g_type_use ':' g_type_use '>'    : [<<"Map">>, '$2', '$4'].
g_type_use -> uname                                : unwrap3_v('$1').

g_type_uses -> g_type_use                          : ['$1'].
g_type_uses -> g_type_use g_type_uses              : ['$1'|'$2'].


Erlang code.

-include("gdt_tree.hrl").

%unwrap2_t({V,_}) -> V.
unwrap3_v({_,_,V}) -> V.

