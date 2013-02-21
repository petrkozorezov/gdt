-module(gdt_tree).
-compile(export_all).

%% получение имени g-типа
type_name({Name, _}) -> Name;
type_name( Name    ) -> Name.

%% получение параметров g-типа
type_params({_, Params}) -> Params;
type_params( _         ) -> [].

is_label(TypeOrLabel) -> gdt_utils:is_lower(TypeOrLabel).
is_type (TypeOrLabel) -> gdt_utils:is_upper(TypeOrLabel).
