-module(example).
% -include("example.hrl").

% %% API
% -export([encode/3, decode/3]).

% %% internal
% -export([encode_/3, decode_/3]).

% encode(msgpack, Type, Value) ->
%     msgpack:pack(encode_msgpack(Type, Value));
% encode(Format, _, _) ->
%     error(bad_format, Format).

% decode(msgpack, Type, Data) ->
%     {ok, Spam} = msgpack:unpack(Data).
%     decode_msgpack(Type, Spam).
% decode(Format, _, _) ->
%     error(bad_format, Format).


% encode_(msgpack, Type, Value) ->
%     encode_msgpack(Type, Value);
% encode_(Format, _, _) ->
%     error(bad_format, Format).

% decode_(msgpack, Type, Data) ->
%     decode_msgpack(Type, Data).
% decode_(Format, _, _) ->
%     error(bad_format, Format).


% encode_msgpack(gender, Gender) ->
%     case Gender of
%         male -> <<"Male">>;
%         female -> <<"Female">>;
%         _ -> error({bad_value, Gender})
%     end;
% encode_msgpack(attr, Attr) ->
%     case Attr of
%         {email, A1} ->
%             {[{0, <<"Email">>},
%                 {1, required(encode_msgpack(string, A1))}]
%             };
%         {phone, A1} ->
%             {[{0, <<"Phone">>},
%                 {1, required(encode_msgpack(string, A1))}]
%             };
%         _ -> 
%             error(bad_value, Attr)
%     end;
% encode_msgpack(person, Person) ->
%     case Person of
%         {person, Uid, Name, Gender, Attrs} ->
%             {[{0, <<"Person">>},
%                 {<<"uid">>, required(encode_msgpack(integer, Uid))}, ...]
%             };
%         _ -> error(bad_value, Person)
%     end;

% encode_msgpack(string, String) when is_list(String) ->
%     list_to_binary(String);
% encode_msgpack(string, String) when is_binary(String) ->
%     String;
% encode_msgpack(bool, Boolean) when (Boolean =:= true) or (Boolean =:= false) ->
%     Boolean;
% encode_msgpack(integer, Integer) when is_integer(Integer) ->
%     Integer;
% encode_msgpack(float, Float) when is_float(Float) ->
%     Float;
% encode_msgpack(binary, Binary) when is_binary(Binary) ->
%     Binary;
% encode_msgpack({list, A}, List) when is_list(List) ->
%     [encode_msgpack(A, E) || E <- List];
% encode_msgpack({map, A, B}, Map) when is_list(Map) ->
%     [{encode_msgpack(A, K), encode_msgpack(B, V)} || {K, V} <- Map];

% encode_msgpack(Type, Value) -> error({bad_type, Type, Value}).




% decode_msgpack(gender, Gender) ->
%     case find_variant_tag(0, Gender) of
%         <<"Male">> ->
%             male;
%         <<"Female">> ->
%             female;
%         _ ->
%             error(bad_value, Gender)
%     end;
% decode_msgpack(attr, Attr) ->
%     case find_variant_tag(0, Attr) of
%         <<"Email">> -> 
%             {email,
%                 decode_msgpack(string, required(find_field(1, Attr)))
%             };
%         <<"Phone">> ->
%             {phone,
%                 decode_msgpack(string, required(find_field(1, Attr)))
%             };
%         undefined ->
%             error(bad_data, Attr)
%     end;
% decode_msgpack(person, Person) ->
%     case find_variant_tag(0, Person) of
%         <<"Person">> ->
%             #person{
%                 uid = decode_msgpack(integer, required(find_field(<<"uid">>, Person))),
%                 name = foo:decode_msgpack(name, required(find_field(<<"name">>, Person))),
%                 gender = decode_msgpack(gender, optional(find_field(<<"gender">>, Person))),
%                 attrs = decode_msgpack({list, attr}, optional(find_field(<<"attrs">>, Person), undefined)),
%                 attrs_map = decode_msgpack({map, integer, attr}, optional(find_field(<<"attrs_map">>, Person), undefined))
%             };
%         _ ->
%             error(bad_value, Person)
%     end;

% decode_msgpack(string, String) when is_binary(String) ->
%     String;
% decode_msgpack(bool, Boolean) when (Boolean =:= true) or (Boolean =:= false) ->
%     Boolean;
% decode_msgpack(integer, Integer) when is_integer(Integer) ->
%     Integer;
% decode_msgpack(float, Float) when is_float(Float) ->
%     Float;
% decode_msgpack(binary, Binary) when is_binary(Binary) ->
%     Binary;
% decode_msgpack({list, A}, List) when is_list(List) ->
%     [decode_msgpack(A, E) || E <- List];
% decode_msgpack({map, A, B}, Map) when is_list(Map) ->
%     [{decode_msgpack(A, K), decode_msgpack(B, V)} || {K, V} <- Map];

% decode_msgpack(Type, Data) ->
%     error({bad_type, Type, Data}).


% find_field(Field, {PL}) -> proplists:get_value(Field, PL).
% find_variant_tag(Field, {PL}) -> proplists:get_value(Field, PL).
% find_variant_tag(Field, V) -> V.
% required(V) -> required(V, undefined).
% required(undefined, _) -> error(required_field_is_absent);
% required(V, _) -> V.
% optional(undefined, V) -> V;
% optional(V, _) -> V.
