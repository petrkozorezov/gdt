-module(encode_tests).

-include_lib("eunit/include/eunit.hrl").

-include("types.hrl").

run_test() ->
    Person = #person{uid=1, name= <<"Petr">>, gender=male, attrs=[{email, <<"petr.kozorezov@gmail.com">>}]},
    ?assertMatch(Person, types:decode(person, types:encode(person, Person))).
    % exit(byte_size(term_to_binary(Person))),
    % exit(byte_size(types:encode(person, Person))).
