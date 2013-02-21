-module(parser_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("gdt_tree.hrl").

-define(TEST_PARSING(Expr, Tree),
    ?assertMatch(Tree, gdt:parse_gdt_string(Expr))
).

parse_test() ->
    %% single variant
    ?TEST_PARSING(
        "module Test where
            data Gender = Male
                        | Female
        ",
        #g_tree{imports=[], module=#g_module{name= <<"Test">>, exprs=[
            #g_data{name= <<"Gender">>, variants=[
                #g_variant{      constructor= <<"Male">>  , value=#g_variant_single{}},
                #g_variant{      constructor= <<"Female">>, value=#g_variant_single{}}
            ]}
        ]}}
    ),

    %% single variant
    ?TEST_PARSING(
        "module Test where
            data Gender = 1: Male
                        | 2: Female
        ",
        #g_tree{imports=[], module=#g_module{name= <<"Test">>, exprs=[
            #g_data{name= <<"Gender">>, variants=[
                #g_variant{id=1, constructor= <<"Male">>  , value=#g_variant_single{}},
                #g_variant{id=2, constructor= <<"Female">>, value=#g_variant_single{}}
            ]}
        ]}}
    ),

    %% simple variant
    ?TEST_PARSING(
        "module Test where
            data Attr = Email String
                      | Phone Integer String
            data Person = Person {attrs = [Email \"petr.kozorezov@gmail.com\", Phone 1 \"911\"] :: [Attr]}
        ",
        #g_tree{imports=[], module=#g_module{name= <<"Test">>, exprs=[
            #g_data{name= <<"Attr">>, variants=[
                #g_variant{constructor= <<"Email">>, value=#g_variant_simple{fields=[<<"String">>]}},
                #g_variant{constructor= <<"Phone">>, value=#g_variant_simple{fields=[<<"Integer">>, <<"String">>]}}
            ]},
            #g_data{name= <<"Person">>, variants=[
                #g_variant{constructor= <<"Person">>, value=#g_variant_record{fields=[
                    #g_field{requirement=optional, name= <<"attrs">>, type= {<<"List">>,[<<"Attr">>]}, default=[
                        #g_simple_value{constructor= <<"Email">>, fields=[<<"petr.kozorezov@gmail.com">>]},
                        #g_simple_value{constructor= <<"Phone">>, fields=[1, <<"911">>]}
                    ]}
                ]}}
            ]}
        ]}}
    ),

    %% records
    ?TEST_PARSING(
        "module Test where
            data Person = Person {
                1: required uid                  :: Integer,
                2: optional name     = \"Vasya\" :: String,
                3:          attrs    = []        :: [String],
                4:          profiles = <>        :: <String:String>
            }
            | Nobody
        ",
        #g_tree{imports=[], module=#g_module{name= <<"Test">>, exprs=[
            #g_data{name= <<"Person">>, variants=[
                #g_variant{constructor= <<"Person">>, value=#g_variant_record{fields=[
                    #g_field{id=1, requirement=required, name= <<"uid">>, type= <<"Integer">>},
                    #g_field{id=2, requirement=optional, name= <<"name">>, type= <<"String">>, default= <<"Vasya">>},
                    #g_field{id=3, requirement=optional, name= <<"attrs">>, type= {<<"List">>,[<<"String">>]}, default= []},
                    #g_field{id=4, requirement=optional, name= <<"profiles">>, type= {<<"Map">>,[<<"String">>,<<"String">>]}, default= []}
                ]}},
                #g_variant{constructor= <<"Nobody">>, value=#g_variant_single{}}
            ]}
        ]}}
    ),

    ?TEST_PARSING(
        "module Test where type Name = String",
        #g_tree{imports=[], module=#g_module{name= <<"Test">>, exprs=[
            #g_typedef{name= <<"Name">>, value= <<"String">>}
        ]}}
    ),

    %% TODO parametrized
    %% TODO rpc

    ok.
