%% Copyright (c) 2021 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(yaml_ast_tests).

-include_lib("eunit/include/eunit.hrl").

build_test_() ->
  Build = fun (Data) ->
              {ok, Events} = yaml_events:parse(Data),
              yaml_ast:build(Events, #{})
          end,
  [?_assertMatch({ok, [#{root :=
                           #{data := {scalar, <<"42">>, plain}}}]},
                 Build(<<"42">>)),
   ?_assertMatch({ok,
                  [#{root :=
                       #{data :=
                           {sequence,
                            [#{data := {scalar, <<"1">>, plain}},
                             #{data := {scalar, <<"2">>, plain}},
                             #{data := {scalar, <<"3">>, plain}}]}}}]},
                 Build(<<"[1,2,3]">>)),
   ?_assertMatch({ok,
                  [#{root :=
                       #{data :=
                           {mapping,
                            [{#{data := {scalar, <<"a">>, plain}},
                              #{data := {scalar, <<"true">>, non_plain}}},
                             {#{data := {scalar, <<"b">>, plain}},
                              #{data := {scalar, <<"false">>, plain}}}]}}}]},
                 Build(<<"{a: \"true\", b: false}">>)),
   ?_assertMatch({ok, [#{root :=
                           #{data := {scalar, <<"1">>, plain}}},
                       #{root :=
                           #{data := {scalar, <<"2">>, plain}}}]},
                 Build(<<"---\n1\n---\n2\n">>)),
   ?_assertMatch({ok, [#{root :=
                           #{data :=
                               {sequence,
                                [#{data := {scalar, <<"1">>, plain},
                                   anchor := <<"a">>},
                                 #{data :=
                                     {sequence,
                                      [#{data := {scalar, <<"2">>, plain},
                                         anchor := <<"a">>},
                                       #{data := {scalar, <<"2">>, plain},
                                         anchor := <<"a">>}]}},
                                 #{data := {scalar, <<"2">>, plain},
                                   anchor := <<"a">>}]}}}]},
                 Build(<<"[&a 1, [&a 2, *a], *a]">>)),
   ?_assertMatch({ok, [#{root :=
                           #{data :=
                               {sequence,
                                [#{data := {scalar, <<"1">>, plain},
                                   tag := <<"tag:yaml.org,2002:str">>},
                                 #{data := {scalar, <<"2">>, non_plain},
                                   tag := <<"tag:yaml.org,2002:int">>},
                                 #{data := {scalar, <<"3">>, plain},
                                   tag := <<"tag:example.com:foo">>},
                                 #{data := {scalar, <<"4">>, plain}}]},
                             tag := <<"tag:yaml.org,2002:set">>}}]},
                 Build(<<"%TAG ! tag:example.com:\n"
                         "---\n"
                         "!!set ["
                         "  !!str 1,"
                         "  !<tag:yaml.org,2002:int> '2',"
                         "  !foo 3,",
                         "  4"
                         "]">>)),
   ?_assertEqual({error, {unknown_alias, <<"b">>, {1,8,7}}},
                 Build(<<"[&a 1, *b]">>)),
   ?_assertEqual({error, {unknown_alias, <<"a">>, {2,5,14}}},
                 Build(<<"--- &a 42\n--- *a">>))].
