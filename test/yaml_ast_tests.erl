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
              yaml_ast:build(Events)
          end,
  [?_assertMatch({ok, [#{root :=
                           #{data := {scalar, <<"42">>}}}]},
                 Build(<<"42">>)),
   ?_assertMatch({ok,
                  [#{root :=
                       #{data :=
                           {sequence,
                            [#{data := {scalar, <<"1">>}},
                             #{data := {scalar, <<"2">>}},
                             #{data := {scalar, <<"3">>}}]}}}]},
                 Build(<<"[1,2,3]">>)),
   ?_assertMatch({ok,
                  [#{root :=
                       #{data :=
                           {mapping,
                            [{#{data := {scalar, <<"a">>}},
                              #{data := {scalar, <<"true">>}}},
                             {#{data := {scalar, <<"b">>}},
                              #{data := {scalar, <<"false">>}}}]}}}]},
                 Build(<<"{a: true, b: \"false\"}">>)),
   ?_assertMatch({ok, [#{root :=
                           #{data := {scalar, <<"1">>}}},
                       #{root :=
                           #{data := {scalar, <<"2">>}}}]},
                 Build(<<"---\n1\n---\n2\n">>)),
   ?_assertMatch({ok, [#{root :=
                           #{data :=
                               {sequence,
                                [#{data := {scalar, <<"1">>},
                                   anchor := <<"a">>},
                                 #{data :=
                                     {sequence,
                                      [#{data := {scalar, <<"2">>},
                                         anchor := <<"a">>},
                                       #{data := {scalar, <<"2">>},
                                         anchor := <<"a">>}]}},
                                 #{data := {scalar, <<"2">>},
                                   anchor := <<"a">>}]}}}]},
                 Build(<<"[&a 1, [&a 2, *a], *a]">>)),
   ?_assertMatch({ok, [#{root :=
                           #{data :=
                               {sequence,
                                [#{data := {scalar, <<"1">>},
                                   tag := <<"tag:yaml.org,2002:str">>},
                                 #{data := {scalar, <<"2">>},
                                   tag := <<"tag:yaml.org,2002:int">>},
                                 #{data := {scalar, <<"3">>},
                                   tag := <<"tag:example.com:foo">>},
                                 #{data := {scalar, <<"4">>},
                                   tag := <<"?">>}]},
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
