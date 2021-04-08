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
  [?_assertEqual({ok, [#{root =>
                           #{data => {scalar, <<"42">>},
                             position => {1,1,0}}}]},
                 Build(<<"42">>)),
   ?_assertEqual({ok,
                  [#{root =>
                       #{data =>
                           {sequence,
                            [#{data => {scalar, <<"1">>},
                               position => {1,2,1}},
                             #{data => {scalar, <<"2">>},
                               position => {1,4,3}},
                             #{data => {scalar, <<"3">>},
                               position => {1,6,5}}]},
                         position => {1,1,0}}}]},
                 Build(<<"[1,2,3]">>)),
   ?_assertEqual({ok,
                  [#{root =>
                       #{data =>
                           {mapping,
                            [{#{data => {scalar, <<"a">>},
                                position => {1,2,1}},
                              #{data => {scalar, <<"true">>},
                                position => {1,5,4}}},
                             {#{data => {scalar, <<"b">>},
                                position => {1,11,10}},
                              #{data => {scalar, <<"false">>},
                                position => {1,14,13}}}]},
                         position => {1,1,0}}}]},
                 Build(<<"{a: true, b: \"false\"}">>)),
   ?_assertEqual({ok, [#{root =>
                           #{data =>
                               {sequence,
                                [#{data => {scalar, <<"1">>},
                                   position => {1,2,1},
                                   anchor => <<"a">>},
                                 #{data =>
                                     {sequence,
                                      [#{data => {scalar, <<"2">>},
                                         position => {1,9,8},
                                         anchor => <<"a">>},
                                       #{data => {scalar, <<"2">>},
                                         position => {1,9,8},
                                         anchor => <<"a">>}]},
                                   position => {1,8,7}},
                                 #{data => {scalar, <<"2">>},
                                   position => {1,9,8},
                                   anchor => <<"a">>}]},
                             position => {1,1,0}}}]},
                 Build(<<"[&a 1, [&a 2, *a], *a]">>))].
