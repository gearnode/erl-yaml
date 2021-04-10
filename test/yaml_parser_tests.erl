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

-module(yaml_parser_tests).

-include_lib("eunit/include/eunit.hrl").

parser_test_() ->
  Parse = fun (Data) ->
              yaml_parser:parse(Data, #{})
          end,
  [%% Values
   ?_assertEqual({ok, []},
                 Parse(<<"">>)),
   ?_assertEqual({ok, [<<"">>]},
                 Parse(<<"---">>)),
   ?_assertEqual({ok, [<<"foo">>]},
                 Parse(<<"foo">>)),
   ?_assertEqual({ok, [<<"foo">>,
                       <<"bar">>]},
                 Parse(<<"---\nfoo\n...\n---\nbar\n...\n">>)),
   ?_assertEqual({ok, [[1, 2, 3]]},
                 Parse(<<"[1,2,3]">>)),
   ?_assertEqual({ok, [#{<<"foo">> => 1, <<"bar">> => 2}]},
                 Parse(<<"{foo: 1, bar: 2}">>)),
   ?_assertEqual({ok, [[null, null, null, null]]},
                 Parse(<<"[~, null, Null, NULL]">>)),
   ?_assertEqual({ok, [[true, true, true]]},
                 Parse(<<"[true, True, TRUE]">>)),
   ?_assertEqual({ok, [[false, false, false]]},
                 Parse(<<"[false, False, FALSE]">>)),
   ?_assertEqual({ok, [[0, 8#7, 16#3a, -19]]},
                 Parse(<<"[0, 0o7, 0x3A, -19]">>)),
   ?_assertEqual({ok, [[0.0, -0.0, 0.5, 12.0e3, -2.0e+5]]},
                 Parse(<<"[0.0, -0.0, 0.5, +12.0e03, -2.0E+05]">>)),
   ?_assertEqual({ok, [[nan, nan, nan]]},
                 Parse(<<"[.nan, .NaN, .NAN]">>)),
   ?_assertEqual({ok, [[positive_infinity, positive_infinity,
                        positive_infinity, positive_infinity,
                        positive_infinity, positive_infinity]]},
                 Parse(<<"[.inf, .Inf, .INF, +.inf, +.Inf, +.INF]">>)),
   ?_assertEqual({ok, [[negative_infinity, negative_infinity,
                        negative_infinity]]},
                 Parse(<<"[-.inf, -.Inf, -.INF]">>)),
   %% Tags
   ?_assertEqual({ok, [[]]},
                 Parse(<<"!!seq []">>)),
   ?_assertEqual({error, {invalid_value, invalid_sequence,
                          <<"tag:yaml.org,2002:seq">>, <<"true">>, {1,1,0}}},
                 Parse(<<"!!seq true">>)),
   ?_assertEqual({ok, [#{}]},
                 Parse(<<"!!map {}">>)),
   ?_assertEqual({error, {invalid_value, invalid_mapping,
                          <<"tag:yaml.org,2002:map">>, <<"42">>, {1,1,0}}},
                 Parse(<<"!!map 42">>)),
   ?_assertEqual({error, {invalid_value, invalid_mapping,
                          <<"tag:yaml.org,2002:map">>, [1,2], {1,1,0}}},
                 Parse(<<"!!map [1,2]">>)),
   ?_assertEqual({ok, [<<"foo">>]},
                 Parse(<<"!!str foo">>)),
   ?_assertEqual({ok, [<<"42">>]},
                 Parse(<<"!!str 42">>)),
   ?_assertEqual({error, {invalid_value, invalid_string,
                          <<"tag:yaml.org,2002:str">>, [], {1,1,0}}},
                 Parse(<<"!!str []">>)),
   %% Anchors and aliases
   ?_assertEqual({ok, [42]},
                 Parse(<<"&a 42">>)),
   ?_assertEqual({ok, [[42, 42]]},
                 Parse(<<"[&a 42, *a]">>)),
   ?_assertEqual({ok, [#{<<"a">> => <<"b">>, <<"b">> => <<"a">>}]},
                 Parse(<<"{&a a: &b b, *b: *a}">>)),
   ?_assertEqual({error, {unknown_alias, <<"a">>, {1,2,1}}},
                 Parse(<<"[*a, &a 42]">>)),
   ?_assertEqual({error, {unknown_alias, <<"b">>, {1,9,8}}},
                 Parse(<<"[&a 42, *b]">>)),
   ?_assertEqual({error, {unknown_alias, <<"a">>, {4,1,14}}},
                 Parse(<<"---\n&a 42\n---\n*a">>))
].
