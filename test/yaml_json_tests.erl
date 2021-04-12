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

-module(yaml_json_tests).

-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
  Parse = fun (Data) ->
              yaml_json:parse(Data, #{})
          end,
  [?_assertEqual({ok, []},
                 Parse(<<"">>)),
   ?_assertEqual({ok, [42]},
                 Parse(<<"42">>)),
   ?_assertEqual({ok, [<<"foo">>, <<"hello world">>]},
                 Parse(<<"--- foo\n---\nhello world\n">>)),
   ?_assertEqual({ok, [[null, true, false]]},
                 Parse(<<"[NULL, True, false]">>)),
   ?_assertEqual({ok, [#{<<"a">> => 42,
                         <<"b">> => 3.14,
                         <<"foo bar">> => <<"-0.1">>}]},
                 Parse(<<"{a: 42, !!str b: 3.14, \"foo bar\": !!str -0.1}">>)),
   ?_assertEqual({error, #{reason => {invalid_json_value, nan},
                           position => {1,7,6}}},
                 Parse(<<"[1.0, .NaN, 3.0]">>)),
   ?_assertEqual({error, #{reason => {invalid_json_key, 1},
                           position => {1,2,1}}},
                 Parse(<<"{1: 2}">>)),
   ?_assertEqual({error, #{reason => {invalid_json_key, [1, 2]},
                           position => {1,2,1}}},
                 Parse(<<"{[1, 2]: 3}">>)),
   ?_assertEqual({error, #{reason => {invalid_json_key, [1, 2]},
                           position => {1,3,2}}},
                 Parse(<<"[{[1, 2]: 3}]">>)),
   ?_assertEqual({error, #{reason => {invalid_json_key, []},
                           position => {1,7,6}}},
                 Parse(<<"[{a: {[]: 3}}]">>))].
