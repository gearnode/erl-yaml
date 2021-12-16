%% Copyright (c) 2021 Exograd SAS.
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

-module(yaml_events_tests).

-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
  Parse = fun yaml_events:parse/1,
  [?_assertMatch({ok, [#{type := stream_start,
                         data := #{encoding := utf8}},
                       #{type := stream_end}]},
                 Parse(<<>>)),
   ?_assertMatch({ok, [#{type := stream_start,
                         data := #{encoding := utf8}},
                       #{type := document_start,
                         data := #{implicit := true}},
                       #{type := sequence_start,
                         data := #{implicit := false,
                                   style := flow}},
                       #{type := scalar,
                         data := #{value := <<"1">>,
                                   plain_implicit := true,
                                   quoted_implicit := false,
                                   style := plain}},
                       #{type := mapping_start,
                         data := #{implicit := false,
                                   style := flow}},
                       #{type := scalar,
                         data := #{value := <<"a">>,
                                   plain_implicit := false,
                                   quoted_implicit := true,
                                   style := double_quoted}},
                       #{type := scalar,
                         data := #{value := <<"2">>,
                                   plain_implicit := true,
                                   quoted_implicit := false,
                                   style := plain}},
                       #{type := mapping_end},
                       #{type := sequence_end},
                       #{type := document_end,
                         data := #{implicit := true}},
                       #{type := stream_end}]},
                 Parse(<<"[1,{\"a\": 2}]">>)),
   ?_assertMatch({error, {syntax_error, _, {1,3,2}}},
                 Parse(<<"[{]">>))].
