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

-module(yaml_schema_failsafe).

-export([schema/0, decode_tagged_value/2, identify_plain_scalar/1]).

-spec schema() -> yaml:schema().
schema() ->
  #{tagged_value_decoder => fun decode_tagged_value/2,
    plain_scalar_identifier => fun identify_plain_scalar/1}.

-spec decode_tagged_value(yaml:tag(), yaml:undecoded_value()) ->
        yaml:value() | unknown_tag.
decode_tagged_value(<<"tag:yaml.org,2002:map">>, Value) ->
  {ok, Value};
decode_tagged_value(<<"tag:yaml.org,2002:seq">>, Value) ->
  {ok, Value};
decode_tagged_value(<<"tag:yaml.org,2002:str">>, Value) ->
  {ok, Value};
decode_tagged_value(_, _) ->
  error.

-spec identify_plain_scalar(binary()) -> yaml:tag().
identify_plain_scalar(_) ->
  <<"tag:yaml.org,2002:str">>.
