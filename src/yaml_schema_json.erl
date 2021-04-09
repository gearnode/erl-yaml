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

-module(yaml_schema_json).

-export([schema/0, decode_tagged_value/2, identify_plain_scalar/1]).

-spec schema() -> yaml:schema().
schema() ->
  #{tagged_value_decoder => fun decode_tagged_value/2,
    plain_scalar_identifier => fun identify_plain_scalar/1}.

-spec decode_tagged_value(yaml:tag(), yaml:undecoded_value()) ->
        yaml:tagged_value_decoding_result().
decode_tagged_value(<<"tag:yaml.org,2002:map">>, Value) ->
  {ok, Value};
decode_tagged_value(<<"tag:yaml.org,2002:seq">>, Value) ->
  {ok, Value};
decode_tagged_value(<<"tag:yaml.org,2002:str">>, Value) ->
  {ok, Value};
decode_tagged_value(<<"tag:yaml.org,2002:null">>, Value) ->
  case Value of
    <<"null">> ->
      {ok, null};
    _ ->
      {error, invalid_null}
  end;
decode_tagged_value(<<"tag:yaml.org,2002:bool">>, Value) ->
  case Value of
    <<"true">> ->
      {ok, true};
    <<"false">> ->
      {ok, false};
    _ ->
      {error, invalid_boolean}
  end;
decode_tagged_value(<<"tag:yaml.org,2002:int">>, Value) ->
  try
    {ok, binary_to_integer(Value)}
  catch
    error:_ ->
      {error, invalid_integer}
  end;
decode_tagged_value(<<"tag:yaml.org,2002:float">>, Value) ->
  try
    {ok, binary_to_float(Value)}
  catch
    error:_ ->
      {error, invalid_float}
  end;
decode_tagged_value(_, _) ->
  unknown_tag.

-spec identify_plain_scalar(binary()) -> yaml:tag().
identify_plain_scalar(Value) ->
  identify(Value, [fun identify_null/1,
                   fun identify_boolean/1,
                   fun identify_integer/1,
                   fun identify_float/1]).

-spec identify(binary(), [IdFun]) -> yaml:tag() when
    IdFun :: fun((binary()) -> {ok, yaml:tag()} | error).
identify(_, []) ->
  <<"tag:yaml.org,2002:str">>;
identify(Value, [Fun | Funs]) ->
  case Fun(Value) of
    {ok, Tag} ->
      Tag;
    error ->
      identify(Value, Funs)
  end.

-spec identify_null(binary()) -> {ok, yaml:tag()} | error.
identify_null(<<"null">>) ->
  {ok, <<"tag:yaml.org,2002:null">>};
identify_null(_) ->
  error.

-spec identify_boolean(binary()) -> {ok, yaml:tag()} | error.
identify_boolean(<<"true">>) ->
  {ok, <<"tag:yaml.org,2002:bool">>};
identify_boolean(<<"false">>) ->
  {ok, <<"tag:yaml.org,2002:bool">>};
identify_boolean(_) ->
  error.

-spec identify_integer(binary()) -> {ok, yaml:tag()} | error.
identify_integer(Value) ->
  RE = "^-?(?:0|[1-9][0-9]*)$",
  case re:run(Value, RE) of
    {match, _} ->
      {ok, <<"tag:yaml.org,2002:int">>};
    nomatch ->
      error
  end.

-spec identify_float(binary()) -> {ok, yaml:tag()} | error.
identify_float(Value) ->
  %% XXX We should support floating point numbers without decimal parts (e.g.
  %% "3e2" or "1."), but erlang:float_to_binary/1 do not. A solution would be
  %% to use the erl-json parser since it parses floating point values on its
  %% own.
  %% RE = "^-?(0|[1-9][0-9]*)(\\.[0-9]*)?([eE][-+]?[0-9]+)?$",
  RE = "^-?(?:0|[1-9][0-9]*)\\.[0-9]+(?:[eE][-+]?[0-9]+)?$",
  case re:run(Value, RE) of
    {match, _} ->
      {ok, <<"tag:yaml.org,2002:float">>};
    nomatch ->
      error
  end.
