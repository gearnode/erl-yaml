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

-module(yaml_schema_core).

-export([schema/0]).

-spec schema() -> yaml:schema().
schema() ->
  #{tagged_value_decoder => fun decode_tagged_value/2,
    plain_scalar_identifier => fun identify_plain_scalar/1}.

-spec decode_tagged_value(yaml:tag(), yaml:undecoded_value()) ->
        yaml:tagged_value_decoding_result().
decode_tagged_value(<<"tag:yaml.org,2002:map">>, Value) ->
  if
    is_map(Value) ->
      {ok, Value};
    true ->
      {error, invalid_mapping}
  end;
decode_tagged_value(<<"tag:yaml.org,2002:seq">>, Value) ->
  if
    is_list(Value) ->
      {ok, Value};
    true ->
      {error, invalid_sequence}
  end;
decode_tagged_value(<<"tag:yaml.org,2002:str">>, Value) ->
  if
    is_binary(Value) ->
      {ok, Value};
    true ->
      {error, invalid_string}
  end;
decode_tagged_value(<<"tag:yaml.org,2002:null">>, Value) ->
  case parse_null(Value) of
    {ok, null} ->
      {ok, null};
    error ->
      {error, invalid_null}
  end;
decode_tagged_value(<<"tag:yaml.org,2002:bool">>, Value) ->
  case parse_bool(Value) of
    {ok, Boolean} ->
      {ok, Boolean};
    error ->
      {error, invalid_boolean}
  end;
decode_tagged_value(<<"tag:yaml.org,2002:int">>, Value) ->
  case parse_integer(Value) of
    {ok, Integer} ->
      {ok, Integer};
    error ->
      {error, invalid_integer}
  end;
decode_tagged_value(<<"tag:yaml.org,2002:float">>, Value) ->
  case parse_float(Value) of
    {ok, Float} ->
      {ok, Float};
    error ->
      {error, invalid_float}
  end;
decode_tagged_value(_, _) ->
  unknown_tag.

-spec identify_plain_scalar(binary()) -> yaml:plain_scalar_identifier_result().
identify_plain_scalar(Value) ->
  identify(Value, [fun identify_null/1,
                   fun identify_boolean/1,
                   fun identify_integer/1,
                   fun identify_float/1]).

-spec identify(binary(), [IdFun]) -> yaml:plain_scalar_identifier_result() when
    IdFun :: fun((binary()) -> yaml:plain_scalar_identifier_result() | error).
identify(Value, []) ->
  {value, Value};
identify(Value, [Fun | Funs]) ->
  case Fun(Value) of
    error ->
      identify(Value, Funs);
    Result ->
      Result
  end.

-spec identify_null(binary()) ->
        yaml:plain_scalar_identifier_result() | error.
identify_null(Value) ->
  case parse_null(Value) of
    {ok, null} ->
      {value, null};
    error ->
      error
  end.

-spec identify_boolean(binary()) ->
        yaml:plain_scalar_identifier_result() | error.
identify_boolean(Value) ->
  case parse_bool(Value) of
    {ok, Boolean} ->
      {value, Boolean};
    error ->
      error
  end.

-spec identify_integer(binary()) ->
        yaml:plain_scalar_identifier_result() | error.
identify_integer(Value) ->
  case parse_integer(Value) of
    {ok, Integer} ->
      {value, Integer};
    error ->
      error
  end.

-spec identify_float(binary()) ->
        yaml:plain_scalar_identifier_result() | error.
identify_float(<<".nan">>) ->
  {value, nan};
identify_float(<<".NaN">>) ->
  {value, nan};
identify_float(<<".NAN">>) ->
  {value, nan};
identify_float(<<".inf">>) ->
  {value, positive_infinity};
identify_float(<<".Inf">>) ->
  {value, positive_infinity};
identify_float(<<".INF">>) ->
  {value, positive_infinity};
identify_float(<<"+.inf">>) ->
  {value, positive_infinity};
identify_float(<<"+.Inf">>) ->
  {value, positive_infinity};
identify_float(<<"+.INF">>) ->
  {value, positive_infinity};
identify_float(<<"-.inf">>) ->
  {value, negative_infinity};
identify_float(<<"-.Inf">>) ->
  {value, negative_infinity};
identify_float(<<"-.INF">>) ->
  {value, negative_infinity};
identify_float(Value) ->
  RE = "^[-+]?(?:\\.[0-9]+|[0-9]+(?:\\.[0-9]*)?)(?:[eE][-+]?[0-9]+)?$",
  case re:run(Value, RE) of
    {match, _} ->
      {tag, <<"tag:yaml.org,2002:float">>};
    nomatch ->
      error
  end.

-spec parse_null(binary()) -> {ok, null} | error.
parse_null(<<"~">>) ->
  {ok, null};
parse_null(<<"null">>) ->
  {ok, null};
parse_null(<<"Null">>) ->
  {ok, null};
parse_null(<<"NULL">>) ->
  {ok, null};
parse_null(_) ->
  error.

-spec parse_bool(binary()) -> {ok, boolean()} | error.
parse_bool(<<"true">>) ->
  {ok, true};
parse_bool(<<"True">>) ->
  {ok, true};
parse_bool(<<"TRUE">>) ->
  {ok, true};
parse_bool(<<"false">>) ->
  {ok, false};
parse_bool(<<"False">>) ->
  {ok, false};
parse_bool(<<"FALSE">>) ->
  {ok, false};
parse_bool(_) ->
  error.

-spec parse_integer(binary()) -> {ok, integer()} | error.
parse_integer(<<"0o", Value/binary>>) ->
  try
    {ok, erlang:binary_to_integer(Value, 8)}
  catch
    error:_ ->
      error
  end;
parse_integer(<<"0x", Value/binary>>) ->
  try
    {ok, erlang:binary_to_integer(Value, 16)}
  catch
    error:_ ->
      error
  end;
parse_integer(Value) ->
  try
    {ok, erlang:binary_to_integer(Value)}
  catch
    error:_ ->
      error
  end.

-spec parse_float(binary()) -> {ok, float()} | error.
parse_float(<<".nan">>) ->
  {ok, nan};
parse_float(<<".NaN">>) ->
  {ok, nan};
parse_float(<<".NAN">>) ->
  {ok, nan};
parse_float(<<".inf">>) ->
  {ok, positive_infinity};
parse_float(<<".Inf">>) ->
  {ok, positive_infinity};
parse_float(<<".INF">>) ->
  {ok, positive_infinity};
parse_float(<<"+.inf">>) ->
  {ok, positive_infinity};
parse_float(<<"+.Inf">>) ->
  {ok, positive_infinity};
parse_float(<<"+.INF">>) ->
  {ok, positive_infinity};
parse_float(<<"-.inf">>) ->
  {ok, negative_infinity};
parse_float(<<"-.Inf">>) ->
  {ok, negative_infinity};
parse_float(<<"-.INF">>) ->
  {ok, negative_infinity};
parse_float(Value) ->
  try
    {ok, erlang:binary_to_float(Value)}
  catch
    error:_ ->
      error
  end.
