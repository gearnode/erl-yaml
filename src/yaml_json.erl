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

-module(yaml_json).

-export([parse/1, parse/2, build/1]).

-export_type([value/0, array/0, object/0, key/0]).

-type value() :: null | boolean() | number() | binary() | array() | object().
-type array() :: [value()].
-type object() :: #{key() := value()}.
-type key() :: binary().

-spec parse(binary()) -> {ok, value()} | {error, yaml:error_reason()}.
parse(Data) ->
  parse(Data, #{}).

-spec parse(binary(), yaml:parsing_options()) ->
        {ok, [value()]} | {error, yaml:error_reason()}.
parse(Data, Options) ->
  case yaml_parser:parse(Data, Options) of
    {ok, DocumentValues} ->
      try
        {ok, lists:map(fun build_1/1, DocumentValues)}
      catch
        throw:{error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec build(yaml:document()) -> {ok, value()} | {error, yaml:error_reason()}.
build(Document) ->
  try
    {ok, build_1(Document)}
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

-spec build_1(yaml:document()) -> value().
build_1(Document) ->
  build_value(Document).

-spec build_value(yaml:value()) -> value().
build_value(null) ->
  null;
build_value(Value) when is_boolean(Value) ->
  Value;
build_value(Value) when is_number(Value) ->
  Value;
build_value(Value) when is_binary(Value) ->
  Value;
build_value(Value) when is_list(Value) ->
  lists:map(fun build_value/1, Value);
build_value(Value) when is_map(Value) ->
  maps:fold(fun
              (K, V, Acc) when is_binary(K) ->
               Acc#{K => V};
              (K, _, _) ->
               throw({error, {invalid_json_key, K}})
           end, #{}, Value);
build_value(Value) ->
  throw({error, {invalid_json_value, Value}}).
