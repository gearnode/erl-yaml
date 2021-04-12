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

-export([parse/1, parse/2, decorate_value/2]).

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
parse(Data, Options0) ->
  Options = Options0#{value_decorator => fun decorate_value/2},
  case yaml_parser:parse(Data, Options) of
    {ok, DocumentValues} ->
      try
        {ok, lists:map(fun build/1, DocumentValues)}
      catch
        throw:{error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec build(yaml:document()) -> value().
build(Document) ->
  build_value(Document).

-spec build_value(yaml:value()) -> value().
build_value({null, _}) ->
  null;
build_value({Value, _}) when is_boolean(Value) ->
  Value;
build_value({Value, _}) when is_number(Value) ->
  Value;
build_value({Value, _}) when is_binary(Value) ->
  Value;
build_value({Value, _}) when is_list(Value) ->
  lists:map(fun build_value/1, Value);
build_value({Value, _}) when is_map(Value) ->
  maps:fold(fun
              ({K, _}, DV, Acc) when is_binary(K) ->
                Acc#{K => build_value(DV)};
              (DK = {_, Position}, _, _) ->
                K = undecorate_value(DK),
                throw({error, yaml:error({invalid_json_key, K}, Position)})
           end, #{}, Value);
build_value(DecoratedValue = {_, Position}) ->
  Value = undecorate_value(DecoratedValue),
  throw({error, yaml:error({invalid_json_value, Value}, Position)}).

-spec decorate_value(yaml:value(), yaml_ast:tree_node()) -> yaml:value().
decorate_value(Value, #{position := Position}) ->
  {Value, Position}.

-spec undecorate_value({yaml:value(), yaml:position()}) -> yaml:value().
undecorate_value({Values, _}) when is_list(Values) ->
  lists:map(fun undecorate_value/1, Values);
undecorate_value({Values, _}) when is_map(Values) ->
  maps:fold(fun (K, V, Acc) ->
                Acc#{undecorate_value(K) => undecorate_value(V)}
            end, #{}, Values);
undecorate_value({Value, _}) ->
  Value.
