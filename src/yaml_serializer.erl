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

-module(yaml_serializer).

-export([serialize/2]).

-spec serialize([yaml:document()], yaml:serialization_options()) ->
        {ok, binary()} | {error, yaml:error_reason()}.
serialize(Documents, Options) ->
  try
    Emitter1 = yaml_nif:new_emitter(),
    Emitter2 = emit(Emitter1, stream_start, #{encoding => utf8}),
    Emitter3 = process_documents(Documents, Emitter2, Options),
    Emitter4 = emit(Emitter3, stream_end),
    {ok, yaml_nif:emitter_data(Emitter4)}
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

-spec process_documents([yaml:document()], yaml_nif:emitter(),
                        yaml:serialization_options()) ->
        yaml_nif:emitter().
process_documents([], Emitter, _Options) ->
  Emitter;
process_documents([Document | Documents], Emitter, Options) ->
  StartData = #{implicit => false},
  EndData = #{implicit => true},
  Emitter2 = emit(Emitter, document_start, StartData),
  Emitter3 = process_value(Document, Emitter2, Options),
  Emitter4 = emit(Emitter3, document_end, EndData),
  process_documents(Documents, Emitter4, Options).

-spec process_value(yaml:value(), yaml_nif:emitter(),
                    yaml:serialization_options()) ->
        yaml_nif:emitter().
process_value(Sequence, Emitter, Options) when is_list(Sequence) ->
  StartData = #{implicit => true,
                style => block},
  Emitter2 = emit(Emitter, sequence_start, StartData),
  Emitter3 = lists:foldl(fun (Element, E) ->
                             process_value(Element, E, Options)
                         end, Emitter2, Sequence),
  emit(Emitter3, sequence_end);
process_value(Mapping, Emitter, Options) when is_map(Mapping) ->
  StartData = #{implicit => true,
                style => block},
  Emitter2 = emit(Emitter, mapping_start, StartData),
  Emitter3 = maps:fold(fun (Key, Value, E) ->
                           E2 = process_value(Key, E, Options),
                           process_value(Value, E2, Options)
                       end, Emitter2, Mapping),
  emit(Emitter3, mapping_end);
process_value(Scalar, Emitter, _Options) when is_boolean(Scalar) ->
  emit_scalar(Emitter, atom_to_binary(Scalar), plain);
process_value(Scalar, Emitter, _Options) when is_integer(Scalar) ->
  emit_scalar(Emitter, integer_to_binary(Scalar), plain);
process_value(Scalar, Emitter, _Options) when is_float(Scalar) ->
  emit_scalar(Emitter, float_to_binary(Scalar), plain);
process_value(Scalar, Emitter, _Options) when is_binary(Scalar) ->
  case binary:match(Scalar, <<"\n">>) of
    {_, _} ->
      emit_scalar(Emitter, Scalar, literal);
    nomatch ->
      emit_scalar(Emitter, Scalar, double_quoted)
  end;
process_value(Scalar, Emitter, _Options) when is_atom(Scalar) ->
  emit_scalar(Emitter, atom_to_binary(Scalar), double_quoted);
process_value(Scalar, Emitter, _Options) ->
  throw({error, {unserializable_value, Scalar}}).

-spec emit(yaml_nif:emitter(), yaml_events:event_type()) -> yaml_nif:emitter().
emit(Emitter, Type) ->
  emit(Emitter, Type, undefined).

-spec emit(yaml_nif:emitter(),
           yaml_events:event_type(), yaml_events:event_data() | undefined) ->
        yaml_nif:emitter().
emit(Emitter, Type, Data) ->
  case yaml_nif:emit(Emitter, Type, Data) of
    {ok, Emitter2} ->
      Emitter2;
    {error, Reason} ->
      throw({error, Reason})
  end.

-spec emit_scalar(yaml_nif:emitter(), binary(), yaml_events:scalar_style()) ->
        yaml_nif:emitter().
emit_scalar(Emitter, Value, Style) ->
  Data = #{value => Value,
           length => byte_size(Value),
           plain_implicit => true,
           quoted_implicit => true,
           style => Style},
  emit(Emitter, scalar, Data).
