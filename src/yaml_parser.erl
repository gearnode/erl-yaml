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

-module(yaml_parser).

-export([parse/2]).

-spec parse(binary(), yaml:parsing_options()) ->
        {ok, [yaml:document()]} | {error, yaml:error_reason()}.
parse(Data, Options0) ->
  DefaultOptions = #{schema => yaml:failsafe_schema()}, % TODO use core schema
  Options = maps:merge(DefaultOptions, Options0),
  case yaml_events:parse(Data) of
    {ok, Events} ->
      case yaml_ast:build(Events, Options) of
        {ok, ASTDocuments} ->
          try
            {ok, [document_value(D, Options) || D <- ASTDocuments]}
          catch
            throw:{error, Reason} ->
              {error, Reason}
          end;
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec document_value(yaml_ast:document(), yaml:parsing_options()) ->
        yaml:document().
document_value(#{root := Node}, Options) ->
  node_value(Node, Options).

-spec node_value(yaml_ast:tree_node(), yaml:parsing_options()) -> yaml:value().
node_value(Node = #{data := {sequence, Children}}, Options) ->
  Sequence = [node_value(Child, Options) || Child <- Children],
  decode_collection(Sequence, Node, Options);
node_value(Node = #{data := {mapping, Pairs}}, Options) ->
  Mapping = lists:foldl(fun ({KeyNode, ValueNode}, Acc) ->
                            Key = node_value(KeyNode, Options),
                            Value = node_value(ValueNode, Options),
                            Acc#{Key => Value}
                        end, #{}, Pairs),
  decode_collection(Mapping, Node, Options);
node_value(Node = #{data := {scalar, Value, Style}}, Options) ->
  decode_scalar(Value, Style, Node, Options).

-spec decode_collection(Value, yaml_ast:tree_node(), yaml:parsing_options()) ->
        yaml:value() when
    Value :: yaml:sequence() | yaml:mapping().
decode_collection(Value, #{tag := Tag}, Options) ->
  decode_tagged_value(Value, Tag, Options);
decode_collection(Value, _, _) ->
  Value.

-spec decode_scalar(binary(), plain | non_plain, yaml_ast:tree_node(),
                    yaml:parsing_options()) ->
        yaml:value().
decode_scalar(Value, _, #{tag := Tag}, Options) ->
  decode_tagged_value(Value, Tag, Options);
decode_scalar(Value, non_plain, _, _) ->
  Value;
decode_scalar(Value, plain, _,
              Options = #{schema := #{plain_scalar_identifier := Id}}) ->
  decode_tagged_value(Value, Id(Value), Options).

-spec decode_tagged_value(Value, yaml:tag(), yaml:parsing_options()) ->
        yaml:value() when
    Value :: binary() | yaml:sequence() | yaml:mapping().
decode_tagged_value(Value, Tag,
                    #{schema := #{tagged_value_decoder := Decoder}}) ->
  case Decoder(Tag, Value) of
    {ok, Term} ->
      Term;
    {error, Reason} ->
      throw({error, {invalid_value, Reason, Tag, Value}});
    unknown_tag ->
      throw({error, {unknown_tag, Tag}})
  end.
