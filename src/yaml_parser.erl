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

-module(yaml_parser).

-export([parse/2]).

-export_type([value_decorator/0]).

-type value_decorator() ::
        fun((yaml:value(), yaml_ast:tree_node()) -> yaml:value()).

-spec parse(binary(), yaml:parsing_options()) ->
        {ok, [yaml:document()]} | {error, yaml:error_reason()}.
parse(Data, Options0) ->
  DefaultOptions = #{schema => yaml:core_schema()},
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
node_value(Node, Options) ->
  Value = decode_value(Node, Options),
  decorate(Value, Node, Options).

-spec decode_value(yaml_ast:tree_node(), yaml:parsing_options()) ->
        yaml:value().
decode_value(Node = #{data := {sequence, Children}}, Options) ->
  Sequence = [node_value(Child, Options) || Child <- Children],
  decode_collection(Sequence, Node, Options);
decode_value(Node = #{data := {mapping, Pairs}}, Options) ->
  Mapping = lists:foldl(fun ({KeyNode, ValueNode}, Acc) ->
                            Key = node_value(KeyNode, Options),
                            Value = node_value(ValueNode, Options),
                            Acc#{Key => Value}
                        end, #{}, Pairs),
  decode_collection(Mapping, Node, Options);
decode_value(Node = #{data := {scalar, Value, Style}}, Options) ->
  decode_scalar(Value, Style, Node, Options).

-spec decode_collection(Value, yaml_ast:tree_node(), yaml:parsing_options()) ->
        yaml:value() when
    Value :: yaml:sequence() | yaml:mapping().
decode_collection(Value, Node = #{tag := _}, Options) ->
  decode_tagged_value(Value, Node, Options);
decode_collection(Value, _, _) ->
  Value.

-spec decode_scalar(binary(), plain | non_plain, yaml_ast:tree_node(),
                    yaml:parsing_options()) ->
        yaml:value().
decode_scalar(Value, _, Node = #{tag := _}, Options) ->
  decode_tagged_value(Value, Node, Options);
decode_scalar(Value, non_plain, _, _) ->
  Value;
decode_scalar(Value, plain, Node,
              Options = #{schema := #{plain_scalar_identifier := Id}}) ->
  case Id(Value) of
    {tag, Tag} ->
      decode_tagged_value(Value, Node#{tag => Tag}, Options);
    {value, Term} ->
      Term
  end.

-spec decode_tagged_value(Value, yaml_ast:tree_node(),
                          yaml:parsing_options()) ->
        yaml:value() when
    Value :: binary() | yaml:sequence() | yaml:mapping().
decode_tagged_value(Value, #{tag := Tag, position := Position},
                    #{schema := #{tagged_value_decoder := Decoder}}) ->
  case Decoder(Tag, Value) of
    {ok, Term} ->
      Term;
    {error, Reason} ->
      throw({error, yaml:error({invalid_value, Reason, Tag, Value},
                               Position)});
    unknown_tag ->
      throw({error, yaml:error({unknown_tag, Tag}, Position)})
  end.

-spec decorate(yaml:value(), yaml_ast:tree_node(), yaml:parsing_options()) ->
        yaml:value().
decorate(Value, Node, #{value_decorator := Decorator}) ->
  Decorator(Value, Node);
decorate(Value, _, _) ->
  Value.
