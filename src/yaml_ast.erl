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

-module(yaml_ast).

-export([build/1, build/2]).

-export_type([options/0]).

-type options() ::
        #{}.

-type document() ::
        #{root := tree_node()}.

-type tree_node() ::
        #{data := node_data(),
          position := yaml:position(),
          anchor => binary()}.

-type node_data() ::
        {scalar, yaml:scalar()}
      | {sequence, [tree_node()]}
      | {mapping, [{tree_node(), tree_node() | undefined}]}.

-type state() ::
        #{options := options(),
          documents := [document()],
          stack := [tree_node()],
          anchors := #{binary() := tree_node()}}.

-spec build([yaml_events:event()]) ->
        {ok, [document()]} | {error, yaml:error_reason()}.
build(Events) ->
  build(Events, #{}).

-spec build([yaml_events:event()], options()) ->
        {ok, [document()]} | {error, yaml:error_reason()}.
build(Events, Options) ->
  State = #{options => Options,
            documents => [],
            stack => [],
            anchors => #{}},
  try
    #{documents := Documents} = build_stream(Events, State),
    {ok, Documents}
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

-spec build_stream([yaml_events:event()], state()) -> state().
build_stream([Event = #{type := stream_start} | Events], State) ->
  check_stream_encoding(Event),
  maybe_build_document(Events, State).

-spec maybe_build_document([yaml_events:event()], state()) -> state().
maybe_build_document([#{type := stream_end}],
                     State = #{documents := Documents}) ->
  State#{documents => lists:reverse(Documents)};
maybe_build_document([Event = #{type := document_start} | Events], State) ->
  check_document_version(Event),
  build_document_root(Events, State).

-spec build_document_root([yaml_events:event()], state()) -> state().
build_document_root(Events, State = #{documents := Documents}) ->
  {Root, Events2, State2} = build_node(Events, State),
  Document = #{root => Root},
  State3 = State2#{documents => [Document | Documents]},
  [#{type := document_end} | Events3] = Events2,
  maybe_build_document(Events3, State3#{anchors => #{}}).

-spec build_node([yaml_events:event()], state()) ->
        {tree_node(), [yaml_events:event()], state()}.
build_node([Event = #{type := alias, data := #{anchor := Anchor}} | Events],
           State = #{stack := Stack, anchors := Anchors}) ->
  case maps:find(Anchor, Anchors) of
    {ok, TargetNode} ->
      process_node(Events, State#{stack => [TargetNode | Stack]});
    error ->
      Position = mark_position(maps:get(start, Event)),
      throw({error, {unknown_alias, Anchor, Position}})
  end;
build_node([Event = #{type := scalar} | Events], State) ->
  Node = event_node(Event),
  process_node(Events, push_node(Node, State));
build_node([Event = #{type := Type} | Events], State) when
    Type =:= sequence_start; Type =:= mapping_start ->
  Node = event_node(Event),
  build_node(Events, push_node(Node, State));
build_node([#{type := sequence_end} | Events],
           State = #{stack := [Node | Nodes]}) ->
  #{data := {sequence, Children}} = Node,
  Node2 = Node#{data => {sequence, lists:reverse(Children)}},
  process_node(Events, State#{stack => [Node2 | Nodes]});
build_node([#{type := mapping_end} | Events],
           State = #{stack := [Node | Nodes]}) ->
  #{data := {mapping, Pairs}} = Node,
  Node2 = Node#{data => {mapping, lists:reverse(Pairs)}},
  process_node(Events, State#{stack => [Node2 | Nodes]}).

-spec event_node(yaml_events:event()) -> tree_node().
event_node(#{type := Type, data := Data, start := Mark}) ->
  NodeData = case Type of
               scalar ->
                 {scalar, maps:get(value, Data)};
               sequence_start ->
                 {sequence, []};
               mapping_start ->
                 {mapping, []}
             end,
  Node0 = #{data => NodeData,
            position => mark_position(Mark)},
  case maps:find(anchor, Data) of
    {ok, Anchor} ->
      Node0#{anchor => Anchor};
    error ->
      Node0
  end.

-spec push_node(tree_node(), state()) -> state().
push_node(Node = #{anchor := Anchor},
          State = #{stack := Stack, anchors := Anchors}) ->
  State#{stack => [Node | Stack],
         anchors => Anchors#{Anchor => Node}};
push_node(Node, State = #{stack := Stack}) ->
  State#{stack => [Node | Stack]}.

-spec process_node([yaml_events:event()], state()) ->
        {tree_node(), [yaml_events:event()], state()}.
process_node(Events, State = #{stack := [Node]}) ->
  {Node, Events, State#{stack => []}};
process_node(Events, State = #{stack := [Node | Nodes]}) ->
  build_node(Events, State#{stack => merge_node(Node, Nodes)}).

-spec merge_node(tree_node(), [tree_node()]) -> [tree_node()].
merge_node(Node, [Parent = #{data := {sequence, Children}} | Nodes]) ->
  Parent2 = Parent#{data => {sequence, [Node | Children]}},
  [Parent2 | Nodes];
merge_node(Node, [Parent = #{data := {mapping, Pairs}} | Nodes]) ->
  Pairs2 = case Pairs of
             [{Key, undefined} | Rest] ->
               [{Key, Node} | Rest];
             _ ->
               [{Node, undefined} | Pairs]
           end,
  Parent2 = Parent#{data => {mapping, Pairs2}},
  [Parent2 | Nodes].

-spec check_stream_encoding(yaml_events:event()) -> ok.
check_stream_encoding(#{type := stream_start, data := Data}) ->
  case maps:get(encoding, Data, utf8) of
    utf8 ->
      ok;
    Encoding ->
      throw({error, {unsupported_encoding, Encoding}})
  end.

-spec check_document_version(yaml_events:event()) -> ok.
check_document_version(#{type := document_start, data := Data}) ->
  case maps:find(version_directive, Data) of
    {ok, #{major := Major, minor := Minor}} ->
      Version = {Major, Minor},
      case yaml:is_version_supported(Version) of
        true ->
          ok;
        false ->
          throw({error, {unsupported_version, Version}})
      end;
    error ->
      true
  end.

-spec mark_position(yaml_events:mark()) -> yaml:position().
mark_position({_, Line, Column}) ->
  {Line+1, Column+1}.
