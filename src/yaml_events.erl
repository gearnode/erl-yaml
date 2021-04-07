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

-module(yaml_events).

-export([parse/1]).

-export_type([event_type/0, event/0, event/1, mark/0,
              stream_start/0, document_start/0, document_end/0, alias/0,
              scalar/0, sequence_start/0, mapping_start/0,
              encoding/0, version_directive/0, tag_directive/0,
              scalar_style/0, sequence_style/0, mapping_style/0]).

-type event_type() ::
        stream_start
      | stream_end
      | document_start
      | document_end
      | alias
      | scalar
      | sequence_start
      | sequence_end
      | mapping_start
      | mapping_end
      | {unknown, non_neg_integer()}.

-type event(Data) ::
        #{type := event_type(),
          data => Data,
          start := mark(),
          'end' := mark()}.

-type event() :: event(any()).

-type mark() ::
        {Index :: non_neg_integer(),
         Line :: non_neg_integer(),
         Column :: non_neg_integer()}.

-type stream_start() ::
        #{encoding => encoding()}.

-type document_start() ::
        #{version_directive => version_directive(),
          tag_directives => [tag_directive()],
          implicit := boolean()}.

-type document_end() ::
        #{implicit := boolean()}.

-type alias() ::
        #{anchor := binary()}.

-type scalar() ::
        #{anchor => binary(),
          tag => binary(),
          value := binary(),
          length := non_neg_integer(),
          plain_implicit := boolean(),
          quoted_implicit := boolean(),
          style := scalar_style()}.

-type sequence_start() ::
        #{anchor => binary(),
          tag => binary(),
          implicit := boolean(),
          style := sequence_style()}.

-type mapping_start() ::
        #{anchor => binary(),
          tag => binary(),
          implicit := boolean(),
          style := mapping_style()}.

-type encoding() ::
        any | utf8 | utf16le | utf16be | {unknown, non_neg_integer()}.

-type version_directive() ::
        #{major := integer(),
          minor := integer()}.

-type tag_directive() ::
        #{handle := binary(),
          prefix := binary()}.

-type scalar_style() ::
        any | plain | single_quoted | double_quoted | literal | folded
      | {unknown, non_neg_integer()}.

-type sequence_style() ::
        any | block | flow | {unknown, non_neg_integer()}.

-type mapping_style() ::
        any | block | flow | {unknown, non_neg_integer()}.

-spec parse(binary()) ->
        {ok, [event()]} | {error, yaml:error_reason()}.
parse(Data) ->
  yaml_nif:parse(Data).
