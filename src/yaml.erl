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

-module(yaml).

-export([libyaml_version/0, libyaml_version_string/0,
         is_version_supported/1,
         parse/1, parse/2]).

-export_type([version/0,
              document/0, value/0, scalar/0, sequence/0, mapping/0,
              parsing_options/0,
              tag/0, tag_decoder/0,
              position/0, error_reason/0]).

-type version() :: {non_neg_integer(), non_neg_integer()}.

-type document() :: value().
-type value() :: scalar() | sequence() | mapping().
-type scalar() :: term().
-type sequence() :: [value()].
-type mapping() :: #{value() := value()}.

-type parsing_options() ::
        #{tag_decoders => #{tag() := tag_decoder()}}.

-type tag() :: binary().

-type tag_decoder() ::
        fun((tag(), binary() | sequence() | mapping()) ->
               {ok, value()} | {error, term()}).

-type position() :: {Line :: pos_integer(),
                     Column :: pos_integer(),
                     Offset :: non_neg_integer()}.

-type error_reason() ::
        memory_error
      | {syntax_error, binary(), position()}
      | {unsupported_encoding, yaml_events:encoding()}
      | {unsupported_version, version()}
      | {unknown_alias, binary(), position()}
      | {unknown_tag, tag()}.

-spec libyaml_version() -> {integer(), integer(), integer()}.
libyaml_version() ->
  yaml_nif:get_version().

-spec libyaml_version_string() -> binary().
libyaml_version_string() ->
  yaml_nif:get_version_string().

-spec is_version_supported(version()) -> boolean().
is_version_supported({1, _}) ->
  true;
is_version_supported(_) ->
  false.

-spec parse(binary()) -> {ok, [document()]} | {error, error_reason()}.
parse(Data) ->
  parse(Data, #{}).

-spec parse(binary(), parsing_options()) ->
        {ok, [document()]} | {error, error_reason()}.
parse(Data, Options) ->
  yaml_parser:parse(Data, Options).
