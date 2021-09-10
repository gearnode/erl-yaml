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
         parse/1, parse/2,
         serialize/1, serialize/2,
         failsafe_schema/0, core_schema/0,
         error/1, error/2,
         format_error/1, format_error_reason/1]).

-export_type([version/0,
              document/0, value/0, scalar/0, sequence/0, mapping/0,
              undecoded_value/0,
              parsing_options/0,
              schema/0, tagged_value_decoder/0,
              tagged_value_decoding_result/0,
              plain_scalar_identifier/0,
              plain_scalar_identifier_result/0,
              serialization_options/0,
              tag/0, position/0, error/0, error_reason/0]).

-type version() :: {non_neg_integer(), non_neg_integer()}.

-type document() :: value().
-type value() :: scalar() | sequence() | mapping().
-type scalar() :: term().
-type sequence() :: [value()].
-type mapping() :: #{value() := value()}.

-type undecoded_value() :: binary() | sequence() | mapping().

-type parsing_options() ::
        #{schema => schema(),
          value_decorator => yaml_parser:value_decorator()}.

-type schema() ::
        #{tagged_value_decoder := tagged_value_decoder(),
          plain_scalar_identifier := plain_scalar_identifier()}.

-type tagged_value_decoder() ::
        fun((tag(), undecoded_value()) -> tagged_value_decoding_result()).

-type tagged_value_decoding_result() ::
        {ok, value()} | {error, term()} | unknown_tag.

-type plain_scalar_identifier() ::
        fun((binary()) -> plain_scalar_identifier_result()).

-type plain_scalar_identifier_result() ::
        {tag, tag()} | {value, value()}.

-type serialization_options() ::
        #{return_binary => boolean()}.

-type tag() :: binary().

-type position() :: {Line :: pos_integer(),
                     Column :: pos_integer(),
                     Offset :: non_neg_integer()}.

-type error() ::
        #{reason := error_reason(),
          position => position()}.

-type error_reason() ::
        memory_error
      | {syntax_error, Description :: binary()}
      | {unsupported_encoding, yaml_events:encoding()}
      | {unsupported_version, version()}
      | {unknown_alias, binary()}
      | {unknown_tag, tag()}
      | {invalid_value, term(), tag(), value()}
      | {invalid_json_value, term()}
      | {invalid_json_key, term()}
      | serialization_error
      | {unserializable_value, term()}.

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

-spec serialize([document()]) ->
        {ok, binary()} | {error, error_reason()}.
serialize(Documents) ->
  serialize(Documents, #{}).

-spec serialize([document()], serialization_options()) ->
        {ok, binary()} | {error, error_reason()}.
serialize(Documents, Options) ->
  yaml_serializer:serialize(Documents, Options).

-spec failsafe_schema() -> schema().
failsafe_schema() ->
  yaml_schema_failsafe:schema().

-spec core_schema() -> schema().
core_schema() ->
  yaml_schema_core:schema().

-spec error(error_reason()) -> error().
error(Reason) ->
  #{reason => Reason}.

-spec error(error_reason(), position()) -> error().
error(Reason, Position) ->
  #{reason => Reason,
    position => Position}.

-spec format_error(error()) -> unicode:chardata().
format_error(#{position := {Line, Column, _}, reason := Reason}) ->
  io_lib:format("~b:~b: ~ts", [Line, Column, format_error_reason(Reason)]);
format_error(#{reason := Reason}) ->
  format_error_reason(Reason).

-spec format_error_reason(error_reason()) -> unicode:chardata().
format_error_reason(memory_error) ->
  "memory allocation failure";
format_error_reason({syntax_error, Msg}) ->
  <<"syntax error: ", Msg/binary>>;
format_error_reason({unsupported_encoding, Encoding}) ->
  io_lib:format("unsupported stream encoding ~ts", [Encoding]);
format_error_reason({unsupported_version, {Major, Minor}}) ->
  io_lib:format("unsupported document version ~b.~b", [Major, Minor]);
format_error_reason({unknown_tag, Tag}) ->
  io_lib:format("unknown tag ~ts", [Tag]);
format_error_reason({unknown_value, Reason, Tag, Value}) ->
  io_lib:format("invalid value ~ts for tag ~ts: ~0tp",
                [Value, Tag, Reason]);
format_error_reason({invalid_json_value, Value}) ->
  io_lib:format("invalid json value ~0tp", [Value]);
format_error_reason({invalid_json_key, Value}) ->
  io_lib:format("invalid json object key ~0tp", [Value]);
format_error_reason(serialization_error) ->
  %% Not much we can do about it, libyaml does not provide detailed error
  %% diagnostics.
  "serialization failure";
format_error_reason({unserializable_value, Value}) ->
  io_lib:format("unserializable value ~0tp", [Value]);
format_error_reason(Reason) ->
  io_lib:format("~0tp", [Reason]).
