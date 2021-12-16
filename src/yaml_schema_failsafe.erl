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

-module(yaml_schema_failsafe).

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
decode_tagged_value(_, _) ->
  unknown_tag.

-spec identify_plain_scalar(binary()) -> yaml:plain_scalar_identifier_result().
identify_plain_scalar(Value) ->
  {value, Value}.
