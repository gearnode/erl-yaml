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

-module(yaml_nif).

-export([get_version/0, get_version_string/0, parse/1]).

-on_load(init/0).

init() ->
  Path = filename:join(find_nif_directory(), "yaml_nif"),
  erlang:load_nif(Path, []).

-spec find_nif_directory() -> string().
find_nif_directory() ->
  PrivDir = code:priv_dir(yaml),
  case filelib:is_dir(PrivDir) of
    true ->
      %% If the private directory exists, we are in a release and the library
      %% is directly there.
      PrivDir;
    false ->
      %% If the private directory does not exists, we (probably) are in an
      %% escript. In that case, code:lib_dir/1 will return <app>/yaml where
      %% <app> is the name of the escript (no it does not make any sense since
      %% it is a file and not a directory, but there is nothing I can do about
      %% it).
      %%
      %% In that situation, we will arbitrarily look for the library in the
      %% directory of the escript file.
      LibDir = code:lib_dir(yaml),
      case filename:split(LibDir) of
        Parts when length(Parts) > 2 ->
          AppDir = filename:join(lists:droplast(lists:droplast(Parts))),
          case filelib:is_dir(AppDir) of
            true ->
              AppDir;
            false ->
              %% If we end up here, then the yaml module was packaged in a way
              %% we do not recognize. Contact me.
              error({directory_not_found, AppDir})
          end;
        _ ->
          %% Same thing here
          error({invalid_lib_dir, LibDir})
      end
  end.

-spec get_version() -> {integer(), integer(), integer()}.
get_version() ->
  erlang:nif_error(nif_not_loaded).

-spec get_version_string() -> binary().
get_version_string() ->
  erlang:nif_error(nif_not_loaded).

-spec parse(binary()) ->
        {ok, [yaml_events:event()]} | {error, yaml:error_reason()}.
parse(_Data) ->
  erlang:nif_error(nif_not_loaded).
