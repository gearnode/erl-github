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

-module(github_app).

-behaviour(application).

-export([start/2, stop/1]).

-export_type([]).

start(_StartType, _Args) ->
  try
    register_jsv_catalogs(),
    github_sup:start_link()
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

stop(_State) ->
  unregister_jsv_catalogs().

-spec register_jsv_catalogs() -> ok | {error, term()}.
register_jsv_catalogs() ->
  Catalogs = catalogs(),
  lists:foreach(fun ({Name, Catalog}) ->
                    jsv:register_catalog(Name, Catalog)
                end, Catalogs),
  lists:foreach(fun ({Name, _}) ->
                    case jsv:verify_catalog(Name, #{}) of
                      ok ->
                        ok;
                      {error, Reason} ->
                        throw({error, {invalid_jsv_catalog, Reason, Name}})
                    end
                end, Catalogs).

-spec unregister_jsv_catalogs() -> ok.
unregister_jsv_catalogs() ->
  Catalogs = catalogs(),
  lists:foreach(fun ({Name, _}) ->
                    jsv:unregister_catalog(Name)
                end, Catalogs).

-spec catalogs() -> [{jsv:catalog_name(), jsv:catalog()}].
catalogs() ->
  [{github, github_jsv:catalog()}].
