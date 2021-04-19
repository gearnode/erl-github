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
