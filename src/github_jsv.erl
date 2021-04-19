-module(github_jsv).

-export([catalog/0]).

-spec catalog() -> jsv:catalog().
catalog() ->
  #{actor => actor_definition(),
    event => event_definition(),
    events => {array, #{element => {ref, event}}},
    event_repo => event_repo_definition()}.

-spec actor_definition() -> jsv:definition().
actor_definition() ->
  {object,
   #{members =>
       #{id => integer,
         login => string,
         display_login => string,
         gravatar_id => string,
         url => uri,
         avatar_url => uri},
     required =>
       [id, login, url, avatar_url]}}.

-spec event_definition() -> jsv:definition().
event_definition() ->
  {object,
   #{members =>
       #{id => string,
         type => string,
         actor => {ref, actor},
         repo => {ref, event_repo},
         org => {ref, actor},
         payload => object,
         public => boolean,
         created_at => datetime},
     required =>
       [id, actor, repo, payload, public]}}.

-spec event_repo_definition() -> jsv:definition().
event_repo_definition() ->
  {object,
   #{members =>
       #{id => integer,
         name => string,
         url => uri},
     required =>
       [id, name, url]}}.
