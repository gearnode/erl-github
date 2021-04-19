-module(github_jsv).

-export([catalog/0]).

-spec catalog() -> jsv:catalog().
catalog() ->
  #{ref_type => ref_type_definition(),
    actor => actor_definition(),
    event => event_definition(),
    events => {array, #{element => {ref, event}}},
    event_repo => event_repo_definition(),
    event_payload_commit_comment => event_payload_commit_definition(),
    event_payload_create => event_payload_create_definition(),
    event_payload_delete => event_payload_delete_definition(),
    event_payload_fork => event_payload_fork_definition(),
    event_payload_gollum => event_payload_gollum_definition(),
    event_payload_issue_comment => event_payload_issue_comment_definition(),
    event_payload_issues => event_payload_issues_definition(),
    event_payload_member => event_payload_member_definition(),
    event_payload_public => event_payload_public_definition(),
    event_payload_pull_request => event_payload_pull_request_definition(),
    event_payload_pull_request_review =>
      event_payload_pull_request_review_definition(),
    event_payload_pull_request_review_comment =>
      event_payload_pull_request_review_comment_definition(),
    event_payload_push => event_payload_push_definition(),
    event_payload_release => event_payload_release_definition(),
    event_payload_sponsorship => event_payload_sponsorship_definition(),
    event_payload_watch => event_payload_watch_definition()}.

-spec ref_type_definition() -> jsv:definition().
ref_type_definition() ->
  %% Yes, "repository" is used in specific cases, with the ref field usually
  %% set to null.
  {string, #{values => [branch, tag, repository]}}.

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
       [id, actor, repo, payload, public]},
   #{validate => fun github_events:validate/1,
     generate => fun github_events:generate/1}}.

-spec event_repo_definition() -> jsv:definition().
event_repo_definition() ->
  {object,
   #{members =>
       #{id => integer,
         name => string,
         url => uri},
     required =>
       [id, name, url]}}.

-spec event_payload_commit_definition() -> jsv:definition().
event_payload_commit_definition() ->
  {object,
   #{members =>
       #{action => {string, #{values => [created]}},
         comment => object}, % TODO
     required =>
       []}}.

-spec event_payload_create_definition() -> jsv:definition().
event_payload_create_definition() ->
  {object,
   #{members =>
       #{ref => string,
         ref_type => {ref, ref_type},
         master_branch => string,
         description => string},
     required =>
       []}}.

-spec event_payload_delete_definition() -> jsv:definition().
event_payload_delete_definition() ->
  {object,
   #{members =>
       #{ref => string,
         ref_type => {ref, ref_type}},
     required =>
       []}}.

-spec event_payload_fork_definition() -> jsv:definition().
event_payload_fork_definition() ->
  {object,
   #{members =>
       #{forkee => object}, % TODO
     required =>
       []}}.

-spec event_payload_gollum_definition() -> jsv:definition().
event_payload_gollum_definition() ->
  {object,
   #{members =>
       #{pages => array}, % TODO
     required =>
       []}}.

-spec event_payload_issue_comment_definition() -> jsv:definition().
event_payload_issue_comment_definition() ->
  {object,
   #{members =>
       #{action => {string, #{values => [created, edited, deleted]}},
         changes => object, % TODO
         issue => object, % TODO
         comment => object}, % TODO
     required =>
       []}}.

-spec event_payload_issues_definition() -> jsv:definition().
event_payload_issues_definition() ->
  {object,
   #{members =>
       #{action => {string, #{values => [opened,
                                         closed,
                                         reopened,
                                         assigned,
                                         unassigned,
                                         labeled,
                                         unlabeled]}},
         issue => object, % TODO
         changes => object, % TODO
         assignee => object, % TODO
         label => object}, % TODO
     required =>
       []}}.

-spec event_payload_member_definition() -> jsv:definition().
event_payload_member_definition() ->
  {object,
   #{members =>
       #{action => {string, #{values => [added]}},
         member => object, % TODO
         changes => object}, % TODO
     required =>
       []}}.

-spec event_payload_public_definition() -> jsv:definition().
event_payload_public_definition() ->
  {object,
   #{members =>
       #{}}}.

-spec event_payload_pull_request_definition() -> jsv:definition().
event_payload_pull_request_definition() ->
  {object,
   #{members =>
       #{action => {string, #{values => [opened,
                                         closed,
                                         reopened,
                                         assigned,
                                         unassigned,
                                         review_requested,
                                         review_request_removed,
                                         labeled,
                                         unlabeled,
                                         synchronize]}},
         number => integer,
         changes => object, % TODO
         pull_request => object},
     required =>
       []}}.

-spec event_payload_pull_request_review_definition() -> jsv:definition().
event_payload_pull_request_review_definition() ->
  {object,
   #{members =>
       #{action => {string, #{values => [created]}},
         pull_request => object, % TODO
         review => object}, % TODO
     required =>
       []}}.

-spec event_payload_pull_request_review_comment_definition() ->
        jsv:definition().
event_payload_pull_request_review_comment_definition() ->
  {object,
   #{members =>
       #{action => {string, #{values => [created]}},
         changes => object, % TODO
         pull_request => object, % TODO
         comment => object}, % TODO
     required =>
       []}}.

-spec event_payload_push_definition() -> jsv:definition().
event_payload_push_definition() ->
  {object,
   #{members =>
       #{push_id => integer,
         size => {integer, #{min => 0}},
         distinct_size => {integer, #{min => 0}},
         ref => string,
         head => string,
         before => string,
         commits => array}, % TODO
     required =>
       []}}.

-spec event_payload_release_definition() -> jsv:definition().
event_payload_release_definition() ->
  {object,
   #{members =>
       #{action => {string, #{values => [published]}},
         changes => object, % TODO
         release => object}, % TODO
     required =>
       []}}.

-spec event_payload_sponsorship_definition() -> jsv:definition().
event_payload_sponsorship_definition() ->
  {object,
   #{members =>
       #{action => {string, #{values => [created]}},
         effective_date => datetime,
         changes => object}, % TODO
     required =>
       []}}.

-spec event_payload_watch_definition() -> jsv:definition().
event_payload_watch_definition() ->
  {object,
   #{members =>
       #{action => {string, #{values => [started]}}},
     required =>
       []}}.
