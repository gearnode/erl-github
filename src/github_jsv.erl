-module(github_jsv).

-export([catalog/0]).

%% We do *NOT* use the JSV URI type because GitHub will regularly include
%% empty strings in fields which are supposed to be URIs. Nothing we can do
%% about it.

-spec catalog() -> jsv:catalog().
catalog() ->
  #{ref_type => ref_type_definition(),
    author_association => author_association_definition(),
    reaction_rollup => reaction_rollup_definition(),
    simple_user => simple_user_definition(),
    repository => repository_definition(),
    commit => commit_definition(),
    commit_author => commit_author_definition(),
    commit_stats => commit_stats_definition(),
    commit_comment => commit_comment_definition(),
    issue => issue_definition(),
    issue_comment => issue_comment_definition(),
    pull_request => pull_request_definition(),
    release => release_definition(),
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
    event_payload_watch => event_payload_watch_definition(),
    hook => hook_definition(),
    hooks => {array, #{element => {ref, hook}}},
    hook_config => hook_config_definition(),
    hook_response => hook_response_definition(),
    org_hook => org_hook_definition(),
    org_hooks => {array, #{element => {ref, org_hook}}},
    org_hook_config => org_hook_config_definition()}.

-spec ref_type_definition() -> jsv:definition().
ref_type_definition() ->
  %% Yes, "repository" is used in specific cases, with the ref field usually
  %% set to null.
  {string, #{values => [branch, tag, repository]}}.

-spec author_association_definition() -> jsv:definition().
author_association_definition() ->
  {string, #{values => ['COLLABORATOR',
                        'CONTRIBUTOR',
                        'FIRST_TIMER',
                        'FIRST_TIME_CONTRIBUTOR',
                        'MANNEQUIN',
                        'MEMBER',
                        'NONE',
                        'OWNER']}}.

-spec reaction_rollup_definition() -> jsv:definition().
reaction_rollup_definition() ->
  {object,
   #{members =>
       #{url => string,
         total_count => integer,
         '+1' => integer,
         '-1' => integer,
         laugh => integer,
         confused => integer,
         heart => integer,
         hooray => integer,
         eyes => integer,
         rocket => integer},
     required =>
       [url, total_count, '+1', '-1', laugh, confused, heart, hooray,
        eyes, rocket]}}.

-spec simple_user_definition() -> jsv:definition().
simple_user_definition() ->
  {object,
   #{members =>
       #{login => string,
         id => integer,
         node_id => string,
         avatar_url => string,
         gravatar_id => string,
         url => string,
         html_url => string,
         following_url => string,
         followers_url => string,
         gists_url => string,
         starred_url => string,
         subscriptions_url => string,
         organizations_url => string,
         repos_url => string,
         events_url => string,
         received_events_url => string,
         type => string,
         site_admin => boolean,
         starred_at => datetime},
     required =>
       [avatar_url, events_url, followers_url, following_url, gists_url,
        html_url, id, node_id, login, organizations_url,
        received_events_url, repos_url, site_admin, starred_url,
        subscriptions_url, type, url]}}.

-spec repository_definition() -> jsv:definition().
repository_definition() ->
  {object,
   #{members =>
       #{id => integer,
         node_id => string,
         name => string,
         full_name => string,
         license => any, % TODO
         organization => {ref, simple_user},
         forks => integer,
         permissions =>
           {object, #{}}, % TODO
         owner => {ref, simple_user},
         private => boolean,
         html_url => string,
         description => string,
         fork => boolean,
         url => string,
         archive_url => string,
         assignees_url => string,
         blobs_url => string,
         branches_url => string,
         collaborators_url => string,
         comments_url => string,
         commits_url => string,
         compare_url => string,
         contents_url => string,
         contributors_url => string,
         deployments_url => string,
         downloads_url => string,
         events_url => string,
         forks_url => string,
         git_commits_url => string,
         git_refs_url => string,
         git_tags_url => string,
         git_url => string,
         issue_comment_url => string,
         issue_events_url => string,
         issues_url => string,
         keys_url => string,
         labels_url => string,
         languages_url => string,
         merges_url => string,
         milestones_url => string,
         notifications_url => string,
         pulls_url => string,
         releases_url => string,
         ssh_url => string,
         stargazers_url => string,
         statuses_url => string,
         subscribers_url => string,
         subscription_url => string,
         tags_url => string,
         teams_url => string,
         trees_url => string,
         clone_url => string,
         mirror_url => string,
         hooks_url => string,
         svn_url => string,
         homepage => string,
         language => string,
         forks_count => integer,
         stargazers_count => integer,
         watchers_count => integer,
         size => integer,
         default_branch => string,
         open_issues_count => integer,
         is_template => boolean,
         topics => {array, #{element => string}},
         has_issues => boolean,
         has_projects => boolean,
         has_wiki => boolean,
         has_pages => boolean,
         has_downloads => boolean,
         archived => boolean,
         disabled => boolean,
         visibility => string,
         pushed_at => datetime,
         created_at => datetime,
         updated_at => datetime,
         allow_rebase_merge => boolean,
         template_repository => object, % TODO
         temp_clone_token => string,
         allow_squash_merge => boolean,
         delete_branch_on_merge => boolean,
         allow_merge_commit => boolean,
         subscribers_count => integer,
         network_count => integer,
         open_issues => integer,
         watchers => integer,
         master_branch => string,
         starred_at => datetime},
     required =>
       [archive_url, assignees_url, blobs_url, branches_url,
        collaborators_url, comments_url, commits_url, compare_url,
        contents_url, contributors_url, deployments_url, downloads_url,
        events_url, fork, forks_url, full_name, git_commits_url, git_refs_url,
        git_tags_url, hooks_url, html_url, id, node_id, issue_comment_url,
        issue_events_url, issues_url, keys_url, labels_url, languages_url,
        merges_url, milestones_url, name, notifications_url, private,
        pulls_url, releases_url, stargazers_url, statuses_url,
        subscribers_url, subscription_url, tags_url, teams_url, trees_url,
        url, clone_url, default_branch, forks, forks_count, git_url,
        has_downloads, has_issues, has_projects, has_wiki, has_pages,
        archived, disabled, open_issues, open_issues_count, size, ssh_url,
        stargazers_count, svn_url, watchers, watchers_count]}}.

-spec commit_definition() -> jsv:definition().
commit_definition() ->
  {object,
   #{members =>
       #{url => string,
         sha => string,
         node_id => string,
         html_url => string,
         comments_url => string,
         commit => object, % TODO
         author => {ref, commit_author},
         committer => {ref, simple_user},
         parents => array, % TODO
         stats => {ref, commit_stats},
         files => array}, % TODO
     required =>
       [url, sha, node_id, html_url, comments_url, commit, parents]}}.

-spec commit_author_definition() -> jsv:definition().
commit_author_definition() ->
  {object,
   #{members =>
       #{name => string,
         email => string},
     required =>
       [name, email]}}.

-spec commit_stats_definition() -> jsv:definition().
commit_stats_definition() ->
  {object,
   #{members =>
       #{additions => integer,
         deletions => integer,
         total => integer}}}.

-spec commit_comment_definition() -> jsv:definition().
commit_comment_definition() ->
  {object,
   #{members =>
       #{html_url => string,
         url => string,
         id => integer,
         node_id => string,
         body => string,
         path => string,
         position => integer,
         line => integer,
         commit_id => string,
         user => {ref, simple_user},
         created_at => datetime,
         updated_at => datetime,
         author_association => {ref, author_association},
         reactions => {ref, reaction_rollup}},
     required =>
       [url, html_url, id, node_id, commit_id, body, author_association,
        created_at, updated_at]}}.

-spec issue_definition() -> jsv:definition().
issue_definition() ->
  {object,
   #{members =>
       #{id => integer,
         node_id => string,
         url => string,
         repository_url => string,
         labels_url => string,
         comments_url => string,
         events_url => string,
         html_url => string,
         number => integer,
         state => string,
         title => string,
         body => string,
         user => {ref, simple_user},
         labels => any, % TODO
         assignee => {ref, simple_user},
         assignees => {array, #{element => {ref, simple_user}}},
         milestone => any, % TODO
         locked => boolean,
         active_lock_reason => string,
         comments => integer,
         pull_request => object, % TODO
         closed_at => datetime,
         created_at => datetime,
         updated_at => datetime,
         closed_by => {ref, simple_user},
         body_html => string,
         body_text => string,
         timeline_url => string,
         repository => {ref, repository},
         performed_via_github_app => any, % TODO
         author_association => {ref, author_association},
         reactions => {ref, reaction_rollup}},
     required =>
       [comments, comments_url, events_url, html_url, id, node_id, labels,
        labels_url, number, repository_url, state, locked, title, url,
        author_association, created_at, updated_at]}}.

-spec issue_comment_definition() -> jsv:definition().
issue_comment_definition() ->
  {object,
   #{members =>
       #{id => integer,
         node_id => string,
         url => string,
         body => string,
         body_text => string,
         body_html => string,
         html_url => string,
         user => {ref, simple_user},
         created_at => datetime,
         updated_at => datetime,
         issue_url => string,
         author_association => {ref, author_association},
         performed_via_github_app => any, % TODO
         reactions => {ref, reaction_rollup}},
     required =>
       [id, node_id, html_url, issue_url, author_association, url,
        created_at, updated_at]}}.

-spec pull_request_definition() -> jsv:definition().
pull_request_definition() ->
  {object,
   #{members =>
       #{url => string,
         id => integer,
         node_id => string,
         html_url => string,
         diff_url => string,
         patch_url => string,
         issue_url => string,
         commits_url => string,
         review_comments_url => string,
         review_comment_url => string,
         comments_url => string,
         statuses_url => string,
         number => integer,
         state => {string, #{values => [open, closed]}},
         locked => boolean,
         title => string,
         user => {ref, simple_user},
         body => string,
         labels => array, % TODO
         milestone => any, % TODO
         active_lock_reason => string,
         created_at => datetime,
         updated_at => datetime,
         closed_at => datetime,
         merged_at => datetime,
         merge_commit_sha => string,
         assignee => {ref, simple_user},
         assignees => {array, #{element => {ref, simple_user}}},
         requested_reviewers => {array, #{element => {ref, simple_user}}},
         requested_teams => {array, #{element => any}}, % TODO
         head => object, % TODO
         base => object, % TODO
         '_links' => object, % TODO
         author_association => {ref, author_association},
         auto_merge => any, % TODO
         draft => boolean,
         merged => boolean,
         mergeable => boolean,
         rebaseable => boolean,
         mergeable_state => string,
         merged_by => {ref, simple_user},
         comments => integer,
         review_comments => integer,
         maintainer_can_modify => boolean,
         commits => integer,
         additions => integer,
         deletions => integer,
         changed_files => integer},
     required =>
       ['_links', labels, base, commits_url, created_at, diff_url, head,
        html_url, id, node_id, issue_url, number, patch_url,
        review_comment_url, review_comments_url, statuses_url, state, locked,
        title, updated_at, url, author_association]}}.

-spec release_definition() -> jsv:definition().
release_definition() ->
  {object,
   #{members =>
       #{url => string,
         html_url => string,
         assets_url => string,
         upload_url => string,
         tarball_url => string,
         zipball_url => string,
         id => integer,
         node_id => string,
         tag_name => string,
         target_commitish => string,
         name => string,
         body => string,
         draft => boolean,
         prerelease => boolean,
         created_at => datetime,
         published_at => datetime,
         author => {ref, simple_user},
         assets => array, % TODO
         body_html => string,
         body_text => string,
         discussion_url => string},
     required =>
       [assets_url, upload_url, created_at, published_at, draft, id, node_id,
        author, html_url, prerelease, tag_name, target_commitish, assets,
        url]}}.

-spec actor_definition() -> jsv:definition().
actor_definition() ->
  {object,
   #{members =>
       #{id => integer,
         login => string,
         display_login => string,
         gravatar_id => string,
         url => string,
         avatar_url => string},
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
         url => string},
     required =>
       [id, name, url]}}.

-spec event_payload_commit_definition() -> jsv:definition().
event_payload_commit_definition() ->
  {object,
   #{members =>
       #{action => {string, #{values => [created]}},
         comment => {ref, commit_comment}},
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
       #{forkee => {ref, repository}},
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
         issue => {ref, issue},
         comment => {ref, issue_comment}},
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
         issue => {ref, issue},
         changes => object, % TODO
         assignee => {ref, simple_user},
         label => object}, % TODO
     required =>
       []}}.

-spec event_payload_member_definition() -> jsv:definition().
event_payload_member_definition() ->
  {object,
   #{members =>
       #{action => {string, #{values => [added]}},
         member => {ref, simple_user},
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
         pull_request => {ref, pull_request}},
     required =>
       []}}.

-spec event_payload_pull_request_review_definition() -> jsv:definition().
event_payload_pull_request_review_definition() ->
  {object,
   #{members =>
       #{action => {string, #{values => [created]}},
         pull_request => {ref, pull_request},
         review => object}, % TODO
     required =>
       []}}.

-spec event_payload_pull_request_review_comment_definition() ->
        jsv:definition().
event_payload_pull_request_review_comment_definition() ->
  {object,
   #{members =>
       #{action => {string, #{values => [created, edited]}},
         changes => object, % TODO
         pull_request =>  {ref, pull_request},
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
         commits =>
           {array,
            #{element =>
                {object,
                 #{members =>
                     #{url => string,
                       message => string,
                       sha => string,
                       author => {ref, commit_author},
                       distinct => boolean},
                   required =>
                     [url, message, sha, author, distinct]}}}}},
     required =>
       []}}.

-spec event_payload_release_definition() -> jsv:definition().
event_payload_release_definition() ->
  {object,
   #{members =>
       #{action => {string, #{values => [published]}},
         changes => object, % TODO
         release => {ref, release}},
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

-spec hook_definition() -> jsv:definition().
hook_definition() ->
  {object,
   #{members =>
       #{type => string,
         id => integer,
         name => string,
         active => boolean,
         events => {array, #{element => string}},
         config => {ref, hook_config},
         updated_at => datetime,
         created_at => datetime,
         url => uri,
         test_url => uri,
         ping_url => uri,
         last_response => {ref, hook_response}},
     required =>
       [id, url, type, name, active, events, config, ping_url,
        created_at, updated_at, last_response, test_url]}}.

-spec hook_config_definition() -> jsv:definition().
hook_config_definition() ->
  {object,
   #{members =>
       #{email => string,
         password => string,
         room => string,
         subdomain => string,
         url => uri,
         insecure_ssl => {one_of, [string, integer]},
         content_type => string,
         digest => string,
         secret => string,
         token => string},
     required =>
       []}}.

-spec hook_response_definition() -> jsv:definition().
hook_response_definition() ->
  {object,
   #{members =>
       #{code => string,
         status => string,
         message => string},
     required =>
       [code, status, message]}}.

-spec org_hook_definition() -> jsv:definition().
org_hook_definition() ->
  {object,
   #{members =>
       #{id => integer,
         url => uri,
         ping_url => string,
         name => string,
         events => {array, #{element => string}},
         active => boolean,
         config => {ref, org_hook_config},
         updated_at => datetime,
         created_at => datetime,
         type => string},
     required =>
       [id, url, type, name, active, events, config, ping_url,
        created_at, updated_at]}}.

-spec org_hook_config_definition() -> jsv:definition().
org_hook_config_definition() ->
  {object,
   #{members =>
       #{url => uri,
         insecure_ssl => string,
         content_type => string,
         secret => string},
     required =>
       []}}.
