-module(github_hook_events).

-export([parse_type/1, parse_event/2, jsv_definition/1]).

-export_type([type/0, event/0]).

-type type() ::
        check_run
      | check_suite
      | code_scanning_alert
      | commit_comment
      | content_reference
      | create
      | delete
      | deploy_key
      | deployment
      | deployment_status
      | discussion
      | discussion_comment
      | fork
      | github_app_authorization
      | gollum
      | installation
      | installation_repositories
      | issue_comment
      | issues
      | label
      | marketplace_purchase
      | member
      | membership
      | meta
      | milestone
      | organization
      | org_block
      | package
      | page_build
      | ping
      | project_card
      | project_column
      | project
      | public
      | pull_request
      | pull_request_review
      | pull_request_review_comment
      | push
      | release
      | repository_dispatch
      | repository
      | repository_import
      | repository_vulnerability_alert
      | secret_scanning_alert
      | security_advisory
      | sponsorship
      | star
      | status
      | team
      | team_add
      | watch
      | workflow_dispatch
      | workflow_run.

-type event() :: map().

-spec parse_type(binary()) -> {ok, type()} | error.
parse_type(<<"check_run">>) ->
  {ok, check_run};
parse_type(<<"check_suite">>) ->
  {ok, check_suite};
parse_type(<<"code_scanning_alert">>) ->
  {ok, code_scanning_alert};
parse_type(<<"commit_comment">>) ->
  {ok, commit_comment};
parse_type(<<"content_reference">>) ->
  {ok, content_reference};
parse_type(<<"create">>) ->
  {ok, create};
parse_type(<<"delete">>) ->
  {ok, delete};
parse_type(<<"deploy_key">>) ->
  {ok, deploy_key};
parse_type(<<"deployment">>) ->
  {ok, deployment};
parse_type(<<"deployment_status">>) ->
  {ok, deployment_status};
parse_type(<<"discussion">>) ->
  {ok, discussion};
parse_type(<<"discussion_comment">>) ->
  {ok, discussion_comment};
parse_type(<<"fork">>) ->
  {ok, fork};
parse_type(<<"github_app_authorization">>) ->
  {ok, github_app_authorization};
parse_type(<<"gollum">>) ->
  {ok, gollum};
parse_type(<<"installation">>) ->
  {ok, installation};
parse_type(<<"installation_repositories">>) ->
  {ok, installation_repositories};
parse_type(<<"issue_comment">>) ->
  {ok, issue_comment};
parse_type(<<"issues">>) ->
  {ok, issues};
parse_type(<<"label">>) ->
  {ok, label};
parse_type(<<"marketplace_purchase">>) ->
  {ok, marketplace_purchase};
parse_type(<<"member">>) ->
  {ok, member};
parse_type(<<"membership">>) ->
  {ok, membership};
parse_type(<<"meta">>) ->
  {ok, meta};
parse_type(<<"milestone">>) ->
  {ok, milestone};
parse_type(<<"organization">>) ->
  {ok, organization};
parse_type(<<"org_block">>) ->
  {ok, org_block};
parse_type(<<"package">>) ->
  {ok, package};
parse_type(<<"page_build">>) ->
  {ok, page_build};
parse_type(<<"ping">>) ->
  {ok, ping};
parse_type(<<"project_card">>) ->
  {ok, project_card};
parse_type(<<"project_column">>) ->
  {ok, project_column};
parse_type(<<"project">>) ->
  {ok, project};
parse_type(<<"public">>) ->
  {ok, public};
parse_type(<<"pull_request">>) ->
  {ok, pull_request};
parse_type(<<"pull_request_review">>) ->
  {ok, pull_request_review};
parse_type(<<"pull_request_review_comment">>) ->
  {ok, pull_request_review_comment};
parse_type(<<"push">>) ->
  {ok, push};
parse_type(<<"release">>) ->
  {ok, release};
parse_type(<<"repository_dispatch">>) ->
  {ok, repository_dispatch};
parse_type(<<"repository">>) ->
  {ok, repository};
parse_type(<<"repository_import">>) ->
  {ok, repository_import};
parse_type(<<"repository_vulnerability_alert">>) ->
  {ok, repository_vulnerability_alert};
parse_type(<<"secret_scanning_alert">>) ->
  {ok, secret_scanning_alert};
parse_type(<<"security_advisory">>) ->
  {ok, security_advisory};
parse_type(<<"sponsorship">>) ->
  {ok, sponsorship};
parse_type(<<"star">>) ->
  {ok, star};
parse_type(<<"status">>) ->
  {ok, status};
parse_type(<<"team">>) ->
  {ok, team};
parse_type(<<"team_add">>) ->
  {ok, team_add};
parse_type(<<"watch">>) ->
  {ok, watch};
parse_type(<<"workflow_dispatch">>) ->
  {ok, workflow_dispatch};
parse_type(<<"workflow_run">>) ->
  {ok, workflow_run};
parse_type(_) ->
  error.

-spec parse_event(binary(), type()) -> github:result(event()).
parse_event(Data, Type) ->
  case json:parse(Data) of
    {ok, Value} ->
      parse_event_value(Value, Type);
    {error, Reason} ->
      {error, {invalid_hook_event, {json, Reason, Data}}}
  end.

-spec parse_event_value(json:value(), type()) -> github:result(event()).
parse_event_value(Value, Type) ->
  case jsv_definition(Type) of
    {ok, Definition} ->
      Options = #{unknown_member_handling => keep,
                  null_member_handling => remove},
      case jsv:validate(Value, Definition, Options) of
        {ok, Event} ->
          {ok, Event};
        {error, Errors} ->
          {error, {invalid_hook_event, {jsv, Errors}, Value}}
      end;
    error ->
      {error, {unsupported_hook_event_type, Type}}
  end.

-spec jsv_definition(type()) -> {ok, jsv:definition()} | error.
jsv_definition(repository) ->
  {ok, {ref, github, hook_event_repository}};
jsv_definition(_) ->
  error.
