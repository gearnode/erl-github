-module(github_hook_events).

-export([request_event/1,
         parse_event_type/1, parse_event/2, validate_event_request/2,
         jsv_definition/1]).

-export_type([event_type/0, event/0]).

-spec request_event(mhttp:request()) -> github:result({event_type(), event()}).
request_event(Request) ->
  case request_event_type(Request) of
    {ok, Type} ->
      Body = mhttp_request:body(Request),
      case parse_event(Body, Type) of
        {ok, Event} ->
          {ok, {Type, Event}};
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec request_event_type(mhttp:request()) -> github:result(event_type()).
request_event_type(Request) ->
  Header = mhttp_request:header(Request),
  case mhttp_header:find(Header, <<"X-Github-Event">>) of
    {ok, TypeString} ->
      case parse_event_type(TypeString) of
        {ok, Type} ->
          {ok, Type};
        error ->
          {error, {invalid_hook_event_type, TypeString}}
      end;
    error ->
      {error, missing_hook_event_type}
  end.

-type event_type() ::
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

-spec parse_event_type(binary()) -> {ok, event_type()} | error.
parse_event_type(<<"check_run">>) ->
  {ok, check_run};
parse_event_type(<<"check_suite">>) ->
  {ok, check_suite};
parse_event_type(<<"code_scanning_alert">>) ->
  {ok, code_scanning_alert};
parse_event_type(<<"commit_comment">>) ->
  {ok, commit_comment};
parse_event_type(<<"content_reference">>) ->
  {ok, content_reference};
parse_event_type(<<"create">>) ->
  {ok, create};
parse_event_type(<<"delete">>) ->
  {ok, delete};
parse_event_type(<<"deploy_key">>) ->
  {ok, deploy_key};
parse_event_type(<<"deployment">>) ->
  {ok, deployment};
parse_event_type(<<"deployment_status">>) ->
  {ok, deployment_status};
parse_event_type(<<"discussion">>) ->
  {ok, discussion};
parse_event_type(<<"discussion_comment">>) ->
  {ok, discussion_comment};
parse_event_type(<<"fork">>) ->
  {ok, fork};
parse_event_type(<<"github_app_authorization">>) ->
  {ok, github_app_authorization};
parse_event_type(<<"gollum">>) ->
  {ok, gollum};
parse_event_type(<<"installation">>) ->
  {ok, installation};
parse_event_type(<<"installation_repositories">>) ->
  {ok, installation_repositories};
parse_event_type(<<"issue_comment">>) ->
  {ok, issue_comment};
parse_event_type(<<"issues">>) ->
  {ok, issues};
parse_event_type(<<"label">>) ->
  {ok, label};
parse_event_type(<<"marketplace_purchase">>) ->
  {ok, marketplace_purchase};
parse_event_type(<<"member">>) ->
  {ok, member};
parse_event_type(<<"membership">>) ->
  {ok, membership};
parse_event_type(<<"meta">>) ->
  {ok, meta};
parse_event_type(<<"milestone">>) ->
  {ok, milestone};
parse_event_type(<<"organization">>) ->
  {ok, organization};
parse_event_type(<<"org_block">>) ->
  {ok, org_block};
parse_event_type(<<"package">>) ->
  {ok, package};
parse_event_type(<<"page_build">>) ->
  {ok, page_build};
parse_event_type(<<"ping">>) ->
  {ok, ping};
parse_event_type(<<"project_card">>) ->
  {ok, project_card};
parse_event_type(<<"project_column">>) ->
  {ok, project_column};
parse_event_type(<<"project">>) ->
  {ok, project};
parse_event_type(<<"public">>) ->
  {ok, public};
parse_event_type(<<"pull_request">>) ->
  {ok, pull_request};
parse_event_type(<<"pull_request_review">>) ->
  {ok, pull_request_review};
parse_event_type(<<"pull_request_review_comment">>) ->
  {ok, pull_request_review_comment};
parse_event_type(<<"push">>) ->
  {ok, push};
parse_event_type(<<"release">>) ->
  {ok, release};
parse_event_type(<<"repository_dispatch">>) ->
  {ok, repository_dispatch};
parse_event_type(<<"repository">>) ->
  {ok, repository};
parse_event_type(<<"repository_import">>) ->
  {ok, repository_import};
parse_event_type(<<"repository_vulnerability_alert">>) ->
  {ok, repository_vulnerability_alert};
parse_event_type(<<"secret_scanning_alert">>) ->
  {ok, secret_scanning_alert};
parse_event_type(<<"security_advisory">>) ->
  {ok, security_advisory};
parse_event_type(<<"sponsorship">>) ->
  {ok, sponsorship};
parse_event_type(<<"star">>) ->
  {ok, star};
parse_event_type(<<"status">>) ->
  {ok, status};
parse_event_type(<<"team">>) ->
  {ok, team};
parse_event_type(<<"team_add">>) ->
  {ok, team_add};
parse_event_type(<<"watch">>) ->
  {ok, watch};
parse_event_type(<<"workflow_dispatch">>) ->
  {ok, workflow_dispatch};
parse_event_type(<<"workflow_run">>) ->
  {ok, workflow_run};
parse_event_type(_) ->
  error.

-spec parse_event(binary(), event_type()) -> github:result(event()).
parse_event(Data, Type) ->
  case json:parse(Data) of
    {ok, Value} ->
      parse_event_value(Value, Type);
    {error, Reason} ->
      {error, {invalid_hook_event, {json, Reason, Data}}}
  end.

-spec parse_event_value(json:value(), event_type()) -> github:result(event()).
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

-spec validate_event_request(mhttp:request(), binary()) -> github:result().
validate_event_request(Request, Secret) ->
  Header = mhttp_request:header(Request),
  Body = mhttp_request:body(Request),
  case mhttp_header:find(Header, <<"X-Hub-Signature-256">>) of
    {ok, <<"sha256=", Signature/binary>>} ->
      ExpectedSignatureData = crypto:mac(hmac, sha256, Secret, Body),
      ExpectedSignature =
        string:lowercase(binary:encode_hex(ExpectedSignatureData)),
      if
        Signature =:= ExpectedSignature ->
          ok;
        true ->
          {error, invalid_hook_signature}
      end;
    {ok, Signature} ->
      {error, {invalid_hook_signature_format, Signature}};
    error ->
      {error, missing_hook_signature}
  end.

-spec jsv_definition(event_type()) -> {ok, jsv:definition()} | error.
jsv_definition(repository) ->
  {ok, {ref, github, hook_event_repository}};
jsv_definition(create) ->
  {ok, {ref, github, hook_event_create}};
jsv_definition(delete) ->
  {ok, {ref, github, hook_event_delete}};
jsv_definition(_) ->
  error.
