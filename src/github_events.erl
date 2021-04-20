-module(github_events).

-export([list_public_events/1, list_repository_events/3,
         validate/1, generate/1]).

-export_type([actor/0, event/0, event/1, repo/0]).

-type actor() ::
        #{id := integer(),
          login := binary(),
          display_login => binary(),
          gravatar_id => binary(),
          url := binary(),
          avatar_url := binary()}.

-type event() :: event(map()).

-type event(Payload) ::
        #{id := binary(),
          type := binary(),
          actor := actor(),
          repo := repo(),
          org => actor(),
          payload := Payload,
          public := boolean(),
          created_at => calendar:datetime()}.

-type repo() ::
        #{id := integer(),
          name := string(),
          url := string()}.

-type event_response() ::
        #{events := [event()],
          poll_interval => non_neg_integer(), % seconds
          etag => binary()}.

-type event_options() ::
        #{per_page => pos_integer(),
          if_none_match => binary()}.

-spec list_public_events(event_options()) ->
        github:result(event_response() | not_modified).
list_public_events(Options) ->
  send_event_request(<<"/events">>, Options).

-spec list_repository_events(Owner :: binary(), Name :: binary(),
                             event_options()) ->
        github:result(event_response() | not_modified).
list_repository_events(Owner, Name, Options) ->
  OwnerPart = uri:encode_path(Owner),
  NamePart = uri:encode_path(Name),
  Path = ["/repos/", OwnerPart, $/, NamePart, "/events"],
  send_event_request(iolist_to_binary(Path), Options).

-spec send_event_request(uri:path(), event_options()) ->
        github:result(event_response() | not_modified).
send_event_request(Path, Options) ->
  PerPage = maps:get(per_page, Options, 10),
  Query = [{<<"per_page">>, integer_to_binary(PerPage)}],
  Target = #{path => Path, query => Query},
  RequestOptions0 = maps:with([if_none_match], Options),
  RequestOptions = maps:merge(RequestOptions0,
                              #{response_body =>
                                  {jsv, {ref, github, events}}}),
  case github_http:send_request(get, Target, RequestOptions) of
    {ok, {Status, Header, Events}} when Status >= 200, Status < 300 ->
      Response1 = #{events => Events},
      Response2 = set_response_poll_interval(Response1, Header),
      Response3 = set_response_etag(Response2, Header),
      {ok, Response3};
    {ok, {304, _, _}} ->
      {ok, not_modified};
    {ok, {Status, _, _}} ->
      {error, {request_error, Status, unknown}};
    {error, Reason} ->
      {error, Reason}
  end.

-spec set_response_poll_interval(event_response(), mhttp:header()) ->
        event_response().
set_response_poll_interval(Response, Header) ->
  case mhttp_header:find(Header, <<"X-Poll-Interval">>) of
    {ok, Value} ->
      try
        erlang:binary_to_integer(Value)
      of
        N when N >= 0 ->
          Response#{poll_interval => N};
        _ ->
          Response
      catch
        error:_ ->
          Response
      end;
    error ->
      Response
  end.

-spec set_response_etag(event_response(), mhttp:header()) -> event_response().
set_response_etag(Response, Header) ->
  case mhttp_header:find(Header, <<"ETag">>) of
    {ok, <<"W/", ETag/binary>>} ->
      Response#{etag => ETag};
    {ok, ETag} ->
      Response#{etag => ETag};
    error ->
      Response
  end.

-spec validate(map()) -> jsv:validation_result(event()).
validate(Event = #{type := Type, payload := Payload}) ->
  case payload_definition(Type) of
    {ok, Definition} ->
      Options = #{unknown_member_handling => keep,
                  null_member_handling => remove},
      case jsv:validate(Payload, Definition, Options) of
        {ok, Payload2} ->
          {ok, Event#{payload => Payload2}};
        {error, Errors} ->
          {error, {invalid_child, [<<"payload">>], Errors}}
      end;
    error ->
      {error, {invalid_value, Type, [<<"type">>],
               unknown_event_type,
               <<"unknown event type ", Type/binary>>}}
  end.

-spec generate(event()) ->
        {ok, term()} | {error, jsv:generation_error_reason()}.
generate(Event = #{type := Type, payload := Payload}) ->
  case payload_definition(Type) of
    {ok, Definition} ->
      case jsv:generate(Payload, Definition) of
        {ok, Value} ->
          {ok, Event#{payload => Value}};
        {error, Reason} ->
          {error, Reason}
      end;
    error ->
      {error, {invalid_event_type, Type}}
  end.

-spec payload_definition(Type :: binary()) -> {ok, jsv:definition()} | error.
payload_definition(<<"CommitCommentEvent">>) ->
  {ok, {ref, github, event_payload_commit_comment}};
payload_definition(<<"CreateEvent">>) ->
  {ok, {ref, github, event_payload_create}};
payload_definition(<<"DeleteEvent">>) ->
  {ok, {ref, github, event_payload_delete}};
payload_definition(<<"ForkEvent">>) ->
  {ok, {ref, github, event_payload_fork}};
payload_definition(<<"GollumEvent">>) ->
  {ok, {ref, github, event_payload_gollum}};
payload_definition(<<"IssueCommentEvent">>) ->
  {ok, {ref, github, event_payload_issue_comment}};
payload_definition(<<"IssuesEvent">>) ->
  {ok, {ref, github, event_payload_issues}};
payload_definition(<<"MemberEvent">>) ->
  {ok, {ref, github, event_payload_member}};
payload_definition(<<"PublicEvent">>) ->
  {ok, {ref, github, event_payload_public}};
payload_definition(<<"PullRequestEvent">>) ->
  {ok, {ref, github, event_payload_pull_request}};
payload_definition(<<"PullRequestReviewEvent">>) ->
  {ok, {ref, github, event_payload_pull_request_review}};
payload_definition(<<"PullRequestReviewCommentEvent">>) ->
  {ok, {ref, github, event_payload_pull_request_review_comment}};
payload_definition(<<"PushEvent">>) ->
  {ok, {ref, github, event_payload_push}};
payload_definition(<<"ReleaseEvent">>) ->
  {ok, {ref, github, event_payload_release}};
payload_definition(<<"SponsorshipEvent">>) ->
  {ok, {ref, github, event_payload_sponsorship}};
payload_definition(<<"WatchEvent">>) ->
  {ok, {ref, github, event_payload_watch}};
payload_definition(_) ->
  error.
