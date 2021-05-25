-module(github_events).

-export([list_public_events/2, list_organization_events/4,
         list_repository_events/4,
         validate/1, generate/1]).

-export_type([actor/0, event/0, event/1, repo/0,
              event_response/0, event_options/0]).

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
        #{first_page => pos_integer(),
          per_page => pos_integer(),
          pages => all | pos_integer(),
          http_options => github_http:options()}.

-spec list_public_events(event_options(), github:options()) ->
        github:result(event_response() | not_modified).
list_public_events(EventOptions, Options) ->
  fetch_events(<<"/events">>, EventOptions, Options).

-spec list_organization_events(Username :: binary(), Name :: binary(),
                               event_options(), github:options()) ->
        github:result(event_response() | not_modified).
list_organization_events(Username, Name, EventOptions, Options) ->
  UsernamePart = uri:encode_path(Username),
  NamePart = uri:encode_path(Name),
  Path =
    iolist_to_binary(["/users/", UsernamePart, "/events/orgs/", NamePart]),
  fetch_events(Path, EventOptions, Options).

-spec list_repository_events(Owner :: binary(), Name :: binary(),
                             event_options(), github:options()) ->
        github:result(event_response() | not_modified).
list_repository_events(Owner, Name, EventOptions, Options) ->
  OwnerPart = uri:encode_path(Owner),
  NamePart = uri:encode_path(Name),
  Path = iolist_to_binary(["/repos/", OwnerPart, $/, NamePart, "/events"]),
  fetch_events(Path, EventOptions, Options).

-spec fetch_events(uri:path(), event_options(), github:options()) ->
        github:result(event_response() | not_modified).
fetch_events(Path, EventOptions, Options) ->
  URI = event_uri(Path, EventOptions),
  HTTPOptions0 = maps:get(http_options, EventOptions, #{}),
  HTTPOptions = maps:merge(HTTPOptions0,
                           #{response_body => {jsv, {ref, github, events}}}),
  Response = #{events => []},
  fetch_events(URI, EventOptions, HTTPOptions, Options, Response).

-spec fetch_events(uri:uri(), event_options(), github_http:options(),
                   github:options(), event_response()) ->
        github:result(event_response() | not_modified).
fetch_events(URI, EventOptions, HTTPOptions, Options,
             Response = #{events := Events}) ->
  case github_http:send_request(get, URI, Options, HTTPOptions) of
    {ok, {304, _, _}} ->
      {ok, not_modified};
    {ok, {_, Header, ResponseEvents}} ->
      %% The handling of the first response is different since we need to
      %% collect data from the header then modify HTTP options for
      %% subsequence HTTP options.
      {Response2, HTTPOptions2} =
        case Events of
          [] ->
            {lists:foldl(fun (F, Res) ->
                             F(Res, Header)
                         end, Response#{events => ResponseEvents},
                         [fun set_response_poll_interval/2,
                          fun set_response_etag/2]),
             HTTPOptions};
          _ ->
            {Response#{events => Events ++ ResponseEvents},
             maps:remove(if_none_match, HTTPOptions)}
        end,
      case github_http:next_page_uri(Header) of
        {ok, NextURI} ->
          case maps:get(pages, EventOptions, 1) of
            all ->
              fetch_events(NextURI, EventOptions, HTTPOptions2, Options,
                           Response2);
            N when is_integer(N), N > 1 ->
              EventOptions2 = EventOptions#{pages => N-1},
              fetch_events(NextURI, EventOptions2, HTTPOptions2, Options,
                           Response2);
            1 ->
              {ok, Response2}
          end;
        error ->
          {ok, Response2};
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec event_uri(uri:path(), event_options()) -> uri:uri().
event_uri(Path, Options) ->
  FirstPage = maps:get(first_page, Options, 1),
  PerPage = maps:get(per_page, Options, 10),
  Query = [{<<"page">>, integer_to_binary(FirstPage)},
           {<<"per_page">>, integer_to_binary(PerPage)}],
  #{path => Path, query => Query}.

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
      Options = #{type_map => github_jsv:type_map(),
                  unknown_member_handling => keep,
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
