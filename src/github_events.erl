-module(github_events).

-export([list_public_events/1]).

-export_type([event/1, actor/0, repo/0]).

-type event(Payload) ::
        #{id := binary(),
          type => binary(),
          actor := actor(),
          repo := repo(),
          org => actor(),
          payload := Payload,
          public := boolean(),
          created_at => calendar:datetime()}.

-type actor() ::
        #{id := integer(),
          login := binary(),
          display_login => binary(),
          gravatar_id => binary(),
          url := binary(),
          avatar_url := binary()}.

-type repo() ::
        #{id := integer(),
          name := string(),
          url := string()}.

-type event_options() ::
        #{per_page => non_neg_integer(),
          page => pos_integer()}.

-type event_response() ::
        #{events := [event(map())],
          poll_interval := non_neg_integer(), % seconds
          etag := binary()}.

-spec list_public_events(event_options()) -> github:result(event_response()).
list_public_events(EventOptions) ->
  RequestOptions = #{response_body => {jsv, {ref, github, events}}},
  Target = #{path => <<"/events">>,
             query => event_query(EventOptions)},
  github_http:send_request(get, Target, RequestOptions).

-spec event_query(event_options()) -> uri:query().
event_query(EventOptions) ->
  maps:fold(fun
              (per_page, N, Acc) ->
                [{<<"per_page">>, integer_to_binary(N)} | Acc];
              (page, N, Acc) ->
                [{<<"page">>, integer_to_binary(N)} | Acc]
            end, [], EventOptions).
