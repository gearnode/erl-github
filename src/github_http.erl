-module(github_http).

-export([send_request/1, send_request/2]).

-export_type([options/0]).

-type options() ::
        #{mhttp_pool => mhttp:pool_id(),
          endpoint => binary() | uri:uri(),
          authentication => github:authentication(),
          user_agent => binary()}.

-spec send_request(mhttp:request()) -> github:result(mhttp:response()).
send_request(Request) ->
  send_request(Request, #{}).

-spec send_request(mhttp:request(), options()) ->
        github:result(mhttp:response()).
send_request(Request0, Options) ->
  PoolId = maps:get(mhttp_pool, Options, default),
  Request = finalize_request(Request0, Options),
  case mhttp:send_request(Request, #{pool => PoolId}) of
    {ok, Response} ->
      {ok, Response};
    {error, Reason} ->
      {error, {request_error, Reason}}
  end.

-spec finalize_request(mhttp:request(), options()) -> mhttp:request().
finalize_request(Request, Options) ->
  Funs = [fun finalize_request_target/2,
          fun finalize_request_auth/2,
          fun finalize_request_user_agent/2,
          fun finalize_request_header/2],
  lists:foldl(fun (Fun, Req) ->
                  Fun(Req, Options)
              end, Request, Funs).

-spec finalize_request_target(mhttp:request(), options()) -> mhttp:request().
finalize_request_target(Request, Options) ->
  TargetRef = mhttp_request:target_uri(Request),
  TargetBase = endpoint_uri(Options),
  Target = uri:resolve_reference(TargetRef, TargetBase),
  Request#{target => Target}.

-spec finalize_request_auth(mhttp:request(), options()) -> mhttp:request().
finalize_request_auth(Request, #{authentication := {personal, User, Token}}) ->
  Credentials = <<User/binary, $:, Token/binary>>,
  Value = iolist_to_binary([<<"Basic ">>, base64:encode(Credentials)]),
  Header = mhttp_request:header(Request),
  Request#{header => mhttp_header:add(Header, <<"Authorization">>, Value)};
finalize_request_auth(Request, _Options) ->
  Request.

-spec finalize_request_user_agent(mhttp:request(), options()) ->
        mhttp:request().
finalize_request_user_agent(Request, Options) ->
  Value = maps:get(user_agent, Options, <<"erl-github">>),
  Header = mhttp_request:header(Request),
  Request#{header => mhttp_header:add(Header, <<"User-Agent">>, Value)}.

-spec finalize_request_header(mhttp:request(), options()) -> mhttp:request().
finalize_request_header(Request, _Options) ->
  DefaultHeader = [{<<"Content-Type">>, <<"application/json">>},
                   {<<"Accept">>, <<"application/json">>}],
  Header = lists:foldl(fun ({Name, Value}, H) ->
                           mhttp_header:add_if_missing(H, Name, Value)
                       end, mhttp_request:header(Request), DefaultHeader),
  Request#{header => Header}.

-spec endpoint_uri(options()) -> uri:uri().
endpoint_uri(#{endpoint := Endpoint}) when is_binary(Endpoint) ->
  case uri:parse(Endpoint) of
    {ok, URI} ->
      URI;
    {error, Reason} ->
      error({invalid_endpoint_uri, Reason, Endpoint})
  end;
endpoint_uri(#{endpoint := Endpoint}) ->
  Endpoint;
endpoint_uri(_) ->
  #{scheme => <<"https">>, host => <<"api.github.com">>}.
