-module(github_http).

-export([send_request/2, send_request/3]).

-export_type([options/0, request_body_spec/0, response_body_spec/0,
              response/0, response_body/0]).

-type options() ::
        #{mhttp_pool => mhttp:pool_id(),
          endpoint => binary() | uri:uri(),
          authentication => github:authentication(),
          user_agent => binary(),
          if_none_match => binary(),
          request_body => request_body_spec(),
          response_body => response_body_spec()}.

-type request_body_spec() ::
        {data, iodata()}
      | {json, json:value()}
      | {jsv, term(), jsv:definition()}.

-type response_body_spec() ::
        data
      | json
      | {jsv, jsv:definition()}.

-type response() :: {mhttp:status(), mhttp:header(), response_body()}.

-type response_body() :: binary() | json:value() | term().

-spec send_request(mhttp:method(), uri:uri() | uri:path()) ->
        github:result(response()).
send_request(Method, TargetOrPath) ->
  send_request(Method, TargetOrPath, #{}).

-spec send_request(mhttp:method(), uri:uri() | uri:path(), options()) ->
        github:result(response()).
send_request(Method, TargetOrPath, Options) ->
  Target = case TargetOrPath of
             Path when is_binary(Path) ->
               #{path => Path};
             URI when is_map(URI) ->
               URI
           end,
  Request0 = #{method => Method, target => Target},
  Request = finalize_request(Request0, Options),
  PoolId = maps:get(mhttp_pool, Options, default),
  case mhttp:send_request(Request, #{pool => PoolId}) of
    {ok, Response} ->
      Status = mhttp_response:status(Response),
      Header = mhttp_response:header(Response),
      Body = mhttp_response:body(Response),
      Spec = maps:get(response_body, Options, data),
      case decode_response_body(Body, Spec) of
        {ok, Term} ->
          {ok, {Status, Header, Term}};
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, {request_error, Reason}}
  end.

-spec finalize_request(mhttp:request(), options()) -> mhttp:request().
finalize_request(Request, Options) ->
  Funs = [fun set_request_target/2,
          fun set_request_auth/2,
          fun set_request_if_none_match/2,
          fun set_request_user_agent/2,
          fun set_request_header/2,
          fun set_request_body/2],
  lists:foldl(fun (Fun, Req) ->
                  Fun(Req, Options)
              end, Request, Funs).

-spec set_request_target(mhttp:request(), options()) -> mhttp:request().
set_request_target(Request, Options) ->
  TargetRef = mhttp_request:target_uri(Request),
  TargetBase = endpoint_uri(Options),
  Target = uri:resolve_reference(TargetRef, TargetBase),
  Request#{target => Target}.

-spec set_request_auth(mhttp:request(), options()) -> mhttp:request().
set_request_auth(Request, #{authentication := {personal, User, Token}}) ->
  Header = mhttp_request:header(Request),
  Header2 = mhttp_header:add_basic_authorization(Header, User, Token),
  Request#{header => Header2};
set_request_auth(Request, _Options) ->
  Request.

-spec set_request_if_none_match(mhttp:request(), options()) ->
        mhttp:request().
set_request_if_none_match(Request, #{if_none_match := Value}) ->
  Header = mhttp_request:header(Request),
  Header2 = mhttp_header:add(Header, <<"If-None-Match">>, Value),
  Request#{header => Header2};
set_request_if_none_match(Request, _) ->
  Request.

-spec set_request_user_agent(mhttp:request(), options()) -> mhttp:request().
set_request_user_agent(Request, Options) ->
  Value = maps:get(user_agent, Options, <<"erl-github">>),
  Header = mhttp_request:header(Request),
  Request#{header => mhttp_header:add(Header, <<"User-Agent">>, Value)}.

-spec set_request_header(mhttp:request(), options()) -> mhttp:request().
set_request_header(Request, _Options) ->
  DefaultHeader = [{<<"Content-Type">>, <<"application/json">>},
                   {<<"Accept">>, <<"application/json">>}],
  Header = lists:foldl(fun ({Name, Value}, H) ->
                           mhttp_header:add_if_missing(H, Name, Value)
                       end, mhttp_request:header(Request), DefaultHeader),
  Request#{header => Header}.

-spec set_request_body(mhttp:request(), options()) -> mhttp:request().
set_request_body(Request, #{request_body := Spec}) ->
  Request#{body => encode_request_body(Spec)};
set_request_body(Request, _Options) ->
  Request.

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

-spec encode_request_body(request_body_spec()) -> iodata().
encode_request_body(undefined) ->
  <<>>;
encode_request_body({data, Data}) ->
  Data;
encode_request_body({json, Value}) ->
  json:serialize(Value);
encode_request_body({jsv, Term, Definition}) ->
  case jsv:generate(Term, Definition) of
    {ok, Value} ->
      encode_request_body({json, Value});
    {error, Reason} ->
      error({invalid_request_body, Reason, Term})
  end.

-spec decode_response_body(binary(), response_body_spec()) ->
        github:result(response_body()).
decode_response_body(Body, data) ->
  {ok, Body};
decode_response_body(Body, json) ->
  case json:parse(Body) of
    {ok, Value} ->
      {ok, Value};
    {error, Error} ->
      {error, {invalid_response_body, {json, Error, Body}}}
  end;
decode_response_body(Body, {jsv, Definition}) ->
  case decode_response_body(Body, json) of
    {ok, Value} ->
      Options = #{unknown_member_handling => remove,
                  null_member_handling => remove,
                  format_value_errors => true},
      case jsv:validate(Value, Definition, Options) of
        {ok, Term} ->
          {ok, Term};
        {error, Errors} ->
          {error, {invalid_response_body, {jsv, Errors, Value}}}
      end;
    {error, Reason} ->
      {error, Reason}
  end.
