%% Copyright (c) 2021 Exograd SAS.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(github_http).

-export([get_resource/4, get_resources/4, create_resource/6,
         delete_resource/3,
         send_request/3, send_request/4,
         next_page_uri/1, link_uri/2]).

-export_type([options/0, request_body_spec/0, response_body_spec/0,
              response/0, response_body/0]).

-type options() ::
        #{if_none_match => binary(),
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

-type response_body() :: none | binary() | json:value() | term().

-spec get_resource(mhttp:method(), uri:uri(), github:options(),
                   jsv:definition()) ->
        github:result(term()).
get_resource(Method, URI, Options, JSVDefinition) ->
  HTTPOptions = #{response_body => {jsv, JSVDefinition}},
  case send_request(Method, URI, Options, HTTPOptions) of
    {ok, {Status, _Header, Value}} when Status >= 200, Status < 300 ->
      {ok, Value};
    {ok, {Status, _Header, _Value}} ->
      %% TODO error
      {error, {request_error, Status, undefined}};
    {error, Reason} ->
      {error, Reason}
  end.

-spec get_resources(mhttp:method(), uri:uri(), github:options(),
                    jsv:definition()) ->
        github:result([term()]).
get_resources(Method, URI, Options, JSVDefinition0) ->
  JSVDefinition = {array, #{element => JSVDefinition0}},
  HTTPOptions = #{response_body => {jsv, JSVDefinition}},
  get_resources(Method, URI, Options, HTTPOptions, []).

-spec get_resources(mhttp:method(), uri:uri(), github:options(), options(),
                    [term()]) ->
        github:result([term()]).
get_resources(Method, URI, Options, HTTPOptions, Acc) ->
  case send_request(Method, URI, Options, HTTPOptions) of
    {ok, {Status, Header, Values}} when Status >= 200, Status < 300 ->
      case Values of
        [] ->
          {ok, lists:flatten(lists:reverse(Acc))};
        _ ->
          case github_http:next_page_uri(Header) of
            {ok, NextURI} ->
              get_resources(Method, NextURI, Options, HTTPOptions,
                            [Values | Acc]);
            error ->
              {ok, lists:flatten(lists:reverse(Acc))};
            {error, Reason} ->
              {error, Reason}
          end
      end;
    {ok, {Status, _Header, _Value}} ->
      %% TODO error
      {error, {request_error, Status, undefined}};
    {error, Reason} ->
      {error, Reason}
  end.

-spec create_resource(mhttp:method(), uri:uri(), github:options(), term(),
                      jsv:definition(), jsv:definition()) ->
        github:result(term()).
create_resource(Method, URI, Options, RequestData, RequestJSVDefinition,
                ResponseJSVDefinition) ->
  HTTPOptions = #{request_body => {jsv, RequestData, RequestJSVDefinition},
                  response_body => {jsv, ResponseJSVDefinition}},
  case send_request(Method, URI, Options, HTTPOptions) of
    {ok, {Status, _Header, Value}} when Status >= 200, Status < 300 ->
      {ok, Value};
    {ok, {Status, _Header, _Value}} ->
      %% TODO error
      {error, {request_error, Status, undefined}};
    {error, Reason} ->
      {error, Reason}
  end.

-spec delete_resource(mhttp:method(), uri:uri(), github:options()) ->
        github:result().
delete_resource(Method, URI, Options) ->
  HTTPOptions = #{},
  case send_request(Method, URI, Options, HTTPOptions) of
    {ok, {Status, _Header, _Value}} when Status >= 200, Status < 300 ->
      ok;
    {ok, {Status, _Header, _Value}} ->
      %% TODO error
      {error, {request_error, Status, undefined}};
    {error, Reason} ->
      {error, Reason}
  end.

-spec send_request(mhttp:method(), uri:uri(), github:options()) ->
        github:result(response()).
send_request(Method, URI, Options) ->
  send_request(Method, URI, Options, #{}).

-spec send_request(mhttp:method(), uri:uri(), github:options(), options()) ->
        github:result(response()).
send_request(Method, URI, Options, HTTPOptions) ->
  Request0 = #{method => Method, target => URI},
  Request = finalize_request(Request0, Options, HTTPOptions),
  PoolId = maps:get(mhttp_pool, Options, default),
  case mhttp:send_request(Request, #{pool => PoolId}) of
    {ok, Response = #{status := Status}} when
        Status >= 200, Status < 300; Status =:= 304 ->
      Header = mhttp_response:header(Response),
      Body = mhttp_response:body(Response),
      Spec = maps:get(response_body, HTTPOptions, data),
      case decode_response_body(Body, Spec) of
        {ok, Term} ->
          {ok, {Status, Header, Term}};
        {error, Reason} ->
          {error, Reason}
      end;
    {ok, Response = #{status := Status}} ->
      {error, {request_error, Status, request_error_reason(Response)}};
    {error, Reason} ->
      {error, {http_error, Reason, Request}}
  end.

-spec request_error_reason(mhttp:response()) -> github:request_error_reason().
request_error_reason(Response) ->
  case mhttp_response:body(Response) of
    <<>> ->
      mhttp_response:reason(Response);
    Body ->
      case json:parse(Body) of
        {ok, Value} ->
          Definition = {ref, github, error},
          Options = #{type_map => github_jsv:type_map(),
                      unknown_member_handling => keep,
                      null_member_handling => remove},
          case jsv:validate(Value, Definition, Options) of
            {ok, Term} ->
              Term;
            {error, _} ->
              Body
          end;
        {error, _} ->
          Body
      end
  end.

-spec finalize_request(mhttp:request(), github:options(), options()) ->
        mhttp:request().
finalize_request(Request, Options, HTTPOptions) ->
  Funs = [fun set_request_target/3,
          fun set_request_auth/3,
          fun set_request_if_none_match/3,
          fun set_request_user_agent/3,
          fun set_request_header/3,
          fun set_request_body/3],
  lists:foldl(fun (Fun, Req) ->
                  Fun(Req, Options, HTTPOptions)
              end, Request, Funs).

-spec set_request_target(mhttp:request(), github:options(), options()) ->
        mhttp:request().
set_request_target(Request, Options, _) ->
  TargetRef = mhttp_request:target_uri(Request),
  TargetBase = endpoint_uri(Options),
  Target = uri:resolve_reference(TargetRef, TargetBase),
  Request#{target => Target}.

-spec set_request_auth(mhttp:request(), github:options(), options()) ->
        mhttp:request().
set_request_auth(Request, #{authentication := {personal, User, Token}}, _) ->
  Header = mhttp_request:header(Request),
  Header2 = mhttp_header:add_basic_authorization(Header, User, Token),
  Request#{header => Header2};
set_request_auth(Request, #{authentication := {oauth2, AccessToken}}, _) ->
  Header = mhttp_request:header(Request),
  Header2 = mhttp_header:add(Header, <<"Authorization">>,
                             <<"token ", AccessToken/binary>>),
  Request#{header => Header2};
set_request_auth(Request, _, _) ->
  Request.

-spec set_request_if_none_match(mhttp:request(), github:options(),
                                options()) ->
        mhttp:request().
set_request_if_none_match(Request, _, #{if_none_match := Value}) ->
  Header = mhttp_request:header(Request),
  Header2 = mhttp_header:add(Header, <<"If-None-Match">>, Value),
  Request#{header => Header2};
set_request_if_none_match(Request, _, _) ->
  Request.

-spec set_request_user_agent(mhttp:request(), github:options(), options()) ->
        mhttp:request().
set_request_user_agent(Request, Options, _) ->
  Value = maps:get(user_agent, Options, <<"erl-github">>),
  Header = mhttp_request:header(Request),
  Request#{header => mhttp_header:add(Header, <<"User-Agent">>, Value)}.

-spec set_request_header(mhttp:request(), github:options(), options()) ->
        mhttp:request().
set_request_header(Request, _, _) ->
  DefaultHeader = [{<<"Content-Type">>, <<"application/json">>},
                   {<<"Accept">>, <<"application/json">>}],
  Header = lists:foldl(fun ({Name, Value}, H) ->
                           mhttp_header:add_if_missing(H, Name, Value)
                       end, mhttp_request:header(Request), DefaultHeader),
  Request#{header => Header}.

-spec set_request_body(mhttp:request(), github:options(), options()) ->
        mhttp:request().
set_request_body(Request, _, #{request_body := Spec}) ->
  Request#{body => encode_request_body(Spec)};
set_request_body(Request, _, _) ->
  Request.

-spec endpoint_uri(github:options()) -> uri:uri().
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
decode_response_body(<<>>, _) ->
  {ok, none};
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
      Options = #{type_map => github_jsv:type_map(),
                  unknown_member_handling => keep,
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

-spec next_page_uri(mhttp:header()) ->
        github:result(uri:uri()) | error.
next_page_uri(Header) ->
  link_uri(Header, <<"next">>).

-spec link_uri(mhttp:header(), github_hypermedia:relation()) ->
        github:result(uri:uri()) | error.
link_uri(Header, Relation) ->
  case mhttp_header:find(Header, <<"link">>) of
    {ok, Value} ->
      case github_hypermedia:parse_links(Value) of
        {ok, Links} ->
          maps:find(Relation, Links);
        {error, Reason} ->
          {error, Reason}
      end;
    error ->
      error
  end.
