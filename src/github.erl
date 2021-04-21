-module(github).

-export([]).

-export_type([result/0, result/1, error_reason/0,
              authentication/0]).

-type result() :: ok | {error, error_reason()}.
-type result(Type) :: {ok, Type} | {error, error_reason()}.

-type error_reason() ::
        {http_error, term(), mhttp:request()} % TODO mhttp:error_reason()
      | {request_error, mhttp:status(), binary() | undefined}
      | {invalid_response_body, {json, json:error(), binary()}}
      | {invalid_response_body, {jsv, [jsv:value_error()], json:value()}}
      | {invalid_hypermedia_data, github_hypermedia:error_reason()}.

-type authentication() ::
        {personal, User :: binary(), Token :: binary()}.
