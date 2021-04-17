-module(github).

-export([]).

-export_type([result/0, result/1, error_reason/0,
              authentication/0]).

-type result() :: ok | {error, error_reason()}.
-type result(Type) :: {ok, Type} | {error, error_reason()}.

-type error_reason() ::
        {request_error, term()} % TODO mhttp:error_reason()
      | {invalid_response_body, {json, json:error(), binary()}}
      | {invalid_response_body, {jsv, [jsv:value_error()], json:value()}}.

-type authentication() ::
        {personal, User :: binary(), Token :: binary()}.
