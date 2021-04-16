-module(github).

-export([]).

-export_type([result/0, result/1, error_reason/0,
              authentication/0]).

-type result() :: ok | {error, error_reason()}.
-type result(Type) :: {ok, Type} | {error, error_reason()}.

-type error_reason() :: {request_error, term()}. % TODO mhttp:error_reason()

-type authentication() ::
        {personal, User :: binary(), Token :: binary()}.
