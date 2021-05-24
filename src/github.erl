-module(github).

-export_type([result/0, result/1, error_reason/0, request_error_reason/0,
              options/0, authentication/0]).

-type result() :: ok | {error, error_reason()}.
-type result(Type) :: {ok, Type} | {error, error_reason()}.

-type error_reason() ::
        {http_error, term(), mhttp:request()} % TODO mhttp:error_reason()
      | {request_error, mhttp:status(), request_error_reason()}
      | {invalid_response_body, {json, json:error(), binary()}}
      | {invalid_response_body, {jsv, [jsv:value_error()], json:value()}}
      | {invalid_hypermedia_data, github_hypermedia:error_reason()}
      | missing_hook_signature
      | {invalid_hook_signature_format, binary()}
      | invalid_hook_signature
      | missing_hook_event_type
      | {invalid_hook_event_type, binary()}
      | {unsupported_hook_event_type, github_hook_events:event_type()}
      | {invalid_hook_event, {json, json:error(), binary()}}
      | {invalid_hook_event, {jsv, [jsv:value_error()], json:value()}}.

-type request_error_reason() ::
        undefined
      | binary()
      | github_error:error().

-type options() ::
        #{mhttp_pool => mhttp:pool_id(),
          endpoint => binary() | uri:uri(),
          authentication => github:authentication(),
          user_agent => binary()}.

-type authentication() ::
        {personal, User :: binary(), Token :: binary()}
      | {oauth2, AccessToken :: binary()}.
