-module(github_error).

-export_type([error/0, simple_validation_error/0, validation_error/0]).

-type error() ::
        #{message := binary(),
          documentation_url => binary(),
          errors => [simple_validation_error() | validation_error()]}.

-type simple_validation_error() ::
        binary().

-type validation_error() ::
        #{resource => binary(),
          field => binary(),
          message => binary(),
          code := binary(),
          index => integer(),
          value => binary() | integer() | [binary()] | null}.
