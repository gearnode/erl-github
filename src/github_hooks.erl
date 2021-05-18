-module(github_hooks).

-export([]).

-export_type([hook/0, hook_config/0, hook_response/0]).

-type hook() ::
        #{type := binary(),
          id := integer(),
          name := binary(),
          active := boolean(),
          events := [binary()],
          config := hook_config(),
          updated_at := calendar:datetime(),
          created_at := calendar:datetime(),
          url := binary(),
          test_url := binary(),
          ping_url := binary(),
          last_response := hook_response()}.

-type hook_config() ::
        #{email => binary(),
          password => binary(),
          room => binary(),
          subdomain => binary(),
          url => binary(),
          insecure_ssl => binary() | integer(),
          content_type => binary(),
          digest => binary(),
          secret => binary(),
          token => binary()}.

-type hook_response() ::
        #{code => integer(),
          status => binary(),
          message => binary()}.
