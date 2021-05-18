-module(github_org_hooks).

-export([]).

-export_type([org_hook/0, org_hook_config/0]).

-type org_hook() ::
        #{id := integer(),
          url := binary(),
          ping_url := binary(),
          name := binary(),
          events := [binary()],
          active := boolean(),
          config := org_hook_config(),
          updated_at := calendar:datetime(),
          created_at := calendar:datetime(),
          type := binary()}.

-type org_hook_config() ::
        #{url => binary(),
          insecure_ssl => binary(),
          content_type => binary(),
          secret => binary()}.
