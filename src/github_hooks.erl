-module(github_hooks).

-export([create_hook/3]).

-export_type([new_hook/0, new_hook_config/0,
              hook/0, hook_config/0, hook_response/0]).

-type new_hook() ::
        #{type := binary(),
          active := boolean(),
          events := [binary()],
          config := new_hook_config()}.

-type new_hook_config() ::
        #{url => binary(),
          insecure_ssl => binary() | integer(),
          content_type => binary(),
          digest => binary(),
          secret => binary(),
          token => binary()}.

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

-spec create_hook(Owner :: binary(), Name :: binary(), new_hook()) ->
        github:result().
create_hook(Owner, Name, NewHook) ->
  URI = #{path => uri_paths:join([<<"repos">>, Owner, Name, <<"hooks">>])},
  github_http:create_resource(post, URI, NewHook, {ref, github, new_hook},
                              {ref, github, hook}).
