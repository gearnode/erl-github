-module(github_org_hooks).

-export([create_org_hook/3, delete_org_hook/3]).

-export_type([new_org_hook/0, new_org_hook_config/0,
              org_hook/0, org_hook_config/0]).

-type new_org_hook() ::
        #{name := binary(),
          events => [binary()],
          active => boolean(),
          config := org_hook_config()}.

-type new_org_hook_config() ::
        #{url := binary(),
          insecure_ssl => binary() | integer(),
          content_type => binary(),
          digest => binary(),
          secret => binary(),
          token => binary()}.

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

-spec create_org_hook(Org :: binary(), new_org_hook(), github:options()) ->
        github:result(org_hook()).
create_org_hook(Org, NewHook, Options) ->
  URI = #{path => uri_paths:join([<<"orgs">>, Org, <<"hooks">>])},
  github_http:create_resource(post, URI, Options, NewHook,
                              {ref, github, new_org_hook},
                              {ref, github, org_hook}).

-spec delete_org_hook(Org :: binary(), Id :: integer(), github:options()) ->
        github:result().
delete_org_hook(Org, Id, Options) ->
  URI = #{path => uri_paths:join([<<"orgs">>, Org, <<"hooks">>,
                                  integer_to_binary(Id)])},
  github_http:delete_resource(delete, URI, Options).
