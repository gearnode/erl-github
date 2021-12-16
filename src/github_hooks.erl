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

-module(github_hooks).

-export([create_hook/4, delete_hook/4]).

-export_type([new_hook/0, new_hook_config/0,
              hook/0, hook_config/0, hook_response/0]).

-type new_hook() ::
        #{name => binary(),
          active => boolean(),
          events => [binary()],
          config => new_hook_config()}.

-type new_hook_config() ::
        #{url := binary(),
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

-spec create_hook(Owner :: binary(), Name :: binary(), new_hook(),
                  github:options()) ->
        github:result(hook()).
create_hook(Owner, Name, NewHook, Options) ->
  URI = #{path => uri_paths:join([<<"repos">>, Owner, Name, <<"hooks">>])},
  github_http:create_resource(post, URI, Options, NewHook,
                              {ref, github, new_hook}, {ref, github, hook}).

-spec delete_hook(Owner :: binary(), Name :: binary(), Id :: integer(),
                  github:options()) ->
        github:result().
delete_hook(Owner, Name, Id, Options) ->
  URI = #{path => uri_paths:join([<<"repos">>, Owner, Name, <<"hooks">>,
                                  integer_to_binary(Id)])},
  github_http:delete_resource(delete, URI, Options).
