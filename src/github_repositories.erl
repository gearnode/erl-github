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

-module(github_repositories).

-export([get_repository/3, list_org_repositories/2]).

-export_type([repository/0]).

-type repository() ::
        #{id := integer(),
          name := binary(),
          full_name := binary(),
          description => binary(),
          url := binary(),
          created_at => calendar:datetime(),
          updated_at => calendar:datetime(),
          private := boolean(),
          fork := boolean(),
          is_template => boolean(),
          archived := boolean(),
          disabled := boolean(),
          default_branch := binary(),
          open_issues_count := integer(),
          subscribers_count => integer()}.

-spec get_repository(Owner :: binary(), Name :: binary(), github:options()) ->
        github:result(repository()).
get_repository(Owner, Name, Options) ->
  URI = #{path => uri_paths:join([<<"repos">>, Owner, Name])},
  github_http:get_resource(get, URI, Options, {ref, github, repository}).

-spec list_org_repositories(Org :: binary(), github:options()) ->
        github:result([repository()]).
list_org_repositories(Org, Options) ->
  URI = #{path => uri_paths:join([<<"orgs">>, Org, <<"repos">>])},
  github_http:get_resources(get, URI, Options, {ref, github, repository}).
