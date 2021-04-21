-module(github_repositories).

-export([get_repository/2]).

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

-spec get_repository(Owner :: binary(), Name :: binary()) ->
        github:result(repository()).
get_repository(Owner, Name) ->
  OwnerPart = uri:encode_path(Owner),
  NamePart = uri:encode_path(Name),
  Path = iolist_to_binary(["/repos/", OwnerPart, $/, NamePart]),
  URI = #{path => Path},
  github_http:get_resource(get, URI, {ref, github, repository}).
