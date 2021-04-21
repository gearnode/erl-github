-module(github_hypermedia).

-export([parse_links/1]).

-export_type([links/0, relation/0, uri/0, error_reason/0]).

-type links() :: #{relation() := uri()}.
-type relation() :: binary().
-type uri() :: binary().

-type error_reason() ::
        {truncated_uri, binary()}
      | {truncated_name, binary()}
      | {invalid_relation, binary()}.

-spec parse_links(binary()) -> github:result(links()).
parse_links(Data) ->
  try
    {ok, parse_links(Data, #{})}
  catch
    throw:{error, Reason} ->
      {error, {invalid_hypermedia_data, Reason}}
  end.

-spec parse_links(binary(), links()) -> links().
parse_links(<<>>, Acc) ->
  Acc;
parse_links(<<C, Data/binary>>, Acc) when C =:= $\s; C =:= $\t; C =:= $, ->
  parse_links(Data, Acc);
parse_links(<<$<, Data/binary>>, Acc) ->
  case binary:split(Data, <<">">>) of
    [URI, Rest] ->
      {Relation, Rest2} = parse_relation(Rest),
      parse_links(Rest2, Acc#{Relation => URI});
    _ ->
      throw({error, {truncated_uri, Data}})
  end.

-spec parse_relation(binary()) -> {relation(), Rest :: binary()}.
parse_relation(<<C, Data/binary>>) when C =:= $\s; C =:= $\t; C =:= $; ->
  parse_relation(Data);
parse_relation(<<"rel=\"", Data/binary>>) when byte_size(Data) > 0 ->
  case binary:split(Data, <<"\"">>) of
    [Relation, Rest] ->
      {Relation, Rest};
    _ ->
      throw({error, {truncated_name, Data}})
  end;
parse_relation(Data) ->
  throw({error, {invalid_relation, Data}}).
