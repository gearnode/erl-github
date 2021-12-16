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

-module(github_jsv_datetime).

-behaviour(jsv_type).

-export([validate_type/2, canonicalize/3, generate/2]).

validate_type(Value, _) when is_binary(Value) ->
  try
    String = binary_to_list(Value),
    {ok, Value, calendar:rfc3339_to_system_time(String)}
  catch
    error:_ ->
      error
  end;
validate_type(Value, _) when is_integer(Value) ->
  {ok, Value, Value};
validate_type(_, _) ->
  error.

canonicalize(_, SystemTime, _) ->
  calendar:gregorian_seconds_to_datetime(SystemTime + 62167219200).

generate({{Y,Mon,D}, {H,Min,S}}, _) ->
  Data = io_lib:format(<<"~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0bZ">>,
                       [Y, Mon, D, H, Min, S]),
  {ok, iolist_to_binary(Data)};
generate(_, _) ->
  error.
