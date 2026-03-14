%% Copyright (C) 2026 Bozhidar Batsov
%% SPDX-License-Identifier: Apache-2.0

-module(sample).

-export([hello/1, factorial/1, process/2]).

-include("sample.hrl").

-define(MAX_SIZE, 100).
-define(DOUBLE(X), (X * 2)).

-type color() :: red | green | blue.
-type result(T) :: {ok, T} | {error, term()}.

-record(person, {
    name :: string(),
    age :: non_neg_integer()
}).

-spec hello(string()) -> ok.
hello(Name) ->
    io:format("Hello, ~s!~n", [Name]).

-spec factorial(non_neg_integer()) -> pos_integer().
factorial(0) -> 1;
factorial(N) when N > 0 ->
    N * factorial(N - 1).

process(Type, Data) ->
    case Type of
        parse ->
            parse_data(Data);
        validate ->
            validate_data(Data);
        _ ->
            {error, unknown_type}
    end.

parse_data(Data) ->
    try
        {ok, do_parse(Data)}
    catch
        error:Reason ->
            {error, Reason}
    end.

validate_data(Data) ->
    if
        is_list(Data) ->
            {ok, Data};
        is_binary(Data) ->
            {ok, binary_to_list(Data)};
        true ->
            {error, invalid}
    end.

do_parse(Data) ->
    [X || X <- Data, X > 0].

start_loop() ->
    receive
        {msg, Msg} ->
            io:format("Got: ~p~n", [Msg]),
            start_loop();
        stop ->
            ok
    after 5000 ->
        timeout
    end.

-ifdef(TEST).
run_tests() ->
    ok.
-endif.
