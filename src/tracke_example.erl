-module(tracke_example).

-include("tracke.hrl").

-compile([{parse_transform, tracke_pt}]).

-export([normal_usage/1,
         merge_error_types/1,
         deep_chain/0]).

-type tracke(Reason) :: tracke:tracke(Reason).

%% @doc Show you normal usage of tracke.
%% You can see the results on shell using the following codes:
%%   tracke_example:normal_usage(0).
%%   tracke_example:normal_usage(-100).
%%   tracke_example:normal_usage(127).
%%   tracke_example:normal_usage(15).
-spec normal_usage(integer()) -> ok.
normal_usage(X) ->
    case func1(X) of
        {ok, V} ->
            io:format("Ok: ~p~n", [V]);
        {error, Reason} ->
            io:format("~s~n", [tracke:format(Reason)]),
            case tracke:reason(Reason) of
                zero ->
                    io:format("Your input is zero.~n", []);
                negative ->
                    io:format("Your input is negative.~n", []);
                boring_number ->
                    io:format("Your input is boring.~n", [])
            end
    end,
    ok.

-spec func1(integer()) -> {ok, binary()} | {error, tracke(zero | negative | boring_number)}.
func1(0) ->
    {error, tracke:new(zero)};
func1(X) when X < 0 ->
    {error, tracke:new(negative)};
func1(X) ->
    case fizzbuzz(X) of
        {ok, _} = Ok ->
            Ok;
        {error, Reason} ->
            {error, tracke:chain(Reason)}
    end.

-spec fizzbuzz(integer()) -> {ok, binary()} | {error, tracke(boring_number)}.
fizzbuzz(X) ->
    case {X rem 5 =:= 0, X rem 3 =:= 0} of
        {true, true} ->
            {ok, <<"fizzbuzz">>};
        {true, false} ->
            {ok, <<"fizz">>};
        {false, true} ->
            {ok, <<"buzz">>};
        {false, false} ->
            {error, tracke:new(boring_number)}
    end.

%% @doc Create new type from different `tracke/1' types.
%% You can see the results on shell using the following codes:
%%   tracke_example:merge_error_types(foo).
%%   tracke_example:merge_error_types(bar).
-spec merge_error_types(foo | bar) -> ok.
merge_error_types(Atom) ->
    case error_func1(Atom) of
        {error, Reason} ->
            case tracke:reason(Reason) of
                % not_exist ->
                    % Dialyzer causes an error here.
                    % io:format("~s~n", [tracke:format(Reason)]);
                aaa ->
                    io:format("~s~n", [tracke:format(Reason)]);
                bbb ->
                    io:format("~s~n", [tracke:format(Reason)])
            end
    end,
    ok.

-spec error_func1(foo | bar) -> {error, tracke(aaa | bbb)}.
error_func1(X) ->
    case X of
        foo ->
            error_func2();
        bar ->
            error_func3()
    end.

-spec error_func2() -> {error, tracke(aaa)}.
error_func2() ->
    {error, tracke:new(aaa)}.

-spec error_func3() -> {error, tracke(bbb)}.
error_func3() ->
    {error, tracke:new(bbb)}.

%% @doc Create deep chained `tracke()'.
%% You can see the results on shell using the following codes:
%%   tracke_example:deep_chain().
-spec deep_chain() -> ok.
deep_chain() ->
    case deep_chain3(x, y, z) of
        {error, Reason} ->
            io:format("~s", [tracke:format(Reason)])
    end,
    ok.

-spec deep_chain1(x) -> {error, tracke(bottom_reason)}.
deep_chain1(X) ->
    {error, tracke:new(bottom_reason)}.

-spec deep_chain2(x, y) -> {error, tracke(bottom_reason)}.
deep_chain2(x, y) ->
    case deep_chain1(x) of
        {error, Reason} ->
            {error, tracke:chain(Reason, "hi")}
    end.

-spec deep_chain3(x, y, z) -> {error, tracke(bottom_reason)}.
deep_chain3(x, y, z) ->
    case deep_chain2(x, y) of
        {error, Reason} ->
            {error, tracke:chain(Reason)}
    end.
