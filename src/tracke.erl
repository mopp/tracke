-module(tracke).

-include("tracke.hrl").

%%====================================================================
%% API
%%====================================================================
-export([format/1,
         format/2]).

-export_type([tracke/1,
              format_options/0]).

%%====================================================================
%% Types
%%====================================================================
-type tracke(Reason) :: #tracke{reason :: Reason}.

-type format_options() :: [{indent, non_neg_integer()}].

%%====================================================================
%% API functions
%%====================================================================
-spec format(tracke(term()) | term()) -> binary().
format(Tracke) ->
    format(Tracke, [{indent, 4}]).

-spec format(tracke(term()) | term(), format_options()) -> binary().
format(#tracke{reason = Reason0,
               histories = Histories0}, [{indent, N}]) ->
    Indent = binary:copy(<<" ">>, N),
    Histories = lists:foldl(fun(#history{module = Module,
                                         function = Function,
                                         args = Args,
                                         line = Line,
                                         aux = Aux}, Acc) ->

                                    F0 = io_lib:format("~s~p:~p/~B at L~B~n", [Indent, Module, Function, Args, Line]),
                                    F = case Aux of
                                            '_' ->
                                                F0;
                                            _ ->
                                                %% Append the aux if it is NOT default value (`_').
                                                io_lib:format("~s~s~sAux: ~p~n", [F0, Indent, Indent, Aux])
                                        end,

                                    [list_to_binary(F) | Acc]
                            end,
                            [],
                            Histories0),
    Reason = list_to_binary(io_lib:format("~p", [Reason0])),
    join([<<"Reason: ">>, Reason, <<"\nHistory:\n">> | Histories]);
format(Reason, _) ->
    list_to_binary(io_lib:format("~p", [Reason])).

%%====================================================================
%% Internal functions
%%====================================================================
-spec join([binary()]) -> binary().
join([X | [Y | Rest]]) ->
    join([<<X/binary, Y/binary>> | Rest]);
join([X]) ->
    X.
