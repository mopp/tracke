-module(tracke).

-include("tracke.hrl").

%%====================================================================
%% API
%%====================================================================
-export([format/1,
         is_tracke/1]).

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
format(#tracke{reason = Reason0,
               histories = Histories0}) ->
    Histories = lists:foldl(fun(#history{module = Module,
                                         function = Function,
                                         args = Args,
                                         line = Line,
                                         aux = Aux},
                                Acc) ->
                                    F0 = io_lib:format("~p:~p/~B at L~B", [Module, Function, Args, Line]),
                                    F = case Aux of
                                            '_' ->
                                                F0;
                                            _ ->
                                                %% Append the aux if it is NOT default value (`_').
                                                io_lib:format("~s (Aux: ~p)", [F0, Aux])
                                        end,

                                    [list_to_binary(F) | Acc]
                            end,
                            [],
                            Histories0),
    Reason = list_to_binary(io_lib:format("~p", [Reason0])),
    join([<<"Reason: ">>, Reason, <<", History:">>, join(Histories, <<" -> ">>)], <<"">>).

-spec is_tracke(term()) -> boolean().
is_tracke(#tracke{}) ->
    true;
is_tracke(_) ->
    false.

%%====================================================================
%% Internal functions
%%====================================================================
-spec join([binary()], binary()) -> binary().
join([X | [Y | Rest]], Joint) ->
    join([<<X/binary, Joint/binary, Y/binary>> | Rest], Joint);
join([X], _) ->
    X.
