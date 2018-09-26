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
                                    Formated = io_lib:format("~s~p:~p/~p at L~p~n"
                                                             "~s~sAux: ~p~n",
                                                             [Indent,
                                                              Module,
                                                              Function,
                                                              Args,
                                                              Line,
                                                              Indent,
                                                              Indent,
                                                              Aux]),
                                    [list_to_binary(Formated) | Acc]
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
