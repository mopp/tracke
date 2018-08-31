-module(tracke).

-include("tracke.hrl").

-export([reason/1,
         format/1,
         format/2]).

-export_type([format_options/0]).

% Only for internal use.
-export([internal_chain/2]).

-type format_options() :: [{indent, non_neg_integer()}].

%%====================================================================
%% API functions
%%====================================================================
-spec reason(tracke() | term()) -> term().
reason(#tracke{reason = Reason}) ->
    Reason;
reason(X) ->
    X.

-spec format(tracke() | term()) -> io_lib:chars().
format(Tracke) ->
    format(Tracke, [{indent, 4}]).

-spec format(tracke() | term(), format_options()) -> io_lib:chars().
format(#tracke{reason = Reason,
               histories = Histories}, [{indent, N}]) ->
    Indent = lists:flatten(lists:duplicate(N, " ")),
    Hists0  = lists:foldl(fun(#history{module = Module,
                                       function = Function,
                                       args = Args,
                                       line = Line,
                                       aux = Aux}, Acc) ->
                                  [io_lib:format("~s~p:~p/~p at L~p~n"
                                                 "~s~sArgs: ~p~n"
                                                 "~s~sAux: ~p~n",
                                                 [Indent,
                                                  Module,
                                                  Function,
                                                  length(Args),
                                                  Line,
                                                  Indent,
                                                  Indent,
                                                  Args,
                                                  Indent,
                                                  Indent,
                                                  Aux]) | Acc]
                          end,
                          [],
                          Histories),
    io_lib:format("Reason: ~p~n"
                  "History:~n"
                  "~s",
                  [Reason,
                   lists:flatten(Hists0)]);
format(Reason, _) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Internal functions
%%====================================================================
-spec internal_chain(tracke() | term(), history()) -> tracke().
internal_chain(#tracke{histories = Histories} = Tracke, NewHistory) ->
    Tracke#tracke{histories = [NewHistory | Histories]};
internal_chain(Reason, NewHistory) ->
    #tracke{reason = Reason,
            histories = [NewHistory]}.
