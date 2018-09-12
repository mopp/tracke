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
-spec format(tracke(term()) | term()) -> io_lib:chars().
format(Tracke) ->
    format(Tracke, [{indent, 4}]).

-spec format(tracke(term()) | term(), format_options()) -> io_lib:chars().
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
