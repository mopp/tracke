-module(tracke_tests).

-compile([{parse_transform, tracke_pt}]).

-include_lib("eunit/include/eunit.hrl").
-include("tracke.hrl").

-type tracke(Reason) :: tracke:tracke(Reason).

-spec underscode_func(Reason, term()) -> tracke(Reason).
underscode_func(Reason, _) ->
    tracke:new(Reason).

api_test_() ->
    {inparallel,
     [{"tracke:new/1 succeeds",
       ?_assertEqual(tracke_new(for_test, {?MODULE, api_test_, [], ?LINE + 1, undefined}),
                     tracke:new(for_test))},
      {"tracke:new/2 succeeds, aux is binary",
       ?_assertEqual(tracke_new(for_test, {?MODULE, api_test_, [], ?LINE + 1, <<"Put here what the caller has to do">>}),
                     tracke:new(for_test, <<"Put here what the caller has to do">>))},
      {"tracke:new/2 succeeds, aux is anonymous function",
       ?_assertEqual(tracke_new(for_test, {?MODULE, api_test_, [], ?LINE + 1, hint}),
                     (fun(X) -> tracke:new(for_test, X) end)(hint))},
      {"tracke:chain/1 succeeds",
       fun() ->
               Tracke = tracke:new(for_test),
               ?assertEqual(tracke_chain(Tracke, {?MODULE, api_test_, [], ?LINE + 1, undefined}),
                            tracke:chain(Tracke))
       end},
      {"tracke:chain/2 succeeds, aux is atom",
       fun() ->
               Tracke = tracke:new(for_test),
               ?assertEqual(tracke_chain(Tracke, {?MODULE, api_test_, [], ?LINE + 1, hint}),
                            tracke:chain(Tracke, hint))
       end},
      {"tracke:reason/1 succeeds",
       fun() ->
               Tracke = tracke:new(for_test),
               ?_assertEqual(for_test,
                             tracke:reason(Tracke))
       end},
      {"`_' variable binding is converted to atom '_`",
       ?_assertEqual(tracke_new(for_test, {?MODULE, underscode_func, [for_test, '_'], 12, undefined}),
                     underscode_func(for_test, dummy))}]}.

-type history_components() :: {?MODULE, atom(), [term()], non_neg_integer(), term()}.

-spec tracke_new(Reason, history_components()) -> tracke(Reason).
tracke_new(Reason, HistoryComponents) ->
    #tracke{reason = Reason,
            histories = [make_history(HistoryComponents)]}.

-spec tracke_chain(tracke(Reason), history_components()) -> tracke(Reason).
tracke_chain(Tracke, HistoryComponents) ->
    Tracke#tracke{histories = [make_history(HistoryComponents) | Tracke#tracke.histories]}.

-spec make_history(history_components()) -> #history{}.
make_history({Module, Function, Args, Line, Aux}) ->
    #history{module = Module,
             function = Function,
             args = Args,
             line = Line,
             aux = Aux}.
