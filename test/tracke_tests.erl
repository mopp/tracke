-module(tracke_tests).

-compile([{parse_transform, tracke_pt}]).

-include_lib("eunit/include/eunit.hrl").
-include("tracke.hrl").

api_test_() ->
    {inparallel,
     [{"error/1 succeeds",
       ?_assertMatch(#tracke{reason = for_test,
                             histories = [#history{module = ?MODULE,
                                                   function = api_test_,
                                                   args = [],
                                                   aux = undefined}]},
                     tracke:new(for_test))},
      {"error/2 succeeds",
       ?_assertMatch(#tracke{reason = for_test,
                             histories = [#history{module = ?MODULE,
                                                   function = api_test_,
                                                   args = [],
                                                   aux = <<"write the reason why error caused here">>}]},
                     tracke:new(for_test, <<"write the reason why error caused here">>))},
      {"reacon/1 succeeds",
       fun() ->
               Tracke = tracke:new(for_test),
               ?assertEqual(for_test, tracke:reason(Tracke))
       end},
      {"chain/1 succeeds",
       fun() ->
               Tracke = tracke:new(for_test),
               ?assertMatch(#tracke{reason = for_test,
                                    histories = [#history{module = ?MODULE,
                                                          function = api_test_,
                                                          args = [],
                                                          aux = undefined},
                                                 #history{module = ?MODULE,
                                                          function = api_test_,
                                                          args = [],
                                                          aux = undefined}]},
                            tracke:chain(Tracke))
       end},
      {"chain/2 succeeds",
       fun() ->
               Tracke = tracke:new(for_test),
               ?assertMatch(#tracke{reason = for_test,
                                    histories = [#history{module = ?MODULE,
                                                          function = api_test_,
                                                          args = [],
                                                          aux = <<"sample message">>},
                                                 #history{module = ?MODULE,
                                                          function = api_test_,
                                                          args = [],
                                                          aux = undefined}]},
                            tracke:chain(Tracke, <<"sample message">>))
       end}
     ]}.

usage_test_() ->
    {inparallel,
     [{"Sample scenario 1",
       fun() ->
               case func2({sample_error_reason, <<"sample message">>}) of
                   ok ->
                       ?assert(false);
                   {error, Reason} ->
                       case tracke:reason(Reason) of
                           sample_error_reason ->
                               _ = io:format("~s", [tracke:format(Reason)]),
                               ?assert(true);
                           another_reason ->
                               ?assert(false)
                       end
               end
       end},
      {"Sample scenario 2",
       fun() ->
               case calc1(999, 1) of
                   {ok, _} ->
                       ?assert(false);
                   {error, Reason} ->
                       case tracke:reason(Reason) of
                           too_large ->
                               _ = io:format("~s", [tracke:format(Reason)]);
                           _ ->
                               ?assert(false)
                       end
               end
       end},
      {"`_' binding convert atom",
       fun() ->
               {error, #tracke{histories = [#history{args = [for_test, '_']}]}} = underscode_func(for_test, aaa)
       end}]}.

func1({Reason, Msg}) ->
    {error, tracke:new(Reason, <<<<"func1: ">>/binary, Msg/binary>>)}.

-spec func2({atom(), binary()}) -> binary().
func2({Reason, Msg}) ->
    case func1({Reason, Msg}) of
        ok ->
            ok;
        {error, Tracke} ->
            {error, tracke:chain(Tracke, {<<<<"func2: ">>/binary, Msg/binary>>, ahhh})}
    end.

calc1(N, M) ->
    case N < M of
        true ->
            {ok, M};
        false ->
            {error, tracke:new(too_large)}
    end.

underscode_func(_, dummy) ->
    ok;
underscode_func(X, _) ->
    {error, tracke:new({should_be_change, X})}.
