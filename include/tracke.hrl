%% Export these records for dialyzer.
-record(history, {module :: module(),
                  function :: atom(),
                  args :: non_neg_integer(),
                  line :: non_neg_integer(),
                  aux :: term()}).

-record(tracke, {reason :: term(),
                 histories :: [#history{}]}).
