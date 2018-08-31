-record(tracke, {reason :: term(),
                 histories :: [history()]}).

-record(history, {module :: module(),
                  function :: atom(),
                  args :: [term()],
                  line :: non_neg_integer(),
                  aux :: term()}).

-type tracke() :: #tracke{}.

-type history() :: #history{}.
