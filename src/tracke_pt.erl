-module(tracke_pt).

-export([parse_transform/2]).

-type form() :: {attribute, line(), module, module()} |
                {function, line(), atom(), non_neg_integer(), [clause()]} |
                erl_parse:abstract_form().

-type clause() :: {clause, line(), Args :: [term()], Guards :: [term()], Body :: [expr()]} |
                  erl_parse:abstract_clause().

-type expr() :: record(atom()) |
                record_field() |
                erl_parse:abstract_expr() |
                term().

-type record(Name) :: {record, line(), Name, [record_field()]}.

-type record_field() :: {record_field, line(), {atom, line(), atom()}, expr()}.

-type line() :: non_neg_integer().

-type history_components() :: map().

%%====================================================================
%% API functions
%%====================================================================
-spec parse_transform([form()], [compile:option()]) -> [form()].
parse_transform(Forms, _) ->
    walk_forms(Forms, #{module => module(Forms),
                        function => undefined,
                        args => undefined,
                        line => undefined,
                        aux => undefined}).

%%====================================================================
%% Internal functions
%%====================================================================
-spec module([form()]) -> module().
module([{attribute, _, module, Module} | _]) ->
    Module;
module([_ | Rest]) ->
    module(Rest).

-spec walk_forms([form()], history_components()) -> [form()].
walk_forms(Forms, History) ->
    lists:map(fun({function, Line, Name, Arity, Clauses}) ->
                      {function, Line, Name, Arity, walk_clauses(Clauses, History#{function => Name})};
                 (Form) ->
                      Form
              end,
              Forms).

-spec walk_clauses([clause()], history_components()) -> [clause()].
walk_clauses(Clauses, History) ->
    lists:map(fun({clause, Line, Args, Guards, Body}) ->
                      {clause, Line, Args, Guards, walk_exprs(Body, History#{args => args_to_cons(Line, Args)})};
                 (Clause) ->
                      Clause
              end,
              Clauses).

-spec walk_exprs([expr()], history_components()) -> [expr()].
walk_exprs(Exprs, History) ->
    lists:map(fun({call, Line, {remote, _, {atom, _, tracke}, {atom, _, new}}, ArgExpr}) ->
                      handle_tracke_new(ArgExpr, History#{line => Line});
                 ({call, Line, {remote, _, {atom, _, tracke}, {atom, _, chain}}, ArgExpr}) ->
                      handle_tracke_chain(ArgExpr, History#{line => Line});
                 ({call, Line, {remote, _, {atom, _, tracke}, {atom, _, reason}}, ArgExpr}) ->
                      handle_tracke_reason(ArgExpr, History#{line => Line});
                 (Expr) when is_list(Expr) ->
                      walk_exprs(Expr, History);
                 (Expr) when is_tuple(Expr) ->
                      list_to_tuple(walk_exprs(tuple_to_list(Expr), History));
                 (Expr) ->
                      Expr
              end,
              Exprs).

-spec handle_tracke_new([expr()], history_components()) -> record(tracke).
handle_tracke_new([ReasonExpr, AuxExpr], History) ->
    handle_tracke_new([ReasonExpr], History#{aux => AuxExpr});
handle_tracke_new([ReasonExpr], #{line := Line} = History) ->
    % This corresponds to the following expression.
    % #tracke{reason = Reason
    %         histories = [NewHistory]}
    HistoryExpr = make_history_record(History),
    make_tracke_record(Line, ReasonExpr, {cons, Line, HistoryExpr, {nil, Line}}).

-spec handle_tracke_chain([expr()], history_components()) -> record(tracke).
handle_tracke_chain([ReasonExpr, AuxExpr], History) ->
    handle_tracke_chain([ReasonExpr], History#{aux => AuxExpr});
handle_tracke_chain([{var, _, _} = TrackeVarExpr], #{line := Line} = History) ->
    % This corresponds to the following expression.
    % #tracke{reason = Reason#tracke.reason,
    %         histories = [NewHistory | Reason#tracke.histories]}
    HistoryExpr = make_history_record(History),
    {record, Line, tracke, [make_record_field(Line, reason, {record_field, Line, TrackeVarExpr, tracke, {atom, Line, reason}}),
                            make_record_field(Line, histories, {cons, Line, HistoryExpr, {record_field, Line, TrackeVarExpr, tracke, {atom, Line, histories}}})]};
handle_tracke_chain(_, _) ->
    error('You can ONLY use a variable as the argument of tracke:chain').

-spec handle_tracke_reason([expr()], history_components()) -> expr().
handle_tracke_reason([{var, _, _} = TrackeVarExpr], #{line := Line}) ->
    % This corresponds to the following expression.
    % Reason#tracke.reason
    {record_field, Line, TrackeVarExpr, tracke, {atom, Line, reason}};
handle_tracke_reason(_, _) ->
    error('You can ONLY use a variable as the argument of tracke:reason').

-spec make_tracke_record(line(), expr(), expr()) -> record(tracke).
make_tracke_record(Line, ReasonExpr, HistoryExpr) ->
    {record, Line, tracke, [make_record_field(Line, reason, ReasonExpr),
                            make_record_field(Line, histories, HistoryExpr)]}.

-spec make_history_record(history_components()) -> record(history).
make_history_record(#{module := Module,
                      function := Function,
                      args := Args,
                      line := Line,
                      aux := Aux0}) ->
    Aux = case Aux0 of
              undefined ->
                  {atom, Line, undefined};
              _ ->
                  Aux0
          end,

    {record, Line, history, [make_record_field(Line, module, {atom, Line, Module}),
                             make_record_field(Line, function, {atom, Line, Function}),
                             make_record_field(Line, args, Args),
                             make_record_field(Line, line, {integer, Line, Line}),
                             make_record_field(Line, aux, Aux)]}.

-spec make_record_field(line(), atom(), expr()) -> record_field().
make_record_field(Line, Name, Expr) ->
    {record_field, Line, {atom, Line, Name}, Expr}.

-spec args_to_cons(non_neg_integer(), [expr()]) -> {nil, line()} | {cons, line(), expr(), expr()}.
args_to_cons(Line, List) ->
    lists:foldr(fun({var, L, '_'}, Acc) ->
                        %% Replace `_' variable to atom.
                        {cons, Line, {atom, L, '_'}, Acc};
                   (X, Acc) ->
                        {cons, Line, X, Acc}
                end,
                {nil, Line},
                List).
