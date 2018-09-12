## tracke
Trackable error library.

### API
- `-spec tracke:new(Reason :: term()) -> tracke()`
    * Make `tracke` object.
    * The argument accepts `Reason`.
- `-spec tracke:new(Reason :: term(), Aux :: term()) -> tracke()`
    * This function works same as `tracke:new/1`.
    * `Aux` is embedded to history.
- `-spec tracke:chain(Reason :: term()) -> tracke()`
    * Append history to `tracke()` and keep the reason.
    * This function works same as `tracke:new/1` if `Reason` is NOT `tracke()`
- `-spec tracke:chain(Reason :: term(), Aux :: term()) -> tracke()`
    * This function works same as `tracke:chain/1`.
    * `Aux` is embedded to history.
- `-spec tracke:reason(tracke() | term()) -> tracke()`
    * Extract error reason from `tracke`.
    * Return it without doing anythings if not `tracke` object is given.
- `-spec tracke:format(tracke()) -> binary()`
    * Build human readable error reason and its history.

### Example
```erlang
-compile([{parse_transform, tracke_pt}]).

main() ->
    case calc1(999, 1) of
        {ok, _} ->
            ok;
        {error, Reason0} ->
            case tracke:reason(Reason0) of
                too_large ->
                    _ = io:format("~s", [tracke:format(Reason0)]);
                Reason ->
                    {error, tracke:chain(Reason)}
            end
    end.

calc1(N, M) ->
    case N < M of
        true ->
            {ok, M};
        false ->
            {error, tracke:new(too_large)}
    end.
```

`tracke:format/1` example:
```console
Reason: sample_error_reason
History:
    tracke_tests:func1/1 at L92
        Args: [{sample_error_reason,<<"sample message">>}]
        Aux: <<"func1: sample message">>
    tracke_tests:func2/1 at L100
        Args: [{sample_error_reason,<<"sample message">>}]
        Aux: {<<"func2: sample message">>,ahhh}
```


## Build
```console
% rebar3 compile
```
