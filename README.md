## tracke
Trackable error library.

### Example
```erlang
-compile([{parse_transform, tracke_pt}]).

main() -> 
    case calc1(999, 1) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            case tracke:reason(Reason) of
                too_large ->
                    _ = io:format("~s", [tracke:format(Reason)]);
                _ ->
                    tracke:chain(Reason)
            end
    end.

calc1(N, M) ->
    case N < M of
        true ->
            {ok, M};
        false ->
            tracke:error(too_large)
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
