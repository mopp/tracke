## tracke
Trackable error library.

### features
- Make errors trackable
- Support OTP-18 and the laters
- Embed error reasons in the type `tracke/1`
    * e.g., `tracke(your_error_atom)`

### API
- `-spec tracke:new(Reason) -> tracke:tracke(Reason)`
    * Make `tracke` object.
    * Define your error as the `Reason`.
- `-spec tracke:new(Reason, Aux :: term()) -> tracke:tracke(Reason)`
    * This function works same as `tracke:new/1`.
    * The `Aux` is embedded to history.
- `-spec tracke:chain(tracke(Reason)) -> tracke(Reason)`
    * Append history to `tracke(Reason)` and keep the reason.
    * This function works same as `tracke:new/1` if `Reason` is NOT `tracke(Reason)`
- `-spec tracke:chain(tracke(Reason), Aux :: term()) -> tracke(Reason)`
    * This function works same as `tracke:chain/1`.
    * The `Aux` is embedded to history.
- `-spec tracke:reason(tracke(Reason)) -> Reason`
    * Extract error reason from `tracke`.
    * Return it without doing anythings if not `tracke` object is given.
- `-spec tracke:format(tracke(Reason)) -> binary()`
    * Build human readable error reason and its history.

### Constrains
- You can ONLY use a variable as the arugment of `tracke:chain/1`, `tracke:chain/1` and `tracke:reason/1`.
    * This measn a forms of `tracke:chain(Reason)` is acceptable.
    * `tracke:chain(tracke:new(not_work))` is NOT supported because this library uses `parse_transform/2`.

### Example
See [src/tracke_example.erl](src/tracke_example.erl) for details.

```erlang
-spec normal_usage(integer()) -> ok.
normal_usage(X) ->
    case func1(X) of
        {ok, V} ->
            io:format("Ok: ~p~n", [V]);
        {error, Reason} ->
            io:format("~s~n", [tracke:format(Reason)]),
            case tracke:reason(Reason) of
                zero ->
                    io:format("Your input is zero.~n", []);
                negative ->
                    io:format("Your input is negative.~n", []);
                boring_number ->
                    io:format("Your input is boring.~n", [])
            end
    end,
    ok.

-spec func1(integer()) -> {ok, binary()} | {error, tracke(zero | negative | boring_number)}.
func1(0) ->
    {error, tracke:new(zero)};
func1(X) when X < 0 ->
    {error, tracke:new(negative)};
func1(X) ->
    case fizzbuzz(X) of
        {ok, _} = Ok ->
            Ok;
        {error, Reason} ->
            {error, tracke:chain(Reason)}
    end.

-spec fizzbuzz(integer()) -> {ok, binary()} | {error, tracke(boring_number)}.
fizzbuzz(X) ->
    case {X rem 5 =:= 0, X rem 3 =:= 0} of
        {true, true} ->
            {ok, <<"fizzbuzz">>};
        {true, false} ->
            {ok, <<"fizz">>};
        {false, true} ->
            {ok, <<"buzz">>};
        {false, false} ->
            {error, tracke:new(boring_number)}
    end.
```

The result is the following:
```console
1> tracke_example:normal_usage(0).
Reason: zero
History:
    tracke_example:func1/1 at L39
        Args: [0]
        Aux: undefined

Your input is zero.
ok


2> tracke_example:normal_usage(-100).
Reason: negative
History:
    tracke_example:func1/1 at L41
        Args: [-100]
        Aux: undefined

Your input is negative.
ok


3> tracke_example:normal_usage(127).
Reason: boring_number
History:
    tracke_example:fizzbuzz/1 at L60
        Args: [127]
        Aux: undefined
    tracke_example:func1/1 at L47
        Args: [127]
        Aux: undefined

Your input is boring.
ok


4> tracke_example:normal_usage(15).
Ok: <<"fizzbuzz">>
ok
```
