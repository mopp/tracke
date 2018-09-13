## tracke
Trackable error library.

### features
- Make your error trackable.

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
```

The result is the following:
```console
```
