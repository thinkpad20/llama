New approach to contexts: refining

We can look at the issue of context validation as context *refining*. Essentially, every assertion in a context is reduced to its most simple form.

For example:

* `{Eq (Maybe a)}` -> `{Eq a}`, because `Eq (Maybe a)` is true if and only if `Eq a`.
* `{Eq a}` -> `{Eq a}`, because it's already in its simplest form (assuming there's no instance for a generic `a` in `Eq`, which there certainly shouldn't be).
* `{AddSelf [a]}` -> `{}`, because `AddSelf [a]` is always true (assuming we've got an instance of `AddSelf` for `[a]`).
* `{IntLit Int}` -> `{}`, for the same reason as above.
* `{StrLit Int}` -> `ERROR`, because there is no instance of `StrLit` for `Int`, so it's a contradiction.

The issue still remains, though, of "what's an instance"?
