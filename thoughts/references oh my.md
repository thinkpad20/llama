```
i = mut 0 # i points to a box on the stack. Let's say its address is 3.
          # At this point `i` will forever point to address 3 (within this
          # scope). Its value is currently 0.
i := 1    # The `:=` operator evaluates what's on the right, and then
          # inserts it at the address given. So it inserts 1 into address
          # 3.
foo (n: ref Int) = n := 15 # foo takes a ref for an argument. So it's a little
                           # different. Here the value of `n` is a pointer.
                           # E.g. if called with `i`, the value of `n` would
                           # be 3. The address of `n` is on the stack; let's
                           # say its address is 5.
                           # However, since we're smart, we know that when we
                           # assign `n` to `15`, we're not putting `15` at
                           # address 5, but at address `3`. 

```
Rules for `:=`:
  * let `rvalue` = value of the right side.
  * If the left side is a reference type, then resolve the reference, 
    and stick `rvalue` into that address.
  * Otherwise, the left side had better be a variable name. Find the
    box to which that variable points, and stick `rvalue` into that
    box.

This suggests a recursive algorithm:

```
update(left_side, right_side):
  if left_side is a reference:
    copy(left_side.address, right_side)
  else if left_side is a variable:
    update(left_side.get_reference, right_side)
  else:
    error
```

So essentially everything is either a reference, which has an address, or is a variable, from which we can obtain a reference.

OK, so how about

```
foo (n: ref Int) = n := n + 15
bar (n: ref Int) = foo (n + 3)
# tough one here!
baz (n: ref Int) = foo (foo n)
```

Hm, that doesn't really work because of the typing, but basically I'm just trying to get the idea of stringing together `ref` functions. Need to thinking, as usual. 
