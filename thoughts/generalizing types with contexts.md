ex:

foo x:a : [a] =
  # here the context contains x:{a0}
  bar: {baz: a -> Int} => Int =
    # now the context has {x: {a0}, baz: {a1 -> Int}}
    baz x
  # now we're done with bar, the context has
  #   {x: {a0}, bar: Int}
  baz (y:a): Int =
    # context = {x: {a0}, bar: Int, y:{a2}}, free = {a0, a2}
    1
    # determine baz: a2 -> Int
    # a2 is free in baz but NOT free in its surrounding env.
    # with baz's context, its type is
    # baz : {x: {a0}, bar: Int, y:{a2}} => a2 -> Int
    # but its argument name will be removed from its context, so
    # the type returned will be {x: {a0}, bar: Int} => a2 -> Int
    # then we generalize *this* type with respect to the outer
    # context, which from before was {x: {a0}, bar: Int}
    # So to generalize, we need a context and a type.
    # We pull the free variables out of baz's type (a0 and a2),
    # remove the variables which are free in the context (a0), and
    # create a polytype out of the remaining ones. So we get
    # baz : forall a. {x: {a0}, bar: Int} => a -> Int.
    # we should probably remove the redundant keys from the context, so
    # the actual type we return is baz : forall a. a -> Int.
  # done with baz, context has
  #   {x: {a0}, bar: Int, baz: {forall a. a -> Int}}
  replicate bar x
