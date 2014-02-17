
foo (arr: [Number]) = map (+ 3) arr
bar = foo [1,2,3] # bar == [4,5,6]
print (bar 1) # prints 5

# I need to be able to make array assignments on mutable vectors.
# for example,

map! (f: a->b) (arr: mut [a]) = 
  for i in arr.range do arr{! i} := f (arr i)

# Let's think about semantics for a moment... in C, arr[i] is a pointer
# to the ith offset from the beginning of arr. So if arr is of type `int *`
# then `arr[i]` is of type `int`. But then used in an assignment, `arr[i]`
# modifies `arr`

# in this definition of @call, arr is not mutable, and `arr i` simply
# returns a copy of whatever's at the `i`th index of `arr`. Or is it a 
# reference to it, not a copy? That's how it is in C... but not exactly.

# in C, let's say we have
int increment(int i) { return i + 1; }
void incr_all(int *nums, int length) {
    int i;
    for (i = 0; i < length; ++i) {
        nums[i] = increment(nums[i]);
    }
}
# if we instead wrote
void incr_all(int *nums, int length) {
    int i;
    for (i = 0; i < length; ++i) {
        increment(nums[i]);
    }
}
# it would have no effect. If we wanted that to have an effect, we'd need
void increment(int *i) { *i += 1; }
void incr_all(int *nums, int length) {
    int i;
    for (i = 0; i < length; ++i) {
        increment(&nums[i]);
    }
}

# So, if we want to change an object's value outside of a function, we need
# to give it a reference to that object.

# Going further then, if we think of (:=) as a function, then its type is
(:=) : mut a -> a -> ()

# and we can do things like
incrAll! (vec: mut [Number]) = map! (a: mut a => a := a + 1) vec

# where (a: mut a => a := 0): mut a -> ()
# ah, so here's the issue. We can't do that, because we're not mutating `a`...
# we're mutating `vec`!
# If we wrote `map!` like so:

map! (f: mut a -> a) (vec: mut [a]) = 
  for a in vec do f a

# then the definition of `incrAll!` would be ok.

# So the key question is, what does a function return: a value, or a 
# reference to that value?

incr (n: mut Int) = n := n + 1
foo = mut [1]
print foo # [1]
print foo 0 # 1
print foo{!0} # 1, but it's a reference!
incr (foo 1) # no error, but has no effect
print foo # [1]
incr foo{!0} # this time it works
print foo # [2]

@call (arr: [a]) (index: Number) = arr{index}
