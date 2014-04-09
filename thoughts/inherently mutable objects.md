Issue: some objects are "inherently mutable", like a ref cell or a mutable hash table. In these cases, is it meaningful to use the "mut" keyword, or more specifically, does it provide the guarantees that we want? I want to say yes but we need to be sure.

```
llama> my_mutable_dict = mut {d! 1 => 2, 2 => 9, 10 => 1}
llama> my_func (mut dict) = dict.add! 4 5
llama> my_func my_mutable_dict
llama> println my_mutable_dict
{d! 1 => 2, 2 => 9, 4 => 5, 10 => 1}
llama> :t my_mutable_dict[: 0]
my_mutable_dict[: 0] : ref Num
llama> my_add! (ref dict) key val = dict[: key] := val
llama> :p my_add!
my_add! :-: Local
```

I think it works, but I'm not sure if it makes it hard (or impossible) to ensure functional purity. On a small scale it seems OK but we need to be careful.

Refs are viral?

```
llama> object Foo = Foo with bar = 1
llama> foo = Foo
llama> :t foo
foo : Foo
llama> :t foo\bar
foo\bar : Num
llama> foo2 = ref Foo
llama> :t foo2
foo2 : ref Foo
llama> :t foo2\bar
foo2\bar : ref Num
llama> object Bar = Bar with foo = ref 1
llama> bar = Bar
llama> :t bar
bar : Bar
llama> :t bar\foo
bar\foo : ref Num
llama> bar2 = ref Bar
llama> :t bar2
bar2 : ref Bar
llama> :t bar2\foo
bar2\foo : ref ref Num
```

Intellesting. The implication details of this seem like they'd need a little work though.
