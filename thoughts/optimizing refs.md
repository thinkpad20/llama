With a strong typing system, we can detect when we have `ref`s that we need to actually allocate memory for, and when we can simply allocate on the stack. For example:

```llama
foo = -> *i after
  i = ref 0
  while *i < 10
    println 'still going!'
    i += 1
```

In this case, we only use the ref internally, so there's no need to use a box.

```c++
int foo() {
  int i = 0;
  while (i < 10) {
    println("still going!");
    i += 1;
  }
  return i;
}

```

```llama
bar = -> i after
  i = ref 0
  while *i < 10
    println 'still going!'
    i += *i
```

In this case, we return a ref, so we need to allocate a reference on the stack.

```c++
typedef Ref std::shared_ptr;

Ref<int> bar() {
  Ref<int> i = new Ref<int>(0);
  while (*i < 10) {
    println("still going!");
    i += *i;
  }
  return i;
}
```
