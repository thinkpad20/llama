foo = json {'a'=>{'b'=>{'c'=>'d'}}}
foo['a']['b']['c'] := 'e'

^ is what we would like to do. However, we can't because

```
foo['a'] : Ref JValue
```

So we'd have to do

```
foo['a']!['b']!['c'] := 'e'
```

We could write a function to do this:

```
_!!_: JValue -> String -> JValue
dict !! key = dict[key]!

foo !! 'a' !! 'b' !! 'c'
```

Or we could do it with multiparam trait. Or we could have multiple indexings:

```
foo : {Int=>String}
foo[1] : Int
foo[|1]: Ref Int
```

Then we could write

```
foo['a']['b'][|'c'] := 'e'
```

That seems like a reasonable possibility.

```
trait Index a key = @index: a val -> key -> val
```

Problem now is the kinds don't match up... because a dict is parameterized by two types (key and val) while a vector has only one... blargh.
