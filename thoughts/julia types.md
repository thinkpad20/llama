```llama
foo : [_ of Show] = ['hello', 2]
```

->

```julia
foo = typed!(larray!([lstr("hello"), lint(2)]), tarray!(of!(tr!("Show"))))

type TVar! <: Type!
  name::String
  traits::Array{Trait!, 1}
end

type TConst! <: Type!
  name::String
  traits::Array{Trait!, 1}
  parent::Maybe{TConst!}
end

type TTuple! <: Type!
  contains::Array{Type!, 1}
end

type TApply! <: Type!
  left::Type!
  right::Type!
end

TApply!(t1::Type!, t2::Type!, t3::Type!) = TApply!(TApply!(t1, t2), t3)

noparent(name, traits) = TConst!(name, traits, Nothing{TConst!})
justname(name) = TConst!(name, [], Nothing{TConst!})
AbsFunction! = noparent("->", [tr!("Callable")])
TFunction!(from, to) = TApply!(AbsFunction!, from, to)
```
