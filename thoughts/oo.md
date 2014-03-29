## Object-orientation, overloading?

```
object Person = Person (name: Str, @age: Int);

object Manager extends Person = 
  Manager name age (department: Str) extends Person (name, age);

object Engineer extends Person = 
  Engineer name age (specialty: Str) extends Person (name, age);

introduce p@(Person name) = println "Hi, my name is #[name] and I'm #{p.age}."
speak (Person _) = println "I don't have anything to say."
speak (m: Manager) = 
  println "I'm the manager of #[m.department]."
speak (Engineer _ a s) = 
  println "I specialize in #[s]!"

tom = Person "Tom" 23
dick = Manager "Dick" 40 "engineering"
harry = Engineer "Harry" 19 "artificial intelligence"

> 
> talk p = {introduce p; speak p}  
> tom.talk
Hi, my name is Tom and I'm 23. 
I don't have anything to say.
> dick.talk
Hi, my name is Dick and I'm 40. 
I'm the manager of engineering.
> harry.talk
Hi, my name is Harry and I'm 19.
I specialize in artificial intelligence!
```

So "overloading" is as simple as just defining the method for the class. If the method doesn't exist for the object, it will try its parent, and so on.

One thing to note, though, is that the return types need to be consistent; otherwise we could have

```
foo (p: Person) = p.name.length * p.age
foo (m: Manager) = m.department.upper_case

bar (p: Person) = p.foo # we don't know what this returns!
```

So if we're extending `f` for an object `o` with a parent `p`, and `p` implements `f`, then the return type for `o`'s version must equal that of `p`. Similarly if `p` has a child which implements `g`, then extending `g` for `p` must yield the same return type, etc.

Thought: the latter condition is harder to ensure... but we'll cross this bridge later.
