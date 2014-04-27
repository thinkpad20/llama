what if we supported interface-style reflection, ONLY for functions where the first argument is the type? E.g.

good_lst: Show a. [a] = ['hello', 1, ('hey', 2/3)]; // this is ok
bad_list: Monad m. [m Str] = [Just 'hello', [~ 1,2,3], Either 0 'hello'] // this isn't OK

As in, you can only 'up-cast' to an interface type, if your trait is of the form

```
trait T a = f: a -> b;
```

Or could we go just on Kinds? E.g.

```
adds: Add m. [m -> m] = [+5, +'hello']
```

No, I'm thinking this isn't useful. Much better would be

```
trait Animal a = 
{ eat: Animal a'. a -> a' -> ();
  move: a -> Direction -> (); }

animals: Animal a. [a] = mut [gorilla, elephant, tiger];
try_to_be_eaten(animal: Animal a. a) = eaten after
{ eaten = mut False;
  for a in animals
  { try 
    { a.eat animal;
      eaten := True }
    catch Can'tEatError reason do
      println "#{a} can't eat #{animal.plural}. #[reason]" } }

for (); animals.length > 0; () do
  for a in animals do 
    if try_to_be_eaten a 
    { println '#{a} died.';
      animals.remove! a } 
    else
    { println '#{a} survived!';
      a.move random_direction }
```

Then it seems that we could do reflective type classes, AND Haskell style. Not sure though.
