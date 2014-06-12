Here's a crazy concise version of K-nearest neighbors in (hypothetical) Llama:

```llama
type LabelPixel label:Int pixels:[Int]

slurp_file: FilePath -> [LabelPixel] = .read.map(.lines.tail.map(make)) after
  make = .trim.split(',').(v -> LabelPixel v.head.read v.tail.map(read))

classify (training: [LabelPixel]) (pxls: [Int]): Int = mini.label after
  dist v1 v2 = zipwith(-)(v1)(v2).map(^ 2).sum
  mini = training[training.map(.pixels.dist pxls).min_index]

println "Percentage correct: #{percent_correct}" after
  training_set = slurp_file "trainingsample.csv"
  validation_sample = slurp_file "validationsample.csv"
  is_correct x = classify training_set x.pixels == x.label
  correct = validation_sample.filter(is_correct)
  percent_correct = correct / validation_sample.length.float * 100.0
```

Note that we don't have to do any imports: all of the functions we need should be in the standard namespace, and any ambiguities are resolved by the types.

The vectors here will probably be 2-3 finger trees. We could change to Array for improved performance.

Note some of the syntactic tricks using lambda-dots:

```
.read.map(.lines.tail.map(make))
== x -> x.read.map(y -> y.lines.tail.map(make))
== x -> map (y -> y.lines.tail.map(make)) (read x)
== x -> map (y -> map make (tail (lines y))) (read x)


.trim.split(',').(v -> LabelPixel v.head.read v.tail.map(read))
== x -> x.trim.split(',').(v -> LabelPixel v.head.read v.tail.map(read))
== x -> (v -> LabelPixel v.head.read v.tail.map(read)) (split ',' (trim x))
== x -> (v -> LabelPixel (read(head v)) (map read (tail v))) (split ',' (trim x))

.pixels.dist pxls
== x -> x.pixels.dist pxls
== x -> dist (pixels x) pxls
```
