## Llama todo list for this week(?)

* Improve unit test library code
* For parser:
  * parse `after` structure
  * parse `object` declarations
  * parse `typedef`s?
  * parse `mut` and `ref`
* For type checker:
  * be able to define recursive functions
  * make `for`s and `while`s always return `()`
  * resolve `typedef`s
  * tighten up multiple dispatch code, choose either sets or multiple names
  * code for choosing most specific match?
  * typing mutable variables and references
  * unit test suite
* For evaluator:
  * evaluate `while` loops
  * handle mutable variables and references
  * REPL should perpetuate state -- better quick 'n dirty than none at all
  * use type table for multiple dispatch, perhaps receive this info from 
    type checker?