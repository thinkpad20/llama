var ConstructorNotFoundError, Exception, Just, LlamaObject, LookupError, NoAttributeError, Nothing, PatternMatchError, is_instance, print, println, show, _divide, _minus, _plus, _throw, _times;
LlamaObject = require('./llama_core').LlamaObject;
is_instance = require('./llama_core').is_instance;
Exception = require('./llama_core').Exception;
LookupError = require('./llama_core').LookupError;
PatternMatchError = require('./llama_core').PatternMatchError;
ConstructorNotFoundError = require('./llama_core').ConstructorNotFoundError;
NoAttributeError = require('./llama_core').NoAttributeError;
Nothing = require('./llama_core').Nothing;
Just = require('./llama_core').Just;
_throw = require('./llama_core')._throw;
show = require('./llama_core').show;
println = require('./llama_core').println;
print = require('./llama_core').print;
_plus = require('./llama_core')._plus;
_minus = require('./llama_core')._minus;
_times = require('./llama_core')._times;
_divide = require('./llama_core')._divide;
_plus = require('./llama_core')._plus;
End = require('./llama_core').End;
Cons = require('./llama_core').Cons;

println("hello, world!");

var lappend, reverse, show_list, to_list;
lappend = function (a) {
  return function (b) {
    if (__Tuple([a, b]).___constr_name === "__Tuple" && __Tuple([a, b]).___values.length === 2 && __Tuple([a, b]).deref("", 0).___constr_name === "End") {
      var b;
      b = __Tuple([a, b]).deref("", 1);
      return b;
    } else if (__Tuple([a, b]).___constr_name === "__Tuple" && __Tuple([a, b]).___values.length === 2 && __Tuple([a, b]).deref("", 0).___constr_name === "Cons") {
      var val, next, b;
      val = __Tuple([a, b]).deref("", 0).deref("", 0);
      next = __Tuple([a, b]).deref("", 0).deref("", 1);
      b = __Tuple([a, b]).deref("", 1);
      return Cons(val)(lappend(next)(b));
    } else {
      throw PatternMatchError();
    }
  };
};
reverse = function (list) {
  if (list.___constr_name === "End") {
    return End;
  } else if (list.___constr_name === "Cons") {
    var a, list;
    a = list.deref("", 0);
    list = list.deref("", 1);
    return lappend(reverse(list))(a);
  } else {
    throw PatternMatchError();
  }
};
show_list = function (list) {
  if (list.___constr_name === "End") {
    return "";
  } else if (list.___constr_name === "Cons") {
    var a, next;
    a = list.deref("", 0);
    next = list.deref("", 1);
    return _append(_append(show(a))(", "))(show_list(next));
  } else {
    throw PatternMatchError();
  }
};

to_list = function (str) {
  if (str === '') {
    return End;
  } else {
    var c, rest;
    c = str[0];
    rest = str.slice(1);
    return Cons(c)(to_list(rest));
  }
}

var list = to_list('hello world!')
var s = show_list(list);
println(s);
