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

var End,Cons,l0,l1,length,print_list;End=new LlamaObject("List","End",[],{},null,null);function Cons(a){return function(list){return new LlamaObject("List","Cons",[a,list],{},null,null);};};l0=End;l1=Cons("a")(l0);function length(_arg){if(_arg.___constr_name==="End"){return 0;}else if(_arg.___constr_name==="Cons"){var x,l;x=_arg.deref("",0);l=_arg.deref("",1);return length(l)+length(l);}else{throw PatternMatchError();};};function print_list(_arg){if(_arg.___constr_name==="End"){return __Tuple([]);}else if(_arg.___constr_name==="Cons"){var x,l;x=_arg.deref("",0);l=_arg.deref("",1);println(x);return print_list(l);}else{throw PatternMatchError();};};print_list(l1);
