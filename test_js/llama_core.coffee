_ = require 'underscore'

Object.prototype.deref = (idx) ->
  return @ if idx is 0
  _throw PatternMatchError()

# So that we can use `throw` in expressions
_throw = (exc) -> throw JSON.stringify exc.get_attr 'Exception', 'trace'

class LlamaObject
  constructor: (@___type_name,
                @___constr_name,
                @___values=[],
                @___attrs={},
                @parent,
                affect) ->
    if affect? then affect @
  deref: (constr_name, idx) ->
    unless _.isString constr_name
      _throw TypeError 'constr_name needs to be a string!'
    unless _.isNumber idx
      _throw TypeError 'idx needs to be a number!'
    if @___constr_name is constr_name
      if idx < @___values.length
        @___values[idx]
      else _throw DerefRangeError idx, @
    else if @parent?
      @parent.deref constr_name, idx
    else _throw ConstructorNotFoundError constr_name, @
  get_attr: (constr_name, key) ->
    unless _.isString constr_name
      _throw TypeError 'constr_name needs to be a string!'
    unless _.isString key
      _throw TypeError 'key needs to be a string!'
    if @___constr_name is constr_name
      if @___attrs[key]? then @___attrs[key]
      else _throw NoAttributeError key, @
    else if @parent?
      @parent.get_attr constr_name, key
    else _throw ConstructorNotFoundError constr_name, @

is_instance = (obj, name) ->
  return true if _.isArray obj and name is 'Array'
  return true if _.isNumber obj and name is 'Num'
  return true if _.isString obj and name is 'Str'
  return true if _.isBoolean obj and name is 'Bool'
  return true if obj.___type_name is name
  obj.___parent? and is_instance obj.___parent, name

Exception = (msg) ->
  new LlamaObject 'Exception', 'Exception', [msg], {}, null, (self) ->
    self.___attrs.trace = new Error(msg).stack

NoAttributeError = (attr, obj) ->
  parent = Exception "Attribute #{show attr} not found on object #{show obj}"
  new LlamaObject 'NoAttributeError', 'NoAttributeError', [attr], {}, parent

DerefRangeError = (idx, obj) ->
  parent = Exception "Index #{show idx} out of range on object #{show obj}"
  new LlamaObject 'NoAttributeError', 'NoAttributeError', [attr], {}, parent

ConstructorNotFoundError = (constr_name, obj) ->
  parent = Exception "Constructor #{show constr_name} not found on object #{show obj}"
  new LlamaObject 'ConstructorNotFoundError', 'ConstructorNotFoundError', [constr_name, obj], {}, parent

# object LookupError (a : Show) <: Exception =
#   LookupError (key: a) <: Exception 'Key #{key} was not found'
LookupError = (key) ->
  parent = Exception _append(_append('Key ')(show key))(' was not found')
  new LlamaObject 'LookupError', 'LookupError', [key], {}, parent

# object PatternMatchError (a : Show) a <: Exception =
#   PatternMatchError (pat: a) (val: a) <:
#     Exception "Couldn't match value #{val} with pattern #{pat}"
PatternMatchError = (pat) -> (val) ->
  s = _append("Couldn't match value ")(show val)
  s = _append(s)(' with pattern ')
  s = _append(s)(show pat)
  parent = Exception s
  new LlamaObject 'PatternMatchError', 'PatternMatchError', [pat, val], {}, parent

TypeError = (msg) ->
  parent = Exception msg
  new LlamaObject 'TypeError', 'TypeError', [msg], {}, parent

Number.prototype.get_attr = (name) ->
  return this[name] if this[name]?
  _throw NoAttributeError name

String.prototype.get_attr = (name) ->
  return this[name] if this[name]?
  _throw NoAttributeError name

Function.prototype._call = (arg) ->
  if _.isArray arg then @apply null, arg else @ arg

_plus = (x) -> (y) -> x + y
_minus = (x) -> (y) -> x - y
_times = (x) -> (y) -> x * y
_divide = (x) -> (y) -> x / y
_append = (a) -> (b) ->
  if _.isNumber(a) and _.isNumber(b)
    a + b
  else if _.isString(a) and _.isString(b)
    a + b
  else if is_instance(a, 'List') and is_instance(b, 'List')
    if a.___constr_name is 'End'
      b
    else
      val = a.deref 'Cons', 0
      next = a.deref 'Cons', 1
      Cons(val)(_append(next)(b))
  else
    throw TypeError "_append not implemented for #{a}, #{b}"

str_to_list = (str) ->
  res = End
  for c in str.split("").reverse().join("")
    res = Cons(c)(res)
  res

_apply = (f) -> (x) -> f x
_f_apply = (x) -> (f) -> f x
_comp = (f) -> (g) -> (x) -> f g x
_f_comp = (g) -> (f) -> (x) -> f g x

show = JSON.stringify
println = console.log
print = process.stdout.write

exports.LlamaObject = LlamaObject
exports.is_instance = is_instance
exports.Exception = Exception
exports.LookupError = LookupError
exports.PatternMatchError = PatternMatchError
exports.ConstructorNotFoundError = ConstructorNotFoundError
exports.NoAttributeError = NoAttributeError
exports._throw = _throw
exports.show = show
exports.println = println
exports.print = print
exports._plus = _plus
exports._minus = _minus
exports._times = _times
exports._divide = _divide
exports._append = _append
exports._f_apply = _f_apply
exports._apply = _apply
exports._f_comp = _f_comp
exports._comp = _comp
exports.str_to_list = str_to_list
