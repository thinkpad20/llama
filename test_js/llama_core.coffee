_ = require 'underscore'

Object.prototype.deref = (idx) ->
  return @ if idx is 0
  _throw PatternMatchError()

# So that we can use `throw` in expressions
_throw = (exc) -> throw exc

class LlamaObject
  constructor: (@___type_name,
                @___constr_name,
                @___values=[],
                @___attrs={},
                @parent,
                affect) ->
    if affect? then affect @
  deref: (constr_name, idx) ->
    if @___constr_name is constr_name
      if idx < @___values.length
        @___values[idx]
      else _throw DerefRangeError idx
    else if @___parent? then @___parent.deref constr_name, idx
    else _throw ConstructorNotFoundError constr_name
  get_attr: (constr_name, key) ->
    if @___constr_name is constr_name
      if @___attrs[key]?
        @___attrs[key]
      else _throw NoAttributeError key
    else if @parent? then @parent.get_attr constr_name, idx
    else _throw ConstructorNotFoundError constr_name

is_instance = (obj, name) ->
  return true if _.isArray obj and name is 'Array'
  return true if _.isNumber obj and name is 'Num'
  return true if _.isString obj and name is 'Str'
  return true if _.isBoolean obj and name is 'Bool'
  return true if obj.___type_name is name
  obj.___parent? and is_instance obj.___parent, name

get_trace = -> new Error().stack.split '\n'

Exception = (msg) ->
  new LlamaObject 'Exception', 'Exception', [msg], {}, null, (self) ->
    self.___attrs.trace = get_trace()

NoAttributeError = (attr) -> (obj) ->
  parent = Exception "Attribute #{show attr} not found on object #{show obj}"
  new LlamaObject 'NoAttributeError', 'NoAttributeError', [attr], {}, parent

ConstructorNotFoundError = (constrName) ->
  parent = Exception "Constructor #{show constr_name} not found on object #{show obj}"
  new LlamaObject 'NoAttributeError', 'NoAttributeError', [attr], {}, parent

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

Number.prototype.get_attr = (name) ->
  return this[name] if this[name]?
  _throw NoAttributeError name

String.prototype.get_attr = (name) ->
  return this[name] if this[name]?
  _throw NoAttributeError name

Function.prototype._call = (arg) ->
  if _.isArray arg then @apply null, arg else @ arg

# object Maybe a = Nothing | Just a
Nothing = new LlamaObject 'Maybe', 'Nothing'
Just = (_a) -> new LlamaObject 'Maybe', 'Just', [_a]

_plus = (x) -> (y) -> x + y
_minus = (x) -> (y) -> x - y
_times = (x) -> (y) -> x * y
_divide = (x) -> (y) -> x / y
_append = _plus

show = (x) -> x
println = console.log
print = process.stdout.write

exports.LlamaObject = LlamaObject
exports.is_instance = is_instance
exports.Exception = Exception
exports.LookupError = LookupError
exports.PatternMatchError = PatternMatchError
exports.ConstructorNotFoundError = ConstructorNotFoundError
exports.NoAttributeError = NoAttributeError
exports.Nothing = Nothing
exports.Just = Just
exports._throw = _throw
exports.show = show
exports.println = println
exports.print = print
exports._plus = _plus
exports._minus = _minus
exports._times = _times
exports._divide = _divide
exports._append = _plus
