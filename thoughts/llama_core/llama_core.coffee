_ = require 'underscore'

class LlamaObject
  constructor: (@___type_name,
                @___constr_name,
                @___values=[],
                @___attrs={},
                @parent,
                affect) ->
    if affect? then affect @
    for [key, val] in _.pairs ___attrs
      Object.defineProperty @, key,
        get: -> @get ___constr_name, key
  deref: (constr_name, idx) ->
    if @___constr_name is constr_name
      if idx < @___values.length
        @___values[idx]
      else _throw DerefRangeError idx
    else if @___parent? then @___parent.deref constr_name, idx
    else _throw ConstructorNotFoundError constr_name
  get: (constr_name, key) ->
    if @___constr_name is constr_name
      if @___attrs[key]?
        @___attrs[key]
      else _throw NoAttributeError key
    else if @parent? then @parent.get constr_name, idx
    else _throw ConstructorNotFoundError constr_name

is_instance = (obj, name) ->
  # Check some primitive types here...
  return true if obj.___type_name is name
  obj.___parent? and is_instance obj.___parent, name

_throw = (exc) ->
  if not is_instance exc, 'Exception'
    throw new Error 'Attempt to throw a non-exception'
  throw exc

# object Maybe a = Nothing | Just a
Nothing = new LlamaObject 'Maybe', 'Nothing'
Just = (_a) -> new LlamaObject 'Maybe', 'Just', [_a]

NoAttributeError = (attr) -> (obj) ->
  parent = Exception "Attribute #{show attr} not found on object #{show obj}"
  new LlamaObject 'NoAttributeError', 'NoAttributeError', [attr], {}, parent

ConstructorNotFoundError = (constrName) ->
  parent = Exception "Constructor #{show constr_name} not found on object #{show obj}"
  new LlamaObject 'NoAttributeError', 'NoAttributeError', [attr], {}, parent

# object Exception =
#   Exception (msg: Str) {
#    trace := Sys\get_trace()
#   }
#   with !trace: [Str]
get_trace = -> new Error().stack.split '\n'
Exception = (msg) ->
  new LlamaObject 'Exception', 'Exception', [msg], {}, null, (self) ->
    self.___attrs.trace = get_trace()

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

_plus = (x) -> (y) -> x + y
_minus = (x) -> (y) -> x - y
_times = (x) -> (y) -> x * y
_divide = (x) -> (y) -> x / y
_append = _plus

show = (x) -> x
println = console.log

exports.LlamaObject = LlamaObject
exports.LookupError = LookupError
exports.PatternMatchError = PatternMatchError
exports.is_instance = is_instance
exports.Exception = Exception
exports.Nothing = Nothing
exports.Just = Just
