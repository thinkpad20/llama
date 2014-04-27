_ = require 'underscore'

# So that we can use `throw` in expressions
_throw = (exc) -> throw JSON.stringify exc.get_attr 'Exception', 'trace'

class LlamaObject
  constructor: (@type,
                @constr_name,
                @values=[],
                @attrs={},
                @parent,
                affect) ->
    if affect? then affect @
  deref: (constr_name, idx) ->
    if @constr_name is constr_name
      if idx < @values.length
        @values[idx]
      else _throw DerefRangeError idx, @
    else if @parent?
      @parent.deref constr_name, idx
    else _throw ConstructorNotFoundError constr_name, @
  get_attr: (constr_name, key) ->
    if @constr_name is constr_name
      if @attrs[key]? then @attrs[key]
      else _throw NoAttributeError key, @
    else if @parent?
      @parent.get_attr constr_name, key
    else _throw ConstructorNotFoundError constr_name, @
  is_constr: (constr_name) ->
    @constr_name is constr_name or @parent and @parent.is_constr constr_name
  repr: (with_attrs=false) ->
    result = @constr_name
    for val in @values
      if val.values.length <= 1
        result = "#{result} #{val.repr(with_attrs)}"
      else
        result = "#{result} (#{val.repr(with_attrs)})"
    if with_attrs then "#{result}#{@repr_attrs()}" else result
  repr_attrs: ->
    if _.size @attrs is 0 then ''
    else if _.size @attrs is 1
      name = (_.keys @attrs)[0]
      " with #{name}=#{@attrs[name].repr(true)}"
    else
      result = " with {"
      first = true
      for k, v of @attrs
        if first
          result = "#{result}{#{k}=#{v.repr(true)}"
          first = false
        else
          result = "#{result};#{k}=#{v.repr(true)}"
      "#{result}}"

Object.prototype.deref = (idx) ->
  return @ if idx is 0
  _throw PatternMatchError()

Object.prototype.get_attr = (name) ->
  return this[name] if this[name]?
  _throw NoAttributeError name

Object.prototype.values = []
Object.prototype.repr = -> "#{@}"
String.prototype.repr = -> JSON.stringify @

Array.prototype.deref = (idx) -> @[idx]

Function.prototype._call = (arg) ->
  if _.isArray arg then @apply null, arg else @ arg

TVar = (name) -> new LlamaObject 'Type', 'TVar', [name]
TConst = (name) -> new LlamaObject 'Type', 'TConst', [name]
TFunction = (a, b) -> new LlamaObject 'Type', 'TFunction', [a, b]
TApply = (a, b) -> new LlamaObject 'Type', 'TApply', [a, b]
TUnion = (types) ->
  new LlamaObject 'Type', 'TUnion', types
TTuple = (types) ->
  new LlamaObject 'Type', 'TTuple', types
TWildCard = new LlamaObject 'Type', 'TWildCard'

Num = TConst 'Num'
Str = TConst 'Str'
Number.prototype.type = Num
String.prototype.type = Str

Exception = (msg) ->
  type = TConst 'Exception'
  new LlamaObject type, 'Exception', [msg], {}, null, (self) ->
    self.attrs.trace = new Error(msg).stack

NoAttributeError = (attr, obj) ->
  type = TConst 'NoAttributeError'
  parent = Exception "Attribute #{attr.repr()} not found on object #{obj.repr()}"
  new LlamaObject type, 'NoAttributeError', [attr], {}, parent

DerefRangeError = (idx, obj) ->
  type = TConst 'DerefRangeError'
  parent = Exception "Index #{idx.repr()} out of range on object #{obj.repr()}"
  new LlamaObject type, 'NoAttributeError', [attr], {}, parent

ConstructorNotFoundError = (constr_name, obj) ->
  type = TConst 'ConstructorNotFoundError'
  parent = Exception "Constructor #{constr_name.repr()} not found on object #{show obj.repr()}"
  new LlamaObject type, 'ConstructorNotFoundError', [constr_name, obj], {}, parent

LookupError = (key) ->
  type = TConst 'LookupError'
  parent = Exception "Couldn't find key #{key}"
  new LlamaObject type, 'LookupError', [key], {}, parent

PatternMatchError = (pat, val) ->
  type = TConst 'PatternMatchError'
  parent = Exception "Couldn't match value #{val.repr()} with pattern #{pat.repr()}"
  new LlamaObject type, 'PatternMatchError', [pat, val], {}, parent

_TypeError = (msg) ->
  type = TConst 'TypeError'
  parent = Exception msg
  new LlamaObject type, 'TypeError', [msg], {}, parent

NoInstanceError = (trait, type) ->
  type = TConst 'TraitError'
  parent = Exception "No instance of #{trait} for #{type.repr()}"
  new LlamaObject type, 'TraitError', [msg], {}, parent

class Trait
  constructor: (@tvar, @funcs) -> @instances = {}
  add_instance: (type, impls) ->
    @instances[@get_type_name type] = impls
  get_instance: (func_name, type) ->
    if @instances[type_name = @get_type_name type]?
      @instances[type_name][func_name]
    else
      _throw NoInstanceError JSON.stringify name, type_name
  make_stub: (func_name, captures) ->
    new Stub @, func_name, captures
  get_type_name: (type) ->
    switch type.constr_name
      when 'TConst'
        type.deref 'TConst', 0
      when 'TApply'
        @get_type_name type.deref 'TApply', 0
      else
        _throw _TypeError "Can't make an instance with a #{type.constr_name}"

class Stub
  constructor: (@trait, @func_name, @captures) ->
  cast: (type) ->
    result = @trait.get_instance @func_name, type
    for arg in @captures
      result = result arg
    result

############ Show trait #############
Show = new Trait 'a', show: TFunction (TVar 'a'), TConst 'Str'
show = (a) ->
  _show = Show.get_instance 'show', a.type
  _show a

Show.add_instance Num, show: (n) -> "#{n}"
Show.add_instance Str, show: JSON.stringify

########### Append trait ############
Append = new Trait()
_append = (a) ->
  __append = Append.get_instance '_append', a.type
  __append a

Append.add_instance Num, _append: (a) -> (b) -> a + b
Append.add_instance Str, _append: (s1) -> (s2) -> s1 + s2
Append.add_instance (TApply (TConst '[~]'), TVar 'a'),
  _append: (l1) -> (l2) ->
    _throw 'list append not implemented'

########## Length trait ###########
Length = new Trait()
length = (a) ->
  _length = Length.get_instance 'length', a.type
  _length a

Length.add_instance Str, length: (s) -> s.length

########## Default trait ############
Default = new Trait()
_default = Default.make_stub 'default', []

Default.add_instance Str, default: ''
Default.add_instance Num, default: 0

_plus   = (x) -> (y) -> x + y
_minus  = (x) -> (y) -> x - y
_times  = (x) -> (y) -> x * y
_divide = (x) -> (y) -> x / y


############## A linked list implementation ################
List = (a) -> TApply (TConst '[~]'), a
End = new LlamaObject (List TWildCard), '[~]'
Cons = (a) -> (list) ->
  type = List a.type
  new LlamaObject type, '~', [a, list]

str_or_array_to_list = (str_or_array) ->
  res = End
  for i in [str_or_array.length - 1 .. 0] by -1
    res = Cons(str_or_array[i])(res)
  res

list_to_array = (list) ->
  result = []
  while list.is_constr '~'
    a = list.deref '~', 0
    result.push a
    list = list.deref '~', 1
  result.reverse()


_apply   = (f) -> (x) -> f x
_f_apply = (x) -> (f) -> f x
_comp    = (f) -> (g) -> (x) -> f g x
_f_comp  = (g) -> (f) -> (x) -> f g x

Maybe = (t) -> TApply (TConst 'Maybe'), t
Just = (a) ->
  type = Maybe a.type
  new LlamaObject type, 'Just', [a]
Nothing = new LlamaObject (Maybe TWildCard), 'Nothing'

println = console.log
print = process.stdout.write

exports.LlamaObject = LlamaObject
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
exports.str_or_array_to_list = str_or_array_to_list
exports.Show = Show
exports.show = show
exports.Trait = Trait
exports.Just = Just
exports.Nothing = Nothing
exports.TVar = TVar
exports.TConst = TConst
exports.TApply = TApply
exports.TFunction = TFunction
exports.TWildCard = TWildCard
exports.Default = Default
exports._default = _default
exports.End = End
exports.Cons = Cons
