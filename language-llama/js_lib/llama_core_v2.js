var $base_llama_type = {
  name: 'Type',
  constructors: [],
  attribs: {},
}

$base_llama_type.type = $base_llama_type;

var $LlamaType = (function() {
  function $LlamaType(name, constructors, parent, attribs) {
    this.name = name,
    this.constructors = constructors || [name],
    this.type = $base_llama_type,
    this.parent = parent,
    this.attribs = attribs || {}
  }
  return $LlamaType;
})();

var $LlamaObject = (function() {
  function $LlamaObject(type, constr_name, values, parent) {
    var attribs = {};
    // Fill in any default attributes stored on the type
    for (name in type.attribs) {
      attribs[name] = type.attribs[name];
    }
    this.type = type;
    this.constr_name = constr_name;
    this.values = values || [];
    this.parent = parent;
    this.attribs = attribs;
  }
  $LlamaObject.prototype.constr_is = function(name) {
    if (this.constr_name === name) {
      return true;
    } else if (this.parent != null) {
      return this.parent.constr_is(name);
    } else {
      return false;
    }
  }
  $LlamaObject.prototype.get = function(constr_name, name) {
    // If no name is provided, go with the default.
    if (name == null) {
      name = constr_name;
      constr_name = this.constr_name;
    }
    if (this.constr_name !== constr_name) {
      if (this.parent != null) {
        return this.parent.get(constr_name, name);
      } else {
        throw new Error('Constructor name mismatch: expected "' + this.constr_name + '" but got "' + constr_name + '"');
      }
    }
    var to_look_in = (typeof name === 'number') ? this.values : this.attribs;
    if (to_look_in[name] != null) {
      return to_look_in[name];
    } else {
      throw new Error("No field '" + name + "' on object");
    }
  }
  $LlamaObject.prototype.with = function(new_attribs) {
    var new_obj = new $LlamaObject(this.type, this.constr_name, this.values, this.parent, this.attribs);
    for (name in new_attribs) {
      new_obj.attribs[name] = new_attribs[name];
    }
    return new_obj;
  }
  return $LlamaObject;
})();

var Maybe$Type = new $LlamaType('Maybe', ['None', 'Some']);

var Some = function(a) {
  return new $LlamaObject(Maybe$Type, 'Some', [a]);
}

var None = new $LlamaObject(Maybe$Type, 'None', []);

var Int$Type = new $LlamaType('Int', ['$int']);

function $int(i) {
  return new $LlamaObject(Int$Type, '$int', [i]);
}

var Str$Type = new $LlamaType('Str', ['$str']);

function $str(s) {
  return new $LlamaObject(Str$Type, '$str', [s]);
}

function $is_llama_object(obj) {
  return obj.constr_name != null &&
         obj.type != null &&
         obj.values != null;
}

var MAX_TUPLE_SIZE = 50;
var TUPLE_TYPES = [];

// Creates a tuple type of the given size.
function $Tuple$Type(size) {
  if (size > MAX_TUPLE_SIZE) {
    throw new Error('Tuple is too large: max size of ' + MAX_TUPLE_SIZE + ' exceeded with request ' + size);
  }
  if (TUPLE_TYPES[size] != null) {
    return TUPLE_TYPES[size];
  }
  if (size > 0) {
    var parent = $Tuple$Type(size - 1);
    var type = new $LlamaType('Tuple('+size+')', [], parent);
    return TUPLE_TYPES[size] = type;
  }
  return TUPLE_TYPES[0] = new $LlamaType('Tuple(0)');
}

// Creates a tuple with the given values and attributes.
function $Tuple(vals, attribs) {
  vals = (vals == null) ? [] : vals;
  var type = $Tuple$Type(vals.length);
  var parent;
  if (vals.length > 0) {
    parent = $Tuple(vals.slice(0, -1), attribs);
  }
  return new $LlamaObject(type, '$Tuple', vals, parent, attribs);
}

var Func$Type = new $LlamaType('$Function');

var Exception$Type = new $LlamaType('Exception', ['Exception']);

// Exceptions have a special constructor because we use JavaScript Errors.
function Exception(msg) {
  var e = new Error(msg);
  var obj = new $LlamaObject(Exception$Type, 'Exception', [msg], {
    trace: e.stack
  });
  e.__proto__ = obj.__proto__;
  obj.__proto__ = e;
  return obj;
}

// Lets us put `throw` in expresions.
function $throw(e) {throw e};

// Named functions are objects, so that we can do things like
// multiple dispatch.
// `name` is the name of the function.
// `obj` is the (optional) LlamaObject being made into a function.
// `f` is the (optional) default function, which will be called if no
// type exactly matches one of the implementations.
function $Func(name, obj, f) {
  if (!name || typeof name !== 'string') {
    $throw(Exception('Unnamed functions cannot be $Funcs'))
  }
  if (obj != null && !$is_llama_object(obj)) {
    $throw(Exception('Non-llama object passed into $Func'));
  }
  if (obj == null) {
    obj = new $LlamaObject(Func$Type, 'Function('+name+')');
  }
  obj.instances = obj.instances || {};
  obj._default = obj._default || function () {
    $throw(Exception('No default implementation for function ' + name));
  }
  obj.has_instance = function(type) {
    if (!type) { return false; }
    return obj.instances[type.name] != null || obj.has_instance(type.parent);
  };
  obj.get_instance = function(type) {
    if (!type) {return obj._default;}
    return obj.instances[type.name] || obj.get_instance(type.parent);
  };
  obj.add_instance = function(type, func) {
    if (!type) {
      obj.instances._default = func;
    } else {
      obj.instances[type.name] = func;
    }
  };
  f = f || function (_arg) {
    _arg = (_arg == null) ? $Tuple() : _arg;
    return obj.get_instance(_arg.type)(_arg);
  };
  obj.__proto__ = f.__proto__;
  f.__proto__ = obj;
  return f;
}

exports.Maybe$Type = Maybe$Type
exports.Some = Some;
exports.None = None;
exports.$base_llama_type = $base_llama_type;
exports.$LlamaObject = $LlamaObject;
exports.$LlamaType = $LlamaType;
exports.Str$Type = Str$Type;
exports.Int$Type = Int$Type;
exports.$Tuple$Type = $Tuple$Type;
exports.$Tuple = $Tuple;
exports.Func$Type = Func$Type;
exports.Exception$Type = Exception$Type;
exports.Exception = Exception;
exports.$Func = $Func;
exports.$str = $str;
exports.$int = $int;

