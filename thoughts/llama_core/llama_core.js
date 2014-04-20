// Generated by CoffeeScript 1.7.1
(function() {
  var Exception, Just, LlamaObject, LookupError, Nothing, PatternMatchError, get_trace, is_instance, println, show, _, _append, _divide, _minus, _plus, _throw, _times;

  _ = require('underscore');

  LlamaObject = (function() {
    function LlamaObject(___type_name, ___constr_name, ___values, ___attrs, parent, affect) {
      var key, val, _i, _len, _ref, _ref1;
      this.___type_name = ___type_name;
      this.___constr_name = ___constr_name;
      this.___values = ___values != null ? ___values : [];
      this.___attrs = ___attrs != null ? ___attrs : {};
      this.parent = parent;
      if (affect != null) {
        affect(this);
      }
      _ref = _.pairs(___attrs);
      for (_i = 0, _len = _ref.length; _i < _len; _i++) {
        _ref1 = _ref[_i], key = _ref1[0], val = _ref1[1];
        Object.defineProperty(this, key, {
          get: function() {
            return this.get(___constr_name, key);
          }
        });
      }
    }

    LlamaObject.prototype.deref = function(constr_name, idx) {
      if (this.___constr_name === constr_name) {
        if (idx < this.___values.length) {
          return this.___values[idx];
        } else {
          return _throw(DerefRangeError(idx));
        }
      } else if (this.___parent != null) {
        return this.___parent.deref(constr_name, idx);
      } else {
        return _throw(ConstrNotFoundError(constr_name));
      }
    };

    LlamaObject.prototype.get = function(constr_name, key) {
      if (this.___constr_name === constr_name) {
        if (this.___attrs[key] != null) {
          return this.___attrs[key];
        } else {
          return _throw(NoAttributeError(key));
        }
      } else if (this.___parent != null) {
        return this.___parent.get(constr_name, idx);
      } else {
        return _throw(ConstrNotFoundError(constr_name));
      }
    };

    return LlamaObject;

  })();

  is_instance = function(obj, name) {
    if (obj.___type_name === name) {
      return true;
    }
    return (obj.___parent != null) && is_instance(obj.___parent, name);
  };

  _throw = function(exc) {
    if (!is_instance(exc, 'Exception')) {
      throw new Error('Attempt to throw a non-exception');
    }
    throw exc;
  };

  Nothing = new LlamaObject('Maybe', 'Nothing');

  Just = function(_a) {
    return new LlamaObject('Maybe', 'Just', [_a]);
  };

  get_trace = function() {
    return new Error().stack.split('\n');
  };

  Exception = function(msg) {
    return new LlamaObject('Exception', 'Exception', [msg], {}, null, function(self) {
      return self.___attrs.trace = get_trace();
    });
  };

  LookupError = function(key) {
    var parent;
    parent = Exception(_append(_append('Key ')(show(key)))(' was not found'));
    return new LlamaObject('LookupError', 'LookupError', [key], {}, parent);
  };

  PatternMatchError = function(pat) {
    return function(val) {
      var parent, a, b;
      a = _append("Couldn't match value ")(show(val));
      a = _append(a)(' with pattern ');
      a = _append(a)(show(pat));
      parent = Exception(a);
      return new LlamaObject('PatternMatchError', 'PatternMatchError', [pat, val], {}, parent);
    };
  };

  _plus = function(x) {
    return function(y) {
      return x + y;
    };
  };

  _minus = function(x) {
    return function(y) {
      return x - y;
    };
  };

  _times = function(x) {
    return function(y) {
      return x * y;
    };
  };

  _divide = function(x) {
    return function(y) {
      return x / y;
    };
  };

  _append = _plus;

  show = function(x) {
    return x;
  };

  println = console.log;

  exports.LlamaObject = LlamaObject;

  exports.LookupError = LookupError;

  exports.PatternMatchError = PatternMatchError;

  exports.is_instance = is_instance;

  exports.Exception = Exception;

  exports.Nothing = Nothing;

  exports.Just = Just;

}).call(this);
