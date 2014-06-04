function Exception(msg) {return new Error(msg);}
function $throw(obj) {throw obj;}
function println(obj) = console.log(obj.value);
var Type$Type = {name: 'Type'}
Type$Type.type = Type$Type;
var Type$Int = {name: 'Int', type: Type$Type}
var Type$Str = {name: 'Str', type: Type$Type}
function $int(i) {
  return {type: Type$Int, value: i}
}
function $str(s) {
  return {type: Type$Str, value: s}
}

var $func = function(name, obj, f) {
  obj = obj || {};
  obj.impls = obj.impls || {};
  obj._default = f || function () {
    $throw(Exception('No default implementation for function ' + name));
  }
  obj.has_instance = function(type) {
    if (!type) { return false; }
    return obj.impls[type.name] != null || obj.has_instance(type.parent);
  };
  obj.get_instance = function(type) {
    if (!type) { return obj._default; }
    return obj.impls[type.name] || obj.get_instance(type.parent);
  };
  obj.add_instance = function(type, func) {
    if (!type) {
      obj.impls._default = func;
    } else {
      obj.impls[type.name] = func;
    }
  };
  var func = function (_arg) {
    return obj.get_instance(_arg.type)(_arg);
  };
  obj.__proto__ = func.__proto__;
  func.__proto__ = obj;
  return func;
};
var foo = $func('foo');
foo.add_instance(Type$Int, function(x) {
  return 'the instance for int! ' + x;
});
foo.add_instance(Type$Str, function(s) {
  return 'the instance for str! ' + s;
  // return concat(reverse(s))(show(foo($int(13))));
});
println(foo($int(10)));
println(foo($str('hello world!')));
