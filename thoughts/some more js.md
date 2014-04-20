function isTupleOf(val, size) {
  return _.isArray(val) && val.length === size;
}

Array.prototype.deref = function(idx) {
  return this[idx];
}

class Instance = {
  __constrName__: string,
  __values__: [Object],
  __parent__?: Instance,
  __attribs__: ...
}

var bar, baz;
var _val = foo(12)(3);
if (isTupleOf(_val, 2)) {
  bar = _val.deref(0);
  baz = _val.deref(1);
} else {
  throw new PatternMatchError();
}
