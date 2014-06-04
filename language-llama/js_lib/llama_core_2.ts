class Llama {
  type: LlamaType;
  constr: string;
  values: Llama[] = [];
  parent: Llama;
  attrs: {[key: string]: any} = {};

  constructor(type: LlamaType,
              constr: string,
              values?: Llama[],
              parent ?: Llama,
              attrs?: {[key: string]:Llama},
              affect ?: (me: Llama) => void) {
    this.type = type;
    this.constr = constr;
    this.values = values || [];
    this.parent = parent;
    this.attrs = attrs || {};
    if (affect !== undefined) {
      affect(this);
    }
  }

  is_constr(constr: string): boolean {
    return this.constr === constr ||
           this.parent && this.parent.is_constr(constr);
  }

  deref(constr: string, idx: number): Llama {
    if (this.constr === constr) {
      if (idx < this.values.length) {
        return this.values[idx];
      } else {
        Exception('index out of range')._throw();
      }
    } else if (this.parent) {
      return this.parent.deref(constr, idx);
    } else {
      Exception('constructor not found')._throw();
    }
  }

  attr(constr: string, key: string): Llama {
    if (this.constr === constr) {
      if (this.attrs[key]) {
        return this.attrs[key];
      } else {
        Exception('No attribute ' + key)._throw();
      }
    } else if (this.parent) {
      return (this.parent.attr(constr, key));
    } else {
      Exception('Constructor ' + constr + ' not found')._throw();
    }
  }

  _throw(): any {
    throw this;
  }
}

var BaseLlamaType = new Llama(undefined, '(BaseType)');

class LlamaType {
  constr: string;
  values: LlamaType[];
  parent: LlamaType;
  name: string;

  constructor(constr: string,
              values: LlamaType[],
              name?: string,
              parent?: LlamaType) {
    this.constr = constr;
    this.values = values || [];
    this.parent = parent;
    this.name = name;
  }

  deref (constr: string, idx: number): LlamaType {
    if (this.constr === constr) {
      if (idx < this.values.length) {
        return this.values[idx];
      } else {
        Exception('Out of range for type deref')._throw();
      }
    } else {
      Exception('Wrong constructor')._throw();
    }
  }

  is_constr(constr: string) {
    return this.constr === constr ||
           this.parent && this.parent.is_constr(constr);
  }
}

interface Number {

}

function TVar(name) { return new LlamaType('TVar', [], name); }
function TConst (name) { return new LlamaType('TConst', [], name); }
function TFunction(from, to) { return new LlamaType('TFunction', [from, to]); }
function TApply(t1, t2) { return new LlamaType('TApply', [t1, t2]); }

function Exception(msg) {
  var type = TConst('Exception');
  return new Llama(type, 'Exception', [msg]);
}

function NoAttributeError(attr, obj) {
  var parent, type;
  parent = Exception("Attribute " + attr + " not found on object " + obj);
  type = TConst('NoAttributeError');
  return new Llama(type, 'NoAttributeError', [attr], parent);
}

function DerefRangeError(idx: number, obj: Llama) {
  var parent, type;
  parent = Exception("Index " + idx + " out of range on object " + obj);
  type = TConst('DerefRangeError');
  return new Llama(type, 'DerefRangeError', [idx, obj], parent);
}

function ConstructorNotFoundError(constr_name: string, obj: Llama) {
  var parent, type;
  parent = Exception("Constructor " + constr_name + " not found on object " + obj);
  type = TConst('ConstructorNotFoundError');
  return new Llama(type, 'ConstructorNotFoundError', [constr_name, obj], parent);
}

function LookupError(key: string) {
  var parent = Exception('Key ' + key + ' was not found');
  var type = TConst('LookupError');
  return new Llama(type, 'LookupError', [key], parent);
}

function PatternMatchError(pat) {
  return function(val) {
    var parent = Exception("Couldn't match value " + val + ' with pattern ' + pat);
    var type = TConst('PatternMatchError');
    return new Llama(type, 'PatternMatchError', [pat, val], parent);
  };
}

function _TypeError(msg: string) {
  var parent = Exception(msg);
  var type = TConst('TypeError');
  return new Llama(type, 'TypeError', [msg], parent, {});
}

class Trait {
  tvar: string;
  funcs: {[func_name: string]: LlamaType};
  instances: {[type_name: string]: any};
  constructor(tvar, funcs) {
    this.tvar = tvar; this.funcs = funcs;
  }
  make_sub(func_name: string, type: LlamaType) {}
}
