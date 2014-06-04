interface Llama {
  type: Llama;
  constr_name: string;
  values: Llama[];
  parent: Llama;
}

class LlamaType implements Llama {
  type: LlamaType;
  constr_name: string;
  parent: LlamaType;
  values: LlamaType[];
  constructor(type, constr_name, values, parent?) {
    this.type = type;
    this.constr_name = constr_name;
    this.values = values;
    this.parent = parent;
  }
}

function _deref(llama: Llama,
                constr: string,
                idx: number): Llama {
  return undefined;
}

class LlamaObject implements Llama {
  values: Llama[];
  type: LlamaType;
  constr_name: string;
  parent: LlamaObject;
  attrs: {[key: string]: Llama}
  constructor(type: LlamaType,
              constr_name: string,
              values: Llama[],
              attrs: {[key: string]:Llama},
              parent ?: LlamaObject,
              affect ?: (_: Llama) => void) {
    this.type = type;
    this.values = values;
    this.constr_name = constr_name;
    this.attrs = attrs;
    this.parent = parent;
    if (affect !== undefined) {
      affect(this);
    }
  }
}
