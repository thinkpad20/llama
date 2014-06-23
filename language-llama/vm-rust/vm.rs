use std::collections::HashMap;
use std::num::pow;
use std::iter::range_step;
use std::vec;
use std::fmt;


type NumArgs = uint;
type Offset = uint;
type Line = uint;
type Register = uint;
type Label = String;

static NUM_REGISTERS: Register = 10;

#[deriving(Show, Clone, Copy)]
enum Value {
  Int(i64),
  Float(f64),
  Char(char),
  String(String),
  Empty,
  Bool(bool)
}

#[deriving(Show, Clone, Copy)]
enum Location {
  Line(Line),
  Label(Label)
}

#[deriving(Show, Clone, Copy)]
enum Instruction {
  Push(Value),
  PushArg(Offset),
  Pop(Register),
  Goto(Location),
  ElseGoto(Location),
  Call(Location, NumArgs),
  DeclareLabel(Label),
  Return,
  Add, Sub, Mult, Div, Mod, Exp, Neg, And, Or, Not,
  Lt, Gt, Eq, Leq, Geq, Neq
}

struct VirtualMachine {
  instruction_list: Vec<Instruction>,
  stack: Vec<Value>,
  registers: [Value, ..NUM_REGISTERS],
  labels: HashMap<Label, Line>,
  return_trace: Vec<Line>,
  pc: Line,
}


impl fmt::Show for VirtualMachine {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut result = String::new();
    for instr in self.instruction_list.iter() {
      result.append(instr.show())
    }
    // write!(f, format!("{}", self.instruction_list));
    write!(f, "Hi")
  }
}

// impl Show for VirtualMachine {
//   fn show(&self) -> "sldkfj"
// }

fn add_val(p1: Value, p2: Value) -> Value {
  match (p1, p2) {
    (Int(i), Int(j)) => Int(i + j),
    (Float(i), Int(j)) => Float(i + j as f64),
    (Int(i), Float(j)) => Float(i as f64 + j),
    (Float(i), Float(j)) => Float(i + j),
    _ => fail!(format!("Can't add"))
  }
}

fn sub_val(p1: Value, p2: Value) -> Value {
  match (p1, p2) {
    (Int(i), Int(j)) => Int(i - j),
    (Float(i), Int(j)) => Float(i - j as f64),
    (Int(i), Float(j)) => Float(i as f64 - j),
    (Float(i), Float(j)) => Float(i - j),
    _ => fail!(format!("Can't sub"))
  }
}

fn mult_val(p1: Value, p2: Value) -> Value {
  match (p1, p2) {
    (Int(i), Int(j)) => Int(i * j),
    (Float(i), Int(j)) => Float(i * j as f64),
    (Int(i), Float(j)) => Float(i as f64 * j),
    (Float(i), Float(j)) => Float(i * j),
    _ => fail!(format!("Can't mult"))
  }
}

fn div_val(p1: Value, p2: Value) -> Value {
  match (p1, p2) {
    (Int(i), Int(j)) => Int(i / j),
    (Float(i), Int(j)) => Float(i / j as f64),
    (Int(i), Float(j)) => Float(i as f64 / j),
    (Float(i), Float(j)) => Float(i / j),
    _ => fail!(format!("Can't div"))
  }
}


fn mod_val(p1: Value, p2: Value) -> Value {
  match (p1, p2) {
    (Int(i), Int(j)) => Int(i % j),
    (Float(i), Int(j)) => Float(i % j as f64),
    (Int(i), Float(j)) => Float(i as f64 % j),
    (Float(i), Float(j)) => Float(i % j),
    _ => fail!(format!("Can't mod"))
  }
}

fn exp_val(p1: Value, p2: Value) -> Value {
  match (p1, p2) {
    (Int(i), Int(j)) if j > 0 => Int(pow(i, j as uint)),
    (Float(i), Int(j)) => Float(i % j as f64),
    (Int(i), Float(j)) => Float(i as f64 % j),
    (Float(i), Float(j)) => Float(i % j),
    _ => fail!(format!("Can't mod"))
  }
}

fn lt_val(p1: Value, p2: Value) -> Value {
  match (p1, p2) {
    (Int(i), Int(j)) => Bool(i < j),
    (Float(i), Int(j)) => Bool(i < j as f64),
    (Int(i), Float(j)) => Bool((i as f64) < j),
    (Float(i), Float(j)) => Bool(i < j),
    _ => fail!(format!("Can't lt"))
  }
}


fn gt_val(p1: Value, p2: Value) -> Value {
  match (p1, p2) {
    (Int(i), Int(j)) => Bool(i > j),
    (Float(i), Int(j)) => Bool(i > j as f64),
    (Int(i), Float(j)) => Bool((i as f64) > j),
    (Float(i), Float(j)) => Bool(i > j),
    _ => fail!(format!("Can't gt"))
  }
}


fn leq_val(p1: Value, p2: Value) -> Value {
  match (p1, p2) {
    (Int(i), Int(j)) => Bool(i <= j),
    (Float(i), Int(j)) => Bool(i <= j as f64),
    (Int(i), Float(j)) => Bool((i as f64) <= j),
    (Float(i), Float(j)) => Bool(i <= j),
    _ => fail!(format!("Can't leq"))
  }
}


fn geq_val(p1: Value, p2: Value) -> Value {
  match (p1, p2) {
    (Int(i), Int(j)) => Bool(i >= j),
    (Float(i), Int(j)) => Bool(i >= j as f64),
    (Int(i), Float(j)) => Bool((i as f64) >= j),
    (Float(i), Float(j)) => Bool(i >= j),
    _ => fail!(format!("Can't geq"))
  }
}


fn eq_val(p1: Value, p2: Value) -> Value {
  match (p1, p2) {
    (Int(i), Int(j)) => Bool(i == j),
    (Float(i), Int(j)) => Bool(i == j as f64),
    (Int(i), Float(j)) => Bool((i as f64) == j),
    (Float(i), Float(j)) => Bool(i == j),
    _ => fail!(format!("Can't eq"))
  }
}


fn neq_val(p1: Value, p2: Value) -> Value {
  match (p1, p2) {
    (Int(i), Int(j)) => Bool(i != j),
    (Float(i), Int(j)) => Bool(i != j as f64),
    (Int(i), Float(j)) => Bool((i as f64) != j),
    (Float(i), Float(j)) => Bool(i != j),
    _ => fail!(format!("Can't neq"))
  }
}


fn and_val(p1: Value, p2: Value) -> Value {
  match (p1, p2) {
    (Bool(i), Bool(j)) => Bool(i && j),
    _ => fail!(format!("Can't and"))
  }
}


fn or_val(p1: Value, p2: Value) -> Value {
  match (p1, p2) {
    (Bool(i), Bool(j)) => Bool(i || j),
    _ => fail!(format!("Can't or"))
  }
}

fn not_val(p1: Value) -> Value {
  match p1 {
    Bool(i) => Bool(!i),
    _ => fail!(format!("Can't logical not"))
  }
}

fn neg_val(p1: Value) -> Value {
  match p1 {
    Int(i) => Int(-i),
    Float(i) => Float(-i),
    _ => fail!(format!("Can't negate"))
  }
}

impl VirtualMachine {
  // Executes a single instruction.
  fn execute(&mut self, inst: Instruction) {
    match (inst) {
      Push(val) => self.stack.push(val),
      PushArg(offset) => self.stack.push(self.registers[offset].clone()),
      Pop(reg) => self.registers[reg] = self.stack.pop().unwrap(),
      Return => self.pc = self.return_trace.pop().unwrap(),
      Call(loc, num_args) => {
        self.return_trace.push(self.pc + 1);
        for i in range_step(num_args - 1, 0, -1) {
          self.registers[i] = self.pop()
        }
        self.goto(loc);
      }
      Goto(loc) => self.goto(loc),
      ElseGoto(loc) => {
        let tos = self.pop();
        match tos {
          Bool(b) => if !b {self.goto(loc)},
          _ => fail!("top of stack is not a bool.")
        }
      }
      // Built-in operators
      Add => self.op2(add_val), Sub => self.op2(sub_val),
      Mult => self.op2(mult_val), Div => self.op2(div_val),
      Mod => self.op2(mod_val), Exp => self.op2(exp_val),
      Lt => self.op2(lt_val), Gt => self.op2(gt_val),
      Leq => self.op2(leq_val), Geq => self.op2(geq_val),
      Eq => self.op2(eq_val), Neq => self.op2(neq_val),
      Not => self.op1(not_val), Neg => self.op1(neg_val),
      _ => {}
    }
  }

  fn goto(&mut self, loc: Location) {
    match loc {
      Label(label) => match self.labels.find(&label) {
        None => fail!(format!("invalid label {}", label)),
        Some(line) => self.pc = *line
      },
      Line(line) => self.pc = line
    }
  }

  fn pop(&mut self) -> Value {
    match self.stack.pop() {
      Some(v) => v,
      None => fail!("Empty stack")
    }
  }

  // Applies a unary operator to the top of the stack. Increments PC.
  fn op1(&mut self, op: fn(Value)->Value) {
    let top = self.stack.pop().unwrap();
    self.stack.push(op(top));
    self.pc += 1;
  }

  // Applies a binary operator to the two top values of the stack.
  // Increments program counter.
  fn op2(&mut self, op: fn(Value, Value)->Value) {
    let a = self.stack.pop().unwrap();
    let b = self.stack.pop().unwrap();
    self.stack.push(op(b, a));
    self.pc += 1;
  }

  fn run(&mut self, start: Location) {
    self.goto(start);
    let mut max_stack : uint = 0;
    let mut inst_count : uint = 0;
    while self.pc < self.instruction_list.len() {
      inst_count += 1;
      if inst_count > 200000 {
        fail!("Infinite loop")
      }
      let next_instr = self.instruction_list.get(self.pc).clone();
      self.execute(next_instr);
      if self.stack.len() > max_stack {max_stack = self.stack.len()}
    }
    if self.stack.len() > 0 {
      println!("Top of stack is {}", self.stack.get(self.stack.len()))
    } else {
      fail!("Empty stack after finish")
    }
  }
}

// Runs through the list of instructions, removes any labels, and records
// the line number the labels correspond to.
// For example, given the list
// [label foo, push 1, push 2, add, label bar, call foo]
// this would return
// [push 1, push 2, add, call 0]
fn prepare_labels(input_instrs: Vec<Instruction>)
  -> (Vec<Instruction>, HashMap<Label, Line>) {
    let mut line_no : uint = 0;
    let mut output_instrs = Vec::new();
    let mut label_map = HashMap::new();
    for inst in input_instrs.iter() {
      match (*inst).clone() {
        DeclareLabel(label) => {
          label_map.insert(label, line_no);
        },
        inst => {line_no += 1; output_instrs.push(inst)}
      }
    }
    (output_instrs, label_map)
  }

fn dec_label(name: &str) -> Instruction {
  DeclareLabel(String::from_str(name))
}

fn mk_label(name: &str) -> Location {
  Label(String::from_str(name))
}

fn main() {
  let mut instrs : Vec<Instruction> = Vec::new();
  instrs.push(dec_label("fact.f"));
  instrs.push(PushArg(0));
  instrs.push(Push(Int(2)));
  instrs.push(Lt);
  instrs.push(ElseGoto(mk_label("fact.f$1")));
  instrs.push(Push(Int(1)));
  instrs.push(Return);
  instrs.push(dec_label("fact.f$1"));
  instrs.push(PushArg(0));
  instrs.push(Push(Int(1)));
  instrs.push(Sub);
  instrs.push(PushArg(1));
  instrs.push(PushArg(0));
  instrs.push(Mult);
  instrs.push(Pop(1));
  instrs.push(Pop(0));
  instrs.push(Goto(mk_label("fact.f")));
  instrs.push(dec_label("fact"));
  instrs.push(PushArg(0));
  instrs.push(Push(Int(1)));
  instrs.push(Call(mk_label("fact.f"), 2));
  for i in instrs.iter() {
    println!("{}", i);
  }
  let (new_instrs, label_map) = prepare_labels(instrs);
  // let vm = VirtualMachine {
  //   instruction_list: new_instrs,
  //   stack: Vec::new(),
  //   registers: [Empty, Empty, Empty, Empty, Empty,
  //               Empty, Empty, Empty, Empty, Empty],
  //   labels: label_map,
  //   return_trace: Vec::new(),
  //   pc: 0
  // };
  // println!("{}", vm);
}
