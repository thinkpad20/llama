use std::collections::HashMap;
use std::num::pow;

type NumArgs = uint;
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

enum Location {
  Line(Line),
  Label(Label)
}

enum Instruction {
  Push(Value),
  Pop(Register),
  Goto(Location),
  ElseGoto(Location),
  Call(Location, NumArgs),
  Return,
  Add, Sub, Mult, Div, Mod, Exp, Neg, And, Or, Not,
  Lt, Gt, Eq, Leq, Geq, Neq
}

struct VirtualMachine {
  instruction_list: Vec<Instruction>,
  initial_list: Vec<Instruction>,
  stack: Vec<Value>,
  registers: [Value, ..NUM_REGISTERS],
  labels: HashMap<Label, Line>,
  label_list: Vec<Label>,
  return_trace: Vec<Line>,
  pc: Line,
  instcount: u32,
}

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
      Push(p) => self.stack.push(p),
      Pop(reg) => self.registers[reg] = self.stack.pop().unwrap(),
      Return => self.pc = self.return_trace.pop().unwrap(),
      Call(loc, num_args) => {
        self.return_trace.push(self.pc + 1);
        for i in range(0, num_args) {
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
        None => fail!("invalid label"),
        Some(line) => self.pc = *line
      },
      Line(line) => self.pc = line
    }
  }

  fn pop(&mut self) -> Value {
    self.stack.pop().unwrap()
  }

  // Applies a unary operator to the top of the stack.
  fn op1(&mut self, op: fn(Value)->Value) {
    let top = self.stack.pop().unwrap();
    self.stack.push(op(top));
  }

  // Applies a binary operator to the two top values of the stack.
  fn op2(&mut self, op: fn(Value, Value)->Value) {
    let a = self.stack.pop().unwrap();
    let b = self.stack.pop().unwrap();
    self.stack.push(op(b, a));
  }
}

fn main() {
  println!("hello\n");
}
