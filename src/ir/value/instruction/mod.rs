pub mod instructions;

pub use instructions::*;

use crate::ir::value::ValueRef;
use crate::ir::types;

#[derive(Clone)]
pub struct Instruction {
  pub(crate) skey: Option<usize>,
  pub(crate) ty: types::TypeRef,
  pub(crate) opcode: InstOpcode,
  pub(crate) name: String,
  pub(crate) operands: Vec<ValueRef>,
  pub(crate) parent: ValueRef,
}

impl ToString for InstOpcode {
  fn to_string(&self) -> String {
    match self {
      InstOpcode::Alloca(_) => format!("alloca"),
      InstOpcode::Return => "ret".to_string(),
      InstOpcode::GetElementPtr(_) => format!("getelementptr"),
      InstOpcode::Load(_) => format!("load"),
      InstOpcode::Store(_) => format!("store"),
      InstOpcode::Call => format!("call"),
      InstOpcode::BinaryOp(binop) => {
        match binop {
          BinaryOp::Add => format!("add"),
          BinaryOp::Sub => format!("sub"),
          BinaryOp::Mul => format!("mul"),
          BinaryOp::Div => format!("div"),
          BinaryOp::Rem => format!("rem"),
          BinaryOp::Shl => format!("shl"),
          BinaryOp::Shr => format!("shr"),
          BinaryOp::And => format!("and"),
          BinaryOp::Or => format!("or"),
          BinaryOp::Xor => format!("xor"),
          BinaryOp::LogicalAnd => format!("and"),
          BinaryOp::LogicalOr => format!("or"),
        }
      }
    }
  }
}


/// Sub-opcodes for binary operations.
#[derive(Clone, PartialEq)]
pub enum BinaryOp {
  Add,
  Sub,
  Mul,
  Div,
  Rem,
  Shl,
  Shr,
  And,
  Or,
  Xor,
  LogicalAnd,
  LogicalOr,
}


// TODO(@were): Revisit this idea of code organization.
/// This is not only the opcode, but also the additional information of
/// these sub-instructions.
#[derive(Clone, PartialEq)]
pub enum InstOpcode {
  /// Memory allocation (alignment).
  Alloca(usize),
  /// Load instruction (alignment).
  Load(usize),
  /// Store instruction (alignment).
  Store(usize),
  /// Return instruction.
  Return,
  /// GetElementPtr instruction (inbound).
  GetElementPtr(bool),
  /// Call a callable value.
  Call,
  /// Binary operation.
  BinaryOp(BinaryOp),
}

impl Instruction {

  pub fn get_name(&self) -> &str {
    &self.name
  }

  pub fn get_type(&self) -> &types::TypeRef {
    &self.ty
  }

  pub fn get_num_operands(&self) -> usize {
    self.operands.len()
  }

  pub fn get_operand(&self, idx: usize) -> ValueRef {
    self.operands[idx].clone()
  }

  pub fn set_operand(&mut self, idx: usize, new_value: ValueRef) {
    self.operands[idx] = new_value;
  }

  pub fn to_string(&self, ctx: &crate::context::Context) -> String {
    match self.opcode {
      InstOpcode::Alloca(align) => { Alloca::new(self, align).to_string(ctx) },
      InstOpcode::Return => { Return::new(self).to_string(ctx) },
      InstOpcode::GetElementPtr(inbounds) => { GetElementPtr::new(self, inbounds).to_string(ctx) },
      InstOpcode::Load(align) => { Load::new(self, align).to_string(ctx) },
      InstOpcode::Store(align) => { Store::new(self, align).to_string(ctx) },
      InstOpcode::Call => { Call::new(self).to_string(ctx) },
      InstOpcode::BinaryOp(_) => { BinaryInst::new(self).to_string(ctx) },
    }
  }

}

