pub mod instructions;

pub use instructions::*;
use types::TypeRef;

use crate::context::{SlabEntry, Reference};
use crate::ir::value::ValueRef;
use crate::ir::types;

use super::Block;

pub struct InstructionImpl {
  pub(crate) ty: types::TypeRef,
  pub(crate) opcode: InstOpcode,
  pub(crate) name_prefix: String,
  pub(crate) operands: Vec<ValueRef>,
  pub(crate) parent: Option<usize>,
  /// Instructions uses this instruction.
  pub(crate) users: Vec<ValueRef>,
  /// Comment for this instruction.
  pub(crate) comment: String,
}

pub type Instruction = SlabEntry<InstructionImpl>;

impl Instruction {

  pub(crate) fn add_user(&mut self, user: ValueRef) -> bool {
    let pos = self.instance.users.iter().position(|x| x.skey == user.skey);
    if pos.is_none() {
      self.instance.users.push(user);
      true
    } else {
      false
    }
  }

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
  /// Bitcast or reinterpret cast.
  CastInst(CastOp),
  /// Int value comparison.
  ICompare(CmpPred),
  /// Branch instruction.
  Branch,
  /// Phi node for SSA.
  Phi,
}

impl ToString for InstOpcode {
  fn to_string(&self) -> String {
    match self {
      InstOpcode::Alloca(_) => "alloca".to_string(),
      InstOpcode::Return => "ret".to_string(),
      InstOpcode::GetElementPtr(_) => "getelementptr".to_string(),
      InstOpcode::Load(_) => "load".to_string(),
      InstOpcode::Store(_) => "store".to_string(),
      InstOpcode::Call => "call".to_string(),
      InstOpcode::ICompare(_) => "icmp".to_string(),
      InstOpcode::Branch => "br".to_string(),
      InstOpcode::BinaryOp(binop) => binop.to_string().to_string(),
      InstOpcode::CastInst(cast) => cast.to_string().to_string(),
      InstOpcode::Phi => "phi".to_string(),
    }
  }
}


/// Sub-opcodes for binary operations.
#[derive(Clone, PartialEq)]
pub enum BinaryOp {
  Add,
  Sub,
  Mul,
  SDiv,
  SRem,
  Rem,
  Shl,
  Shr,
  And,
  Or,
  Xor,
  LogicalAnd,
  LogicalOr,
}

impl BinaryOp {
  fn to_string(&self) -> &str {
    match &self {
      BinaryOp::Add => "add",
      BinaryOp::Sub => "sub",
      BinaryOp::Mul => "mul",
      BinaryOp::SDiv => "sdiv",
      BinaryOp::SRem => "srem",
      BinaryOp::Rem => "rem",
      BinaryOp::Shl => "shl",
      BinaryOp::Shr => "shr",
      BinaryOp::And => "and",
      BinaryOp::Or => "or",
      BinaryOp::Xor => "xor",
      BinaryOp::LogicalAnd => "and",
      BinaryOp::LogicalOr => "or",
    }
  }
}

/// Sub-opcodes of cast operation.
#[derive(Clone, PartialEq)]
pub enum CastOp {
  Bitcast,
  FpToSi,
  SignExt,
  Trunc,
}

impl CastOp {
  fn to_string(&self) -> &str {
    match &self {
      CastOp::Bitcast => "bitcast",
      CastOp::FpToSi => "fptosi",
      CastOp::SignExt => "sext",
      CastOp::Trunc => "trunc"
    }
  }
}

/// Sub instructions of comparison.
#[derive(Clone, PartialEq)]
pub enum CmpPred {
  /// Signed less than.
  SLT,
  /// Signed greater than.
  SGT,
  /// Signed less equal.
  SLE,
  /// Signed greater equal.
  SGE,
  /// Int equal.
  EQ,
}

impl CmpPred {

  pub fn to_string(&self) -> String {
    match self {
      CmpPred::SLT => "slt".to_string(),
      CmpPred::SGT => "sgt".to_string(),
      CmpPred::SLE => "sle".to_string(),
      CmpPred::SGE => "sge".to_string(),
      CmpPred::EQ => "eq".to_string(),
    }
  }

}

impl InstructionImpl {
  pub fn new(ty: TypeRef, opcode: InstOpcode, name_prefix: String, operands: Vec<ValueRef>) -> Self {
    InstructionImpl {
      ty,
      opcode,
      name_prefix,
      operands,
      parent: None,
      comment: "".to_string(),
      users: vec![],
    }
  }

}

impl Instruction {

  pub fn set_operand(&mut self, idx: usize, new_value: ValueRef) {
    self.instance.operands[idx] = new_value;
  }

  pub fn set_comment(&mut self, comment: String) {
    self.instance.comment = comment;
  }

  pub fn add_operand(&mut self, new_value: ValueRef) {
    self.instance.operands.push(new_value);
  }
}

impl Instruction {

  pub fn new(ty: TypeRef, opcode: InstOpcode, name_prefix: String, operands: Vec<ValueRef>) -> Self {
    let instance = InstructionImpl::new(ty, opcode, name_prefix, operands);
    Instruction::from(instance)
  }

}

pub type InstructionRef<'ctx> = Reference<'ctx, Instruction>;

impl <'ctx>InstructionRef<'ctx> {

  pub fn get_opcode(&self) -> &InstOpcode {
    &self.instance.instance.opcode
  }

  pub fn get_name(&self) -> String {
    format!("{}.{}", self.instance.instance.name_prefix, self.skey)
  }

  pub fn get_type(&self) -> &types::TypeRef {
    &self.instance.instance.ty
  }

  pub fn get_num_operands(&self) -> usize {
    self.instance.instance.operands.len()
  }

  pub fn get_operand(&self, idx: usize) -> Option<&ValueRef> {
    if idx < self.instance.instance.operands.len() {
      Some(&self.instance.instance.operands[idx])
    } else {
      None
    }
  }

  pub fn get_parent(&self) -> &'ctx Block {
    let block = Block::from_skey(self.instance.instance.parent.unwrap());
    block.as_ref::<Block>(self.ctx).unwrap()
  }

  pub fn to_string(&self) -> String {
    let mut res = match self.instance.instance.opcode {
      InstOpcode::Alloca(_) => { Alloca::new(self).to_string(self.ctx) },
      InstOpcode::Return => { Return::new(self).to_string(self.ctx) },
      InstOpcode::GetElementPtr(_) => { GetElementPtr::new(self).to_string(self.ctx) },
      InstOpcode::Load(_) => { Load::new(self).to_string(self.ctx) },
      InstOpcode::Store(_) => { Store::new(self).to_string(self.ctx) },
      InstOpcode::Call => { Call::new(self).to_string(self.ctx) },
      InstOpcode::BinaryOp(_) => { BinaryInst::new(self).to_string(self.ctx) },
      InstOpcode::CastInst(_) => { CastInst::new(self).to_string(self.ctx) }
      InstOpcode::ICompare(_) => { CompareInst::new(self).to_string(self.ctx) }
      InstOpcode::Branch => { BranchInst::new(self).to_string(self.ctx) }
      InstOpcode::Phi => { PhiNode::new(self).to_string(self.ctx) }
    };
    if self.instance.instance.comment.len() != 0 {
      res = format!("; {}\n  {}", self.instance.instance.comment, res)
    }
    // TODO(@were): Fix the redundancy removal.
    // for user in self.instance.instance.users.iter() {
    //   res = format!("; user: {}\n  {}", user.to_string(self.ctx, true), res);
    // }
    res
  }

  pub fn operand_iter(&self) -> ValueRefIter {
    ValueRefIter {
      idx: 0,
      vec: &self.instance.instance.operands,
    }
  }

  pub fn user_iter(&self) -> ValueRefIter {
    ValueRefIter {
      idx: 0,
      vec: &self.instance.instance.users,
    }
  }

  pub fn as_sub<'inst, T: SubInst<'inst>>(&'inst self) -> T {
    T::new(self)
  }

}

pub struct ValueRefIter<'inst> {
  idx: usize,
  vec: &'inst Vec<ValueRef>,
}

impl Iterator for ValueRefIter<'_> {
  type Item = ValueRef;

  fn next(&mut self) -> Option<Self::Item> {
    if self.idx < self.vec.len() {
      let res = &self.vec[self.idx];
      self.idx += 1;
      Some(res.clone())
    } else {
      None
    }
  }

}

