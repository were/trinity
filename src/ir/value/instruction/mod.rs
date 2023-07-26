pub mod instructions;

pub use instructions::*;
use types::TypeRef;

use crate::context::{SlabEntry, Reference, Context};
use crate::ir::value::ValueRef;
use crate::ir::{types, Function};

use super::Block;
use super::block::BlockRef;

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

pub struct InstMutator<'ctx> {
  ctx: &'ctx mut Context,
  skey: usize
}

impl <'ctx> InstMutator <'ctx> {
  
  pub fn new(ctx: &'ctx mut Context, inst: &ValueRef) -> Self {
    match inst.kind {
      super::VKindCode::Instruction => Self { ctx, skey: inst.skey },
      _ => panic!("Cannot mutate a non-instruction value.")
    }
  }

  pub fn add_operand(&mut self, operand: ValueRef) {
    let inst_value = Instruction::from_skey(self.skey);
    let inst = inst_value.as_mut::<Instruction>(self.ctx).unwrap();
    inst.add_operand(operand.clone());
    self.ctx.add_user_redundancy(&inst_value, &vec![operand]);
  }

  pub fn value(&self) -> ValueRef {
    ValueRef {
      skey: self.skey,
      kind: super::VKindCode::Instruction
    }
  }

  /// Remove this instruction!
  pub fn erase_from_parent(&mut self) {
    let (operands, block, is_branch) = {
      let inst = self.value().as_ref::<Instruction>(&self.ctx).unwrap();
      let mut user_iter = inst.user_iter();
      assert!(user_iter.next().is_none());
      let operands = inst.operand_iter().map(|x| x.clone()).collect::<Vec<_>>();
      let block = inst.get_parent().as_super();
      let is_branch = if let InstOpcode::Branch(_) = inst.get_opcode() {
        true
      } else {
        false
      };
      (operands, block, is_branch)
    };
    for operand in operands {
      if let Some(operand_inst) = operand.as_mut::<Instruction>(self.ctx) {
        operand_inst.instance.users.retain(|x| x.skey != self.skey);
      }
      if let Some(operand_block) = operand.as_mut::<Block>(self.ctx) {
        operand_block.instance.users.retain(|x| *x != self.skey);
      }
      let value = self.value();
      if let Some(operand_func) = operand.as_mut::<Function>(self.ctx) {
        operand_func.remove_caller(value);
      }
    }
    // Maintain the block redundancy.
    let block = block.as_mut::<Block>(&mut self.ctx).unwrap();
    block.instance.insts.retain(|x| *x != self.skey);
    if is_branch {
      block.instance.succs.clear();
    }
    self.ctx.dispose(self.skey);
  }

  /// Replace old instruction with new value.
  pub fn replace_all_uses_with(&mut self, new: ValueRef) -> bool {
    let old = self.value();
    let old_inst = old.as_ref::<Instruction>(self.ctx).unwrap();
    let old_parent = old_inst.get_parent();
    let func = old_parent.get_parent();
    let mut to_replace = Vec::new();
    func.block_iter().for_each(|block| {
      for inst in block.inst_iter() {
        for i in 0..inst.get_num_operands() {
          if inst.get_operand(i).unwrap().skey == old.skey {
            to_replace.push((Instruction::from_skey(inst.get_skey()), i));
          }
        }
      }
    });
    to_replace.iter().for_each(|(before, idx)| {
      self.ctx.add_user_redundancy(before, &vec![new.clone()]);
      let inst = before.as_mut::<Instruction>(self.ctx).unwrap();
      inst.set_operand(*idx, new.clone());
    });
    // Maintain the redundant information.
    old.as_mut::<Instruction>(self.ctx)
      .unwrap()
      .instance
      .users
      .clear();
    return !to_replace.is_empty();
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
  Branch(Option<usize>),
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
      InstOpcode::Branch(_) => "br".to_string(),
      InstOpcode::BinaryOp(binop) => binop.to_string().to_string(),
      InstOpcode::CastInst(cast) => cast.to_string().to_string(),
      InstOpcode::Phi => "phi".to_string(),
    }
  }
}


/// Sub-opcodes for binary operations.
#[derive(Clone, PartialEq, Eq, Hash)]
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
  pub fn to_string(&self) -> &str {
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

  pub(crate) fn set_operand(&mut self, idx: usize, new_value: ValueRef) -> ValueRef {
    if idx >= self.instance.operands.len() {
      panic!("Index out of bound.");
    }
    let old = self.instance.operands[idx].clone();
    self.instance.operands[idx] = new_value;
    old
  }

  pub fn set_comment(&mut self, comment: String) {
    self.instance.comment = comment;
  }

  fn add_operand(&mut self, new_value: ValueRef) {
    self.instance.operands.push(new_value);
  }
}

impl Instruction {

  pub fn new(ty: TypeRef, opcode: InstOpcode, name_prefix: String, operands: Vec<ValueRef>) -> Self {
    let instance = InstructionImpl::new(ty, opcode, name_prefix, operands);
    Instruction::from(instance)
  }

}

pub type InstructionRef<'ctx> = Reference<'ctx, InstructionImpl>;

impl <'ctx>InstructionRef<'ctx> {

  pub fn get_opcode(&self) -> &InstOpcode {
    &self.instance().unwrap().opcode
  }

  pub fn get_name(&self) -> String {
    if let Some(skey) = self.is_invalid() {
      return format!("{{invalid.inst.{}}}", skey);
    }
    format!("{}.{}", self.instance().unwrap().name_prefix, self.get_skey())
  }

  pub fn get_type(&self) -> &types::TypeRef {
    &self.instance().unwrap().ty
  }

  pub fn get_num_operands(&self) -> usize {
    self.instance().unwrap().operands.len()
  }

  pub fn get_operand(&self, idx: usize) -> Option<&ValueRef> {
    if idx < self.instance().unwrap().operands.len() {
      Some(&self.instance().unwrap().operands[idx])
    } else {
      None
    }
  }

  pub fn get_parent(&self) -> BlockRef<'ctx> {
    let block = Block::from_skey(self.instance().unwrap().parent.unwrap());
    block.as_ref::<Block>(self.ctx).unwrap()
  }

  pub fn to_string(&self, comment: bool) -> String {
    if self.is_invalid().is_some() {
      return self.get_name();
    }
    let mut res = match self.instance().unwrap().opcode {
      InstOpcode::Alloca(_) => { self.as_sub::<Alloca>().unwrap().to_string() },
      InstOpcode::Return => { self.as_sub::<Return>().unwrap().to_string() },
      InstOpcode::GetElementPtr(_) => { self.as_sub::<GetElementPtr>().unwrap().to_string() },
      InstOpcode::Load(_) => { self.as_sub::<Load>().unwrap().to_string() },
      InstOpcode::Store(_) => { self.as_sub::<Store>().unwrap().to_string() },
      InstOpcode::Call => { self.as_sub::<Call>().unwrap().to_string() },
      InstOpcode::BinaryOp(_) => { self.as_sub::<BinaryInst>().unwrap().to_string() },
      InstOpcode::CastInst(_) => { self.as_sub::<CastInst>().unwrap().to_string() }
      InstOpcode::ICompare(_) => { self.as_sub::<CompareInst>().unwrap().to_string() }
      InstOpcode::Branch(_) => { self.as_sub::<BranchInst>().unwrap().to_string() }
      InstOpcode::Phi => { self.as_sub::<PhiNode>().unwrap().to_string() }
    };
    if comment {
      if self.instance().unwrap().comment.len() != 0 {
        res = format!("; {}\n  {}", self.instance().unwrap().comment, res)
      }
      for user in self.instance().unwrap().users.iter() {
        let user = user.as_ref::<Instruction>(self.ctx).unwrap();
        res = format!("; user: {}\n  {}", user.to_string(false), res);
      }
    }
    res
  }

  pub fn operand_iter(&'ctx self) -> impl Iterator<Item=&'ctx ValueRef> {
    self.instance().unwrap().operands.iter()
  }

  pub fn user_iter(&self) -> impl Iterator<Item=InstructionRef<'ctx>> {
    self.instance().unwrap().users.iter().map(|u| u.as_ref::<Instruction>(self.ctx).unwrap())
  }

  pub fn as_sub<T: SubInst<'ctx, T>>(&'ctx self) -> Option<T> {
    T::new(self)
  }

}

