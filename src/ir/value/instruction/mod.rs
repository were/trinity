pub mod instructions;
pub mod const_folder;

pub use instructions::*;
use types::TypeRef;

use crate::context::component::GetSlabKey;
use crate::context::{SlabEntry, Reference, Context};
use crate::ir::value::ValueRef;
use crate::ir::types;

use super::Block;
use super::block::BlockRef;
use super::function::FuncMutator;

pub struct InstructionImpl {
  pub(crate) ty: types::TypeRef,
  pub(crate) opcode: InstOpcode,
  pub(crate) name_prefix: String,
  pub(crate) operands: Vec<ValueRef>,
  pub(crate) parent: Option<usize>,
  /// Instructions uses this instruction (inst, idx).
  /// It is inst's idx-th operand.
  pub(crate) users: Vec<(ValueRef, usize)>,
  /// Comment for this instruction.
  pub(crate) comment: String,
}

pub type Instruction = SlabEntry<InstructionImpl>;

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

  pub fn set_operand(&mut self, index: usize, operand: ValueRef) {
    let inst_value = Instruction::from_skey(self.skey);
    let inst = inst_value.as_mut::<Instruction>(self.ctx).unwrap();
    assert!(index < inst.instance.operands.len());
    let old = inst.set_operand(index, operand.clone());
    if old == operand {
      return;
    }
    self.ctx.add_user_redundancy(&inst_value, vec![(operand, index)]);
    self.ctx.remove_user_redundancy(old, inst_value, Some(index));
  }

  pub fn remove_operand(&mut self, index: usize) {
    let inst_value = Instruction::from_skey(self.skey);
    let inst = inst_value.as_mut::<Instruction>(self.ctx).unwrap();
    assert!(index < inst.instance.operands.len());
    let old = inst.instance.operands.remove(index);
    self.ctx.remove_user_redundancy(old, inst_value, Some(index));
  }

  pub fn set_opcode(&mut self, opcode: InstOpcode) {
    let inst_value = Instruction::from_skey(self.skey);
    let inst = inst_value.as_mut::<Instruction>(self.ctx).unwrap();
    inst.instance.opcode = opcode;
  }

  pub fn add_operand(&mut self, operand: ValueRef) {
    let inst_value = Instruction::from_skey(self.skey);
    let inst = inst_value.as_mut::<Instruction>(self.ctx).unwrap();
    let idx = inst.instance.operands.len();
    inst.add_operand(operand.clone());
    self.ctx.add_user_redundancy(&inst_value, vec![(operand, idx)]);
  }

  pub fn value(&self) -> ValueRef {
    ValueRef {
      skey: self.skey,
      kind: super::VKindCode::Instruction
    }
  }

  /// Remove this instruction!
  pub fn erase_from_parent(&mut self) {
    let (operands, block) = {
      let inst = self.value().as_ref::<Instruction>(&self.ctx).unwrap();
      let mut user_iter = inst.user_iter();
      assert!(user_iter.next().is_none());
      let operands = inst.operand_iter().map(|x| x.clone()).collect::<Vec<_>>();
      let block = inst.get_parent().as_super();
      (operands, block)
    };
    for operand in operands {
      self.ctx.remove_user_redundancy(operand, self.value(), None);
    }
    // Maintain the block redundancy.
    let block = block.as_mut::<Block>(&mut self.ctx).unwrap();
    block.instance.insts.retain(|x| *x != self.skey);
    self.ctx.dispose(self.skey);
  }

  /// Move to a new block's given index.
  pub fn move_to_block(&mut self, new_block: &ValueRef, idx: Option<usize>) {
    let inst = self.value();
    let inst_ref = inst.as_ref::<Instruction>(self.ctx).unwrap();
    let old_block = inst_ref.get_parent().as_super();
    // Remove from old parent.
    let old_block = old_block.as_mut::<Block>(self.ctx).unwrap();
    old_block.instance.insts.retain(|x| *x != inst.skey);
    // Add to new parent.
    let new_block = new_block.as_mut::<Block>(self.ctx).unwrap();
    let n = new_block.instance.insts.len();
    let idx = if idx.is_none() { n } else { idx.unwrap() };
    assert!(idx <= n);
    new_block.instance.insts.insert(idx, inst.skey);
    let new_parent = new_block.get_skey();
    let inst = self.value().as_mut::<Instruction>(self.ctx).unwrap();
    inst.instance.parent = Some(new_parent);
  }

  /// Replace old instruction with new value.
  pub fn replace_all_uses_with(&mut self, new: ValueRef) -> bool {
    let func = self
      .value()
      .as_ref::<Instruction>(self.ctx)
      .unwrap()
      .get_parent()
      .get_parent()
      .as_super();
    let old = self.value();
    let mut func = FuncMutator::new(self.ctx, func);
    func.replace_all_uses_with(old, new)
  }
  
}

// TODO(@were): Revisit this idea of code organization.
/// This is not only the opcode, but also the additional information of
/// these sub-instructions.
#[derive(Clone, PartialEq, Hash, Eq)]
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
  Branch(BranchMetadata),
  /// Phi node for SSA.
  Phi,
  /// Select instruction.
  Select,
}

pub enum BranchMetadata {
  LLVMLoop,
  ReturnJump,
  None
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
      InstOpcode::Select => "select".to_string(),
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
  AShr,
  And,
  Or,
  Xor,
  LogicalAnd,
  LogicalOr,
  Select,
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
      BinaryOp::AShr => "ashr",
      BinaryOp::And => "and",
      BinaryOp::Or => "or",
      BinaryOp::Xor => "xor",
      BinaryOp::LogicalAnd => "and",
      BinaryOp::LogicalOr => "or",
      BinaryOp::Select => "select",
    }
  }
}

/// Sub-opcodes of cast operation.
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum CastOp {
  Bitcast,
  FpToSi,
  SignExt,
  ZeroExt,
  Trunc,
}

impl CastOp {
  fn to_string(&self) -> &str {
    match &self {
      CastOp::Bitcast => "bitcast",
      CastOp::FpToSi => "fptosi",
      CastOp::SignExt => "sext",
      CastOp::ZeroExt => "zext",
      CastOp::Trunc => "trunc"
    }
  }
}

/// Sub instructions of comparison.
#[derive(Clone, PartialEq, Eq, Hash)]
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
  /// Int not equal.
  NE
}

impl CmpPred {

  pub fn to_string(&self) -> String {
    match self {
      CmpPred::SLT => "slt".to_string(),
      CmpPred::SGT => "sgt".to_string(),
      CmpPred::SLE => "sle".to_string(),
      CmpPred::SGE => "sge".to_string(),
      CmpPred::EQ => "eq".to_string(),
      CmpPred::NE => "ne".to_string(),
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

  pub(crate) fn add_user(&mut self, user: &ValueRef, idx: usize) {
    self.instance.users.push((user.clone(), idx));
  }

  pub(crate) fn remove_user(&mut self, user: &ValueRef, idx: Option<usize>) {
    if let Some(idx) = idx {
      let tuple = (user.clone(), idx);
      self.instance.users.retain(|x| *x != tuple);
    } else {
      self.instance.users.retain(|x| x.0 != *user);
    }
  }

  pub fn set_opcode(&mut self, opcode: InstOpcode) {
    self.instance.opcode = opcode;
  }

  pub(super) fn set_operand(&mut self, idx: usize, new_value: ValueRef) -> ValueRef {
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

  pub fn next_inst(&self) -> Option<InstructionRef<'ctx>> {
    let idx = self.get_parent().inst_iter().position(|x| x.get_skey() == self.get_skey()).unwrap();
    self
      .get_parent()
      .get_inst(idx + 1)
      .map(|x| x.as_super())
      .map(|x| x.as_ref::<Instruction>(self.ctx()).unwrap())
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
      InstOpcode::Select => { self.as_sub::<SelectInst>().unwrap().to_string() }
    };
    if comment {
      if self.instance().unwrap().comment.len() != 0 {
        res = format!("; {}\n  {}", self.instance().unwrap().comment, res)
      }
      for (user, _) in self.instance().unwrap().users.iter() {
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
    self.instance().unwrap().users.iter().map(|(u, _)| u.as_ref::<Instruction>(self.ctx).unwrap())
  }

  pub fn as_sub<T: SubInst<'ctx, T>>(&'ctx self) -> Option<T> {
    T::new(self)
  }

}

