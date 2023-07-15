use crate::{context::{SlabEntry, Reference}, ir::value::instruction::InstructionRef};

use super::{ValueRef, instruction::{Instruction, InstOpcode}, Function, function::FunctionRef, VKindCode};

pub struct BlockImpl {
  /// The name prefix of this block.
  pub(crate) name_prefix: String,
  /// The slab keys of the instructions in this block.
  pub(crate) insts: Vec<usize>,
  /// The slab key of the parent function.
  pub(crate) parent: usize,
  /// The slab keys of the branch instructions target this block.
  pub(crate) users: Vec<usize>,
}

pub type Block = SlabEntry<BlockImpl>;
pub type BlockRef<'ctx> = Reference<'ctx, BlockImpl>;

impl BlockImpl {

  fn new(name_prefix: String, parent: usize) -> Self {
    Self {
      name_prefix,
      insts: Vec::new(),
      parent,
      users: Vec::new(),
    }
  }

}

impl Block {

  pub fn new(name_prefix: String, parent: &ValueRef) -> Self {
    Block::from(BlockImpl::new(name_prefix, parent.skey))
  }

  pub(crate) fn add_user(&mut self, inst: &ValueRef) {
    self.instance.users.push(inst.skey);
  }


}

impl <'ctx> BlockRef<'ctx> {

  pub fn get_parent(&self) -> FunctionRef<'ctx> {
    let func = Function::from_skey(self.instance().unwrap().parent);
    func.as_ref::<Function>(self.ctx).unwrap()
  }

  pub fn get_num_insts(&self) -> usize {
    return self.instance().unwrap().insts.len();
  }

  pub fn get_name(&self) -> String {
    if let Some(skey) = self.is_invalid() {
      return format!("{{invalid.block.{}}}", skey);
    }
    format!("{}.{}", self.instance().unwrap().name_prefix, self.get_skey())
  }

  /// If this block is closed, i.e. ends with a branch.
  pub fn closed(&self) -> bool {
    let ctx = self.ctx;
    if let Some(inst_ref) = self.instance().unwrap().insts.last() {
      let inst = ValueRef{ skey: *inst_ref, kind: crate::ir::VKindCode::Instruction };
      let inst = inst.as_ref::<Instruction>(ctx).unwrap();
      match inst.get_opcode() {
        InstOpcode::Branch | InstOpcode::Return => true,
        _ => false
      }
    } else {
      false
    }
  }

  pub fn get_inst(&'ctx self, i: usize) -> Option<InstructionRef<'ctx>> {
    if i < self.get_num_insts() {
      let inst = Instruction::from_skey(self.instance().unwrap().insts[i]);
      return inst.as_ref::<Instruction>(self.ctx)
    } else {
      None
    }
  }

  pub fn last_inst(&'ctx self) -> Option<InstructionRef<'ctx>> {
    self.get_inst(self.get_num_insts() - 1)
  }

  pub fn to_string(&self) -> String {
    if self.is_invalid().is_some() {
      return self.get_name();
    }
    let ctx = self.ctx;
    let insts = self.instance().unwrap().insts.iter().map(|i| {
      let inst_value = ValueRef{skey: *i, kind: crate::ir::value::VKindCode::Instruction};
      let inst = inst_value.as_ref::<Instruction>(ctx).unwrap();
      format!("  {}", inst.to_string(true))
    }).collect::<Vec<String>>().join("\n");
    let pred_comments = self.pred_iter().map(|inst| {
      let pred_block = inst.get_parent();
      let block_name = pred_block.get_name();
      format!("{}", block_name)
    }).collect::<Vec<String>>().join(", ");
    format!("{}:        ; predecessors: [{}]\n{}\n", self.get_name(), pred_comments, insts)
  }

  pub fn succ_iter(&'ctx self) -> impl Iterator<Item = BlockRef<'ctx>> {
    if let Some(last_inst) = self.last_inst() {
      let is_br = if let InstOpcode::Branch = last_inst.get_opcode() { true } else { false };
      last_inst
        .operand_iter()
        .filter(|x| is_br && x.kind == VKindCode::Block)
        .map(|x| x.as_ref::<Block>(self.ctx).unwrap())
        .collect::<Vec<_>>()
        .into_iter()
    } else {
      Vec::new().into_iter()
    }
  }

  /// Iterate over each instruction belongs to this block.
  pub fn inst_iter(&'ctx self) -> impl Iterator<Item = InstructionRef<'ctx>> + DoubleEndedIterator {
    self.instance().unwrap().insts.iter().map(|skey| {
      Instruction::from_skey(*skey).as_ref::<Instruction>(self.ctx).unwrap()
    })
  }

  /// Iterate over each branch instruction destinated to this block.
  pub fn user_iter(&'ctx self) -> impl Iterator<Item = InstructionRef<'ctx>> {
    self.instance().unwrap().users.iter().map(|skey| {
      Instruction::from_skey(*skey).as_ref::<Instruction>(self.ctx).unwrap()
    })
  }

  /// Filter out non-branch instructions.
  pub fn pred_iter(&'ctx self) -> impl Iterator<Item = InstructionRef<'ctx>> {
    self.instance().unwrap().users.iter()
      .map(|skey| {
        Instruction::from_skey(*skey).as_ref::<Instruction>(self.ctx).unwrap()
      }).filter(|inst| {
        if let InstOpcode::Branch = inst.get_opcode() {
          true
        } else {
          false
        }
      })
  }

}

