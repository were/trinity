use crate::context::Context;

use super::{ValueRef, instruction::{Instruction, InstOpcode}};

pub struct Block {
  /// The slab key of this block.
  pub(crate) skey: Option<usize>,
  /// The name prefix of this block.
  pub(crate) name_prefix: String,
  /// The slab keys of the instructions in this block.
  pub(crate) insts: Vec<usize>,
  /// The slab key of the parent function.
  pub(crate) parent: usize,
  /// The slab keys of the branch instructions target this block.
  pub(crate) predecessors: Vec<usize>,
}

impl Block {

  pub fn get_parent(&self) -> ValueRef {
    ValueRef{skey: self.parent, kind: super::VKindCode::Function}
  }

  pub fn get_num_insts(&self) -> usize {
    return self.insts.len();
  }

  pub fn get_name(&self) -> String {
    format!("{}.{}", self.name_prefix, self.skey.unwrap())
  }

  /// If this block is closed, i.e. ends with a branch.
  pub fn closed(&self, ctx: &Context) -> bool {
    if let Some(inst_ref) = self.insts.last() {
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

  pub fn get_inst(&self, i: usize) -> Option<ValueRef> {
    if i < self.insts.len() {
      let res = ValueRef{skey: self.insts[i], kind: super::VKindCode::Instruction};
      Some(res)
    } else {
      None
    }
  }

  pub fn get_num_predecessors(&self) -> usize {
    return self.predecessors.len();
  }

  pub fn get_predecessor(&self, i: usize) -> Option<ValueRef> {
    if i < self.get_num_predecessors() {
      Some(ValueRef{skey: self.predecessors[i], kind: super::VKindCode::Instruction})
    } else {
      None
    }
  }

  pub fn to_string(&self, ctx: &Context) -> String {
    let insts = self.insts.iter().map(|i| {
      let inst_ref = ValueRef{skey: *i, kind: crate::ir::value::VKindCode::Instruction};
      let inst = inst_ref.as_ref::<Instruction>(ctx).unwrap();
      format!("  {}", inst.to_string(ctx))
    }).collect::<Vec<String>>().join("\n");
    let pred_comments = self.predecessors.iter().enumerate().map(|(i, _)| {
      let pred_block = self.get_predecessor(i).unwrap().as_ref::<Instruction>(ctx).unwrap().get_parent();
      let block_name = pred_block.as_ref::<Block>(ctx).unwrap().get_name();
      format!("{}", block_name)
    }).collect::<Vec<String>>().join(", ");
    format!("{}:        ; predecessors: [{}]\n{}\n", self.get_name(), pred_comments, insts)
  }

  /// Iterate over each instruction belongs to this block.
  pub fn inst_iter<'ctx>(&'ctx self, ctx: &'ctx Context) -> BlockInstIter<'ctx> {
    BlockInstIter {
      idx: [0, self.get_num_insts()],
      block: self,
      ctx,
      get_inst: Block::get_inst,
    }
  }

  /// Iterate over each branch instruction destinated to this block.
  pub fn pred_iter<'ctx>(&'ctx self, ctx: &'ctx Context) -> BlockInstIter<'ctx> {
    BlockInstIter {
      idx: [0, self.get_num_predecessors()],
      block: self,
      ctx,
      get_inst: Block::get_predecessor,
    }
  }

}

pub struct BlockInstIter <'ctx> {
  idx: [usize; 2],
  block: &'ctx Block,
  ctx: &'ctx Context,
  get_inst: fn(&'ctx Block, usize) -> Option<ValueRef>,
}

impl <'ctx> BlockInstIter <'ctx> {

  pub fn tick(&mut self, which: usize) -> Option<&'ctx Instruction> {
    if self.idx[0] < self.idx[1] {
      let inst = (self.get_inst)(self.block, self.idx[which] - which).unwrap();
      let res = inst.as_ref::<Instruction>(self.ctx).unwrap();
      if which == 0 {
        self.idx[which] += 1;
      } else {
        self.idx[which] -= 1;
      }
      Some(res)
    } else {
      None
    }
  }

}

impl <'ctx> Iterator for BlockInstIter <'ctx> {
  type Item = &'ctx Instruction;

  fn next(&mut self) -> Option<Self::Item> {
    self.tick(0)
  }

}

impl <'ctx> DoubleEndedIterator for BlockInstIter <'ctx> {

  fn next_back(&mut self) -> Option<Self::Item> {
    self.tick(1)
  }

}

