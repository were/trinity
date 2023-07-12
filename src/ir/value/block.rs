use crate::{context::{Context, SlabEntry, Reference}, ir::value::instruction::InstructionRef};

use super::{ValueRef, instruction::{Instruction, InstOpcode}, Function, function::FunctionRef};

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
    let func = Function::from_skey(self.instance().parent);
    func.as_ref::<Function>(self.ctx).unwrap()
  }

  pub fn get_num_insts(&self) -> usize {
    return self.instance().insts.len();
  }

  pub fn get_name(&self) -> String {
    format!("{}.{}", self.instance().name_prefix, self.get_skey())
  }

  /// If this block is closed, i.e. ends with a branch.
  pub fn closed(&self) -> bool {
    let ctx = self.ctx;
    if let Some(inst_ref) = self.instance().insts.last() {
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

  pub fn get_inst(&'ctx self, i: usize) -> Option<ValueRef> {
    if i < self.get_num_insts() {
      Some(Instruction::from_skey(self.instance().insts[i]))
    } else {
      None
    }
  }

  pub fn to_string(&self) -> String {
    let ctx = self.ctx;
    let insts = self.instance().insts.iter().map(|i| {
      let inst_value = ValueRef{skey: *i, kind: crate::ir::value::VKindCode::Instruction};
      let inst = inst_value.as_ref::<Instruction>(ctx).unwrap();
      format!("  {}", inst.to_string())
    }).collect::<Vec<String>>().join("\n");
    let pred_comments = self.pred_iter().map(|inst| {
      let pred_block = inst.get_parent();
      let block_name = pred_block.get_name();
      format!("{}", block_name)
    }).collect::<Vec<String>>().join(", ");
    format!("{}:        ; predecessors: [{}]\n{}\n", self.get_name(), pred_comments, insts)
  }

  /// Iterate over each instruction belongs to this block.
  pub fn inst_iter(&'ctx self) -> BlockInstIter<'ctx> {
    BlockInstIter {
      ctx: self.ctx,
      iter: self.instance().insts.iter(),
      cond: |_| { true }
    }
  }

  /// Iterate over each branch instruction destinated to this block.
  pub fn user_iter(&'ctx self) -> BlockInstIter<'ctx> {
    BlockInstIter {
      ctx: self.ctx, iter: self.instance().users.iter(),
      cond: |_| { true }
    }
  }

  /// Filter out non-branch instructions.
  pub fn pred_iter(&'ctx self) -> BlockInstIter<'ctx> {
    BlockInstIter {
      ctx: self.ctx, iter: self.instance().users.iter(),
      cond: |inst| {
        if let InstOpcode::Branch = inst.get_opcode() {
          true
        } else {
          false
        }
      }
    }
  }

}

pub struct BlockInstIter <'ctx> {
  ctx: &'ctx Context,
  iter: std::slice::Iter<'ctx, usize>,
  cond: fn (&InstructionRef) -> bool,
}

macro_rules! iter_impl {
  ($next: tt) => {
    fn $next (&mut self) -> Option<Self::Item> {
      loop {
        if let Some(value) = self.iter. $next () {
          let v = *value;
          let inst = Instruction::from_skey(v);
          let inst = inst.as_ref::<Instruction>(self.ctx).unwrap();
          if (self.cond)(&inst) {
            return Some(inst);
          }
        } else {
          return None;
        }
      }
    }
  };
}

impl <'ctx> Iterator for BlockInstIter <'ctx> {
  type Item = InstructionRef<'ctx>;
  iter_impl!(next);

}

impl <'ctx> DoubleEndedIterator for BlockInstIter <'ctx> {
  iter_impl!(next_back);
}

