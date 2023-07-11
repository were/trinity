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
  pub(crate) predecessors: Vec<usize>,
}

pub type Block = SlabEntry<BlockImpl>;
pub type BlockRef<'ctx> = Reference<'ctx, BlockImpl>;

impl BlockImpl {

  fn new(name_prefix: String, parent: usize) -> Self {
    Self {
      name_prefix,
      insts: Vec::new(),
      parent,
      predecessors: Vec::new(),
    }
  }

}

impl Block {

  pub fn new(name_prefix: String, parent: &ValueRef) -> Self {
    Block::from(BlockImpl::new(name_prefix, parent.skey))
  }

  pub(crate) fn add_predecessor(&mut self, br: &ValueRef) {
    self.instance.predecessors.push(br.skey);
  }


}

impl <'ctx> BlockRef<'ctx> {

  pub fn get_parent(&self) -> FunctionRef<'ctx> {
    let func = Function::from_skey(self.instance().parent);
    let func = func.as_ref::<Function>(self.ctx).unwrap();
    Reference::new(self.ctx, func)
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
      let inst = InstructionRef::new(ctx, inst);
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

  pub fn get_num_predecessors(&self) -> usize {
    return self.instance().predecessors.len();
  }

  pub fn get_predecessor(&'ctx self, i: usize) -> Option<ValueRef> {
    if i < self.get_num_predecessors() {
      Some(ValueRef{skey: self.instance().predecessors[i], kind: super::VKindCode::Instruction})
    } else {
      None
    }
  }

  pub fn to_string(&self) -> String {
    let ctx = self.ctx;
    let insts = self.instance().insts.iter().map(|i| {
      let inst_value = ValueRef{skey: *i, kind: crate::ir::value::VKindCode::Instruction};
      let inst = inst_value.as_ref::<Instruction>(ctx).unwrap();
      let inst_ref = InstructionRef::new(ctx, inst);
      format!("  {}", inst_ref.to_string())
    }).collect::<Vec<String>>().join("\n");
    let pred_comments = self.instance().predecessors.iter().enumerate().map(|(i, _)| {
      let pred_br = self.get_predecessor(i).unwrap().as_ref::<Instruction>(ctx).unwrap();
      let pred_ref = Reference::new(ctx, pred_br);
      let pred_block = pred_ref.get_parent();
      let block_name = pred_block.get_name();
      format!("{}", block_name)
    }).collect::<Vec<String>>().join(", ");
    format!("{}:        ; predecessors: [{}]\n{}\n", self.get_name(), pred_comments, insts)
  }

  /// Iterate over each instruction belongs to this block.
  pub fn inst_iter(&'ctx self) -> BlockInstIter<'ctx> {
    BlockInstIter {
      ctx: self.ctx,
      iter: self.instance().insts.iter()
    }
  }

  /// Iterate over each branch instruction destinated to this block.
  pub fn pred_iter(&'ctx self, ctx: &'ctx Context) -> BlockInstIter<'ctx> {
    BlockInstIter {
      ctx, iter: self.instance().predecessors.iter(),
    }
  }

}

pub struct BlockInstIter <'ctx> {
  ctx: &'ctx Context,
  iter: std::slice::Iter<'ctx, usize>,
}

impl <'ctx> Iterator for BlockInstIter <'ctx> {
  type Item = InstructionRef<'ctx>;

  fn next(&mut self) -> Option<Self::Item> {
    if let Some(value) = self.iter.next() {
      let v = *value;
      let inst = Instruction::from_skey(v);
      let inst = inst.as_ref::<Instruction>(self.ctx).unwrap();
      let res = Reference::new(self.ctx, inst);
      Some(res)
    } else {
      None
    }
  }

}

impl <'ctx> DoubleEndedIterator for BlockInstIter <'ctx> {

  fn next_back(&mut self) -> Option<Self::Item> {
    if let Some(value) = self.iter.next_back() {
      let v = *value;
      let inst = Instruction::from_skey(v);
      let inst = inst.as_ref::<Instruction>(self.ctx).unwrap();
      let res = Reference::new(self.ctx, inst);
      Some(res)
    } else {
      None
    }
  }

}

