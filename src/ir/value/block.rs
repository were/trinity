use crate::{
  context::{SlabEntry, Reference, Context, WithSuperType},
  ir::{value::instruction::InstructionRef, ddg::Edge}
};

use super::{
  ValueRef, instruction::{Instruction, InstOpcode, BranchInst},
  Function, function::{FunctionRef, FuncMutator}, VKindCode
};

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
      let inst = ValueRef{ skey: *inst_ref, kind: VKindCode::Instruction };
      let inst = inst.as_ref::<Instruction>(ctx).unwrap();
      match inst.get_opcode() {
        InstOpcode::Branch(_) | InstOpcode::Return => true,
        _ => false
      }
    } else {
      false
    }
  }

  pub fn get_inst(&'ctx self, i: usize) -> Option<InstructionRef<'ctx>> {
    self.instance().unwrap().insts.get(i).map(|x| {
      let inst = Instruction::from_skey(*x);
      inst.as_ref::<Instruction>(self.ctx).unwrap()
    })
  }

  pub fn last_inst(&'ctx self) -> Option<InstructionRef<'ctx>> {
    if self.get_num_insts() == 0 {
      return None;
    }
    self.get_inst(self.get_num_insts() - 1)
  }

  /// If this block is a loop head.
  ///
  /// According to LLVM's definition (refer `https://llvm.org/docs/LoopTerminology.html`
  /// for more details), a latch is a branch instructions destinated to the loop head.
  /// Therefore, a loop head is a block is targeted by at least one latch.
  pub fn is_loop_head(&self) -> Option<InstructionRef> {
    self.pred_iter().filter(|x| {
      if let Some(br) = x.as_sub::<BranchInst>() {
        br.is_loop_latch() && br.true_label().unwrap().get_skey() == self.get_skey()
      } else {
        false
      }
    }).next()
  }

  pub fn to_string(&self, comment: bool) -> String {
    if self.is_invalid().is_some() {
      return self.get_name();
    }
    let ctx = self.ctx;
    let insts = self.instance().unwrap().insts.iter().map(|i| {
      let inst_value = Instruction::from_skey(*i);
      let inst = inst_value.as_ref::<Instruction>(ctx).unwrap();
      format!("  {}", inst.to_string(false))
    }).collect::<Vec<String>>().join("\n");
    let pred_comments = self.pred_iter().map(|inst| {
      let pred_block = inst.get_parent();
      let block_name = pred_block.get_name();
      format!("{}", block_name)
    }).collect::<Vec<String>>().join(", ");
    let mut res = format!("{}:        ; predecessors: [{}]\n{}\n",
                          self.get_name(), pred_comments, insts);
    if comment {
      self.user_iter().for_each(|user| {
        if *user.get_opcode() == InstOpcode::Phi {
          res = format!("; used by phi: {} in block: {}\n{}",
                        user.get_name(), user.get_parent().get_name(), res);
        }
      });
    }
    res
  }

  pub fn succ_iter(&'ctx self) -> Box<dyn Iterator<Item = BlockRef<'ctx>> + 'ctx> {
    if let Some(inst) = self.last_inst() {
      if let Some(br) = inst.as_sub::<BranchInst>() {
        // TODO(@were): Better manage the lifetime of each returned value.
        let vec = br.succ_iter().map(|x| x.get_skey()).collect::<Vec<_>>();
        return Box::new(
          vec.into_iter().map(|x| Block::from_skey(x).as_ref::<Block>(self.ctx).unwrap()));
      }
    }
    return Box::new(std::iter::empty());
  }

  /// Iterate over each instruction belongs to this block.
  pub fn inst_iter(&'ctx self) -> impl Iterator<Item = InstructionRef<'ctx>> + DoubleEndedIterator {
    self.instance().unwrap().insts.iter().map(|skey| {
      Instruction::from_skey(*skey).as_ref::<Instruction>(self.ctx).unwrap()
    })
  }

  /// Iterate over each branch instruction destinated to this block.
  pub fn user_iter(&'ctx self) -> impl Iterator<Item = InstructionRef<'ctx>> {
    self.instance().unwrap().users.iter().map(|edge| {
      self.ctx
        .get_value_ref::<Edge>(*edge)
        .unwrap()
        .user()
        .as_ref::<Instruction>(self.ctx)
        .unwrap()
    })
  }

  /// Filter out non-branch instructions.
  pub fn pred_iter(&'ctx self) -> impl Iterator<Item = InstructionRef<'ctx>> {
    self
      .user_iter()
      .filter_map(|inst| {
        if let InstOpcode::Branch(_) = inst.get_opcode() {
          Some(inst)
        } else {
          None
        }
      })
  }

}

pub struct BlockMutator<'ctx> {
  ctx: &'ctx mut Context,
  value: ValueRef,
}

impl <'ctx> BlockMutator<'ctx> {

  pub fn new(ctx: &'ctx mut Context, value: ValueRef) -> Self {
    value.as_ref::<Block>(ctx).unwrap();
    Self { ctx, value }
  }

  pub fn erase_from_parent(&mut self) {
    let block = self.value.as_ref::<Block>(self.ctx).unwrap();
    assert!(block.get_num_insts() == 0);
    assert!(block.user_iter().count() == 0);
    let func = block.get_parent().as_super();
    let func_mut = func.as_mut::<Function>(self.ctx).unwrap();
    func_mut.basic_blocks_mut().retain(|x| *x != self.value.skey);
  }

  pub fn replace_all_uses_with(&mut self, new: ValueRef) {
    let func = self.value.as_ref::<Block>(self.ctx).unwrap().get_parent().as_super();
    let old = self.value.clone();
    let mut mutator = FuncMutator::new(self.ctx, func);
    mutator.replace_all_uses_with(old, new);
  }

}
