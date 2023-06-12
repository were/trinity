use crate::context::Context;

use super::{ValueRef, instruction::Instruction};

pub struct Block {
  pub(crate) skey: Option<usize>,
  pub(crate) name_prefix: String,
  pub(crate) insts: Vec<usize>,
  pub(crate) parent: usize,
}

impl Block {

  pub fn get_parent(&self) -> ValueRef {
    ValueRef{skey: self.parent, kind: crate::ir::value::VKindCode::Function}
  }

  pub fn get_num_insts(&self) -> usize {
    return self.insts.len();
  }

  pub fn get_name(&self) -> String {
    format!("{}.{}", self.name_prefix, self.skey.unwrap())
  }

  pub fn get_inst(&self, i: usize) -> ValueRef {
    ValueRef{skey: self.insts[i], kind: crate::ir::value::VKindCode::Instruction}
  }

  pub fn to_string(&self, ctx: &Context) -> String {
    let insts = self.insts.iter().map(|i| {
      let inst_ref = ValueRef{skey: *i, kind: crate::ir::value::VKindCode::Instruction};
      let inst = inst_ref.as_ref::<Instruction>(ctx).unwrap();
      format!("  {}", inst.to_string(ctx))
    }).collect::<Vec<String>>().join("\n");
    format!("{}:\n{}\n", self.get_name(), insts)
  }

  pub fn iter(&self) -> BlockInstIter {
    BlockInstIter { i: 0, block: self }
  }
}

pub struct BlockInstIter <'ctx> {
  i: usize,
  block: &'ctx Block
}

impl <'ctx> Iterator for BlockInstIter <'ctx> {
  type Item = ValueRef;

  fn next(&mut self) -> Option<Self::Item> {
    if self.i < self.block.insts.len() {
      let res = self.block.get_inst(self.i);
      self.i += 1;
      Some(res)
    } else {
      None
    }
  }
}

