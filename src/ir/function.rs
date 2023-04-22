use super::{
  value::{Argument, ValueRef, VKindCode},
  types::FunctionType,
};

use crate::context::Context;

pub struct Function {
  pub(crate) skey: Option<usize>,
  pub(crate) name: String,
  pub(crate) args: Vec<usize>,
  pub(crate) fty: FunctionType,
  pub(crate) blocks: Vec<usize>,
}

impl Function {

  pub fn get_num_args(&self) -> usize {
    return self.args.len();
  }

  pub fn get_arg(&self, i: usize) -> ValueRef {
    return ValueRef{skey: self.args[i], v_kind: VKindCode::Argument};
  }

  pub fn get_num_blocks(&self) -> usize {
    return self.blocks.len();
  }

  pub fn get_block(&self, i: usize) -> ValueRef {
    return ValueRef{skey: self.blocks[i], v_kind: VKindCode::Block};
  }

}

impl Argument {

  pub fn name(&self) -> String {
    format!("%arg.{}", self.arg_idx)
  }

  pub fn to_string(&self, context: &Context) -> String {
    format!("{} {}", self.ty.to_string(context), self.name())
  }

}

