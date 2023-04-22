use super::{
  value::{ValueRef, VKindCode},
  types::{TypeRef, FunctionType}, module::namify, block::Block,
};

use crate::context::Context;

#[derive(Clone)]
pub struct Argument {
  pub(crate) skey: Option<usize>,
  pub(crate) ty: TypeRef,
  pub(crate) arg_idx: usize,
  pub(crate) parent: usize
}

pub struct Function {
  pub(crate) skey: Option<usize>,
  pub(crate) name: String,
  pub(crate) args: Vec<usize>,
  pub(crate) fty: TypeRef,
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

  pub fn get_ret_ty(&self, ctx: &Context) -> TypeRef {
    return self.fty.as_ref::<FunctionType>(ctx).unwrap().ret_ty.clone();
  }

  pub fn to_string(&self, ctx: &Context) -> String {
    let mut res = String::new();
    let fty = self.fty.as_ref::<FunctionType>(ctx).unwrap();
    res.push_str(format!("define dso_local {} @{}(", fty.ret_ty.to_string(&ctx), namify(&self.name)).as_str());
    for i in 0..self.get_num_args() {
      if i != 0 {
        res.push_str(", ");
      }
      let arg_ref = self.get_arg(i);
      let arg = arg_ref.as_ref::<Argument>(ctx).unwrap();
      res.push_str(format!("{}", arg.to_string(&ctx)).as_str());
    }
    res.push_str(")");
    if self.blocks.len() != 0 {
      res.push_str(" {\n");
      for i in 0..self.get_num_blocks() {
        let block_ref = self.get_block(i);
        let block = block_ref.as_ref::<Block>(ctx).unwrap();
        res.push_str(block.to_string(&ctx).as_str());
      }
      res.push_str("}");
    } else {
      res.push_str(";");
    }
    return res;
  }

}

impl Argument {

  pub fn get_parent(&self) -> ValueRef {
    return ValueRef{ skey: self.parent, v_kind: VKindCode::Function };
  }

  pub fn name(&self) -> String {
    format!("%arg.{}", self.arg_idx)
  }

  pub fn to_string(&self, context: &Context) -> String {
    format!("{} {}", self.ty.to_string(context), self.name())
  }

}

