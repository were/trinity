use super::{ValueRef, VKindCode, block::Block};
use crate::ir::types::{TypeRef, FunctionType};
use crate::ir::module::namify;
use crate::context::Ptr;

use crate::context::Context;

pub struct ArgumentImpl {
  pub(crate) ty: TypeRef,
  pub(crate) parent: usize
}

pub struct FunctionImpl {
  pub(crate) name: String,
  pub(crate) args: Vec<usize>,
  pub(crate) fty: TypeRef,
  pub(crate) blocks: Vec<usize>,
}

pub type Argument = Ptr<ArgumentImpl>;
pub type Function = Ptr<FunctionImpl>;

impl Function {

  pub fn get_name(&self) -> &String {
    &self.instance.name
  }

  pub fn basic_blocks(&self) -> &Vec<usize> {
    &self.instance.blocks
  }

  pub fn basic_blocks_mut(&mut self) -> &mut Vec<usize> {
    &mut self.instance.blocks
  }

  pub fn get_num_args(&self) -> usize {
    return self.instance.args.len();
  }

  pub fn get_arg(&self, i: usize) -> ValueRef {
    return Argument::from_skey(self.instance.args[i]);
  }

  pub fn get_num_blocks(&self) -> usize {
    return self.instance.blocks.len();
  }

  /// Get the type of the function.
  pub fn get_type(&self) -> TypeRef {
    return self.instance.fty.clone();
  }

  pub fn get_block(&self, i: usize) -> Option<ValueRef> {
    if i < self.instance.blocks.len() {
      Some(ValueRef{skey: self.instance.blocks[i], kind: VKindCode::Block})
    } else {
      None
    }
  }

  pub fn get_ret_ty(&self, ctx: &Context) -> TypeRef {
    return self.instance.fty.as_ref::<FunctionType>(ctx).unwrap().ret_ty.clone();
  }

  pub fn is_declaration(&self) -> bool {
    return self.instance.blocks.len() == 0;
  }

  pub fn to_string(&self, ctx: &Context) -> String {
    let mut res = String::new();
    let fty = self.instance.fty.as_ref::<FunctionType>(ctx).unwrap();
    let prefix = if self.is_declaration() {
      "declare"
    } else {
      "define dso_local"
    };
    res.push_str(format!("{} {} @{}(", prefix, fty.ret_ty.to_string(&ctx), namify(&self.get_name())).as_str());
    for i in 0..self.get_num_args() {
      if i != 0 {
        res.push_str(", ");
      }
      let arg_ref = self.get_arg(i);
      let arg = arg_ref.as_ref::<Argument>(ctx).unwrap();
      res.push_str(format!("{}", arg.to_string(&ctx)).as_str());
    }
    res.push_str(")");
    if !self.is_declaration() {
      res.push_str(" {\n");
      for block in self.iter(ctx) {
        res.push_str(block.to_string(&ctx).as_str());
      }
      res.push_str("}");
    } else {
      res.push_str(";");
    }
    return res;
  }

  pub fn iter<'ctx>(&'ctx self, ctx: &'ctx Context) -> FuncBlockIter {
    FuncBlockIter{ i: 0, func: self, ctx }
  }

  pub(crate) fn new(name: String, fty: TypeRef, args: Vec<usize>) -> Self {
    let instance = FunctionImpl{name, args, fty, blocks: Vec::new()};
    Function::from(instance)
  }

}

pub struct FuncBlockIter<'ctx> {
  i: usize,
  func: &'ctx Function,
  ctx: &'ctx Context
}

impl <'ctx>Iterator for FuncBlockIter<'ctx> {

  type Item = &'ctx Block;

  fn next(&mut self) -> Option<Self::Item> {
    if self.i < self.func.get_num_blocks() {
      let res = self.func.get_block(self.i).unwrap();
      self.i += 1;
      res.as_ref::<Block>(self.ctx)
    } else {
      None
    }
  }

}

/// Function argument
impl Argument {

  pub fn get_parent(&self) -> ValueRef {
    return Function::from_skey(self.instance.parent);
  }

  pub fn get_name(&self) -> String {
    format!("arg.{}", self.get_ptr())
  }

  pub fn to_string(&self, context: &Context) -> String {
    format!("{} %{}", self.instance.ty.to_string(context), self.get_name())
  }

  pub fn from_fty(fty: &FunctionType) -> Vec<Self> {
    fty.args.iter().map(|ty| {
      let instance = ArgumentImpl{ty: ty.clone(), parent: 0};
      Argument::from(instance)
    }).collect()
  }

}

