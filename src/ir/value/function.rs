use std::collections::HashSet;

use super::Instruction;
use super::block::BlockRef;
use super::{ValueRef, VKindCode, block::Block};
use crate::ir::types::{TypeRef, FunctionType};
use crate::ir::module::namify;
use crate::context::{SlabEntry, Reference};

pub struct ArgumentImpl {
  pub(crate) ty: TypeRef,
  pub(crate) parent: usize
}

pub struct FunctionImpl {
  pub(crate) name: String,
  pub(crate) args: Vec<usize>,
  pub(crate) fty: TypeRef,
  pub(crate) blocks: Vec<usize>,
  pub(crate) callers: HashSet<usize>,
}

pub type Argument = SlabEntry<ArgumentImpl>;
pub type ArgumentRef<'ctx> = Reference<'ctx, ArgumentImpl>;
pub type Function = SlabEntry<FunctionImpl>;
pub type FunctionRef<'ctx> = Reference<'ctx, FunctionImpl>;

impl Function {

  pub(crate) fn new(name: String, fty: TypeRef, args: Vec<usize>) -> Self {
    let instance = FunctionImpl{name, args, fty, blocks: Vec::new(), callers: HashSet::new()};
    Function::from(instance)
  }

  pub(crate) fn add_caller(&mut self, caller: &ValueRef) {
    self.instance.callers.insert(caller.skey);
  }

  pub(crate) fn remove_caller(&mut self, caller: ValueRef) {
    self.instance.callers.remove(&caller.skey);
  }

  pub fn basic_blocks_mut(&mut self) -> &mut Vec<usize> {
    &mut self.instance.blocks
  }


}

impl <'ctx>FunctionRef<'ctx> {

  pub fn get_name(&self) -> &String {
    &self.instance().name
  }

  pub fn basic_blocks(&self) -> &Vec<usize> {
    &self.instance().blocks
  }

  pub fn get_num_args(&self) -> usize {
    return self.instance().args.len();
  }

  pub fn get_arg(&self, i: usize) -> ValueRef {
    return Argument::from_skey(self.instance().args[i]);
  }

  pub fn get_num_blocks(&self) -> usize {
    return self.instance().blocks.len();
  }

  /// Get the type of the function.
  pub fn get_type(&self) -> TypeRef {
    return self.instance().fty.clone();
  }

  pub fn get_block(&'ctx self, i: usize) -> Option<BlockRef> {
    if i < self.instance().blocks.len() {
      let value = ValueRef {skey: self.instance().blocks[i], kind: VKindCode::Block};
      Some(value.as_ref::<Block>(self.ctx).unwrap())
    } else {
      None
    }
  }

  pub fn get_ret_ty(&self) -> TypeRef {
    let fty = self.instance().fty.as_ref::<FunctionType>(self.ctx).unwrap();
    fty.ret_ty().clone()
  }

  pub fn is_declaration(&self) -> bool {
    return self.instance().blocks.len() == 0;
  }

  pub fn to_string(&self) -> String {
    let ctx = self.ctx;
    let mut res = String::new();
    for elem in self.instance().callers.iter() {
      let caller = Instruction::from_skey(*elem);
      res.push_str(format!("; caller: {}\n", caller.to_string(ctx, true)).as_str());
    }
    let fty = self.instance().fty.as_ref::<FunctionType>(ctx).unwrap();
    let prefix = if self.is_declaration() {
      "declare"
    } else {
      "define dso_local"
    };
    res.push_str(format!("{} {} @{}(", prefix, fty.ret_ty().to_string(&ctx), namify(&self.get_name())).as_str());
    for i in 0..self.get_num_args() {
      if i != 0 {
        res.push_str(", ");
      }
      let arg_ref = self.get_arg(i);
      let arg = arg_ref.as_ref::<Argument>(ctx).unwrap();
      res.push_str(format!("{}", arg.to_string()).as_str());
    }
    res.push_str(")");
    if !self.is_declaration() {
      res.push_str(" {\n");
      for block in self.iter() {
        res.push_str(block.to_string().as_str());
      }
      res.push_str("}");
    } else {
      res.push_str(";");
    }
    return res;
  }

  pub fn iter(&'ctx self) -> FuncBlockIter {
    FuncBlockIter{ i: 0, func: self }
  }

}

pub struct FuncBlockIter<'ctx> {
  i: usize,
  func: &'ctx FunctionRef<'ctx>,
}

impl <'ctx>Iterator for FuncBlockIter<'ctx> {

  type Item = BlockRef<'ctx>;

  fn next(&mut self) -> Option<Self::Item> {
    if self.i < self.func.get_num_blocks() {
      let res = self.func.get_block(self.i).unwrap();
      self.i += 1;
      Some(res)
    } else {
      None
    }
  }

}

/// Function argument
impl Argument {

  pub fn from_fty(fty: &FunctionType) -> Vec<Self> {
    fty.instance.args.iter().map(|ty| {
      let instance = ArgumentImpl{ty: ty.clone(), parent: 0};
      Argument::from(instance)
    }).collect()
  }

}

impl <'ctx>ArgumentRef<'ctx> {

  pub fn get_parent(&self) -> FunctionRef<'ctx> {
    let func = Function::from_skey(self.instance().parent);
    func.as_ref::<Function>(self.ctx).unwrap()
  }

  pub fn get_name(&self) -> String {
    format!("arg.{}", self.get_skey())
  }

  pub fn get_type(&self) -> TypeRef {
    self.instance().ty.clone()
  }

  pub fn to_string(&self) -> String {
    format!("{} %{}", self.instance().ty.to_string(self.ctx), self.get_name())
  }
}

