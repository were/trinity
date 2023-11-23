use std::collections::HashSet;

use super::Instruction;
// use super::Instruction;
use super::block::BlockRef;
use super::instruction::InstMutator;
use super::{ValueRef, VKindCode, block::Block};
use crate::ir::types::functype::FunctionTypeRef;
use crate::ir::types::{TypeRef, FunctionType};
use crate::ir::module::namify;
use crate::context::{SlabEntry, Reference, Context};

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

  pub fn basic_blocks_mut(&mut self) -> &mut Vec<usize> {
    &mut self.instance.blocks
  }

  pub(crate) fn add_user(&mut self, caller: &ValueRef, _: usize) {
    self.instance.callers.insert(caller.skey);
  }

  pub(crate) fn remove_user(&mut self, caller: &ValueRef, _: Option<usize>) {
    self.instance.callers.remove(&caller.skey);
  }

}

impl <'ctx>FunctionRef<'ctx> {

  pub fn get_name(&self) -> String {
    if let Some(skey) = self.is_invalid() {
      return format!("{{invalid.func.{}}}", skey);
    }
    self.instance().unwrap().name.clone()
  }

  pub fn basic_blocks(&self) -> &Vec<usize> {
    &self.instance().unwrap().blocks
  }

  pub fn get_num_args(&self) -> usize {
    return self.instance().unwrap().args.len();
  }

  pub fn get_arg(&self, i: usize) -> ValueRef {
    return Argument::from_skey(self.instance().unwrap().args[i]);
  }

  pub fn get_num_blocks(&self) -> usize {
    return self.instance().unwrap().blocks.len();
  }

  /// Get the type of the function.
  pub fn get_type(&self) -> FunctionTypeRef {
    return self.instance().unwrap().fty.as_ref::<FunctionType>(self.ctx).unwrap();
  }

  pub fn get_block(&'ctx self, i: usize) -> Option<BlockRef> {
    if i < self.instance().unwrap().blocks.len() {
      let value = ValueRef {skey: self.instance().unwrap().blocks[i], kind: VKindCode::Block};
      Some(value.as_ref::<Block>(self.ctx).unwrap())
    } else {
      None
    }
  }

  pub fn get_ret_ty(&self) -> TypeRef {
    let fty = self.instance().unwrap().fty.as_ref::<FunctionType>(self.ctx).unwrap();
    fty.ret_ty().clone()
  }

  pub fn is_declaration(&self) -> bool {
    return self.instance().unwrap().blocks.len() == 0;
  }

  pub fn to_string(&self) -> String {
    if let Some(_) = self.is_invalid() {
      return self.get_name().clone();
    }
    let instance = self.instance().unwrap();
    let ctx = self.ctx;
    let mut res = String::new();
    // for elem in instance.callers.iter() {
    //   let caller = Instruction::from_skey(*elem);
    //   res.push_str(format!("; caller: {}\n", caller.to_string(ctx, true)).as_str());
    // }
    let fty = instance.fty.as_ref::<FunctionType>(ctx).unwrap();
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
      for block in self.block_iter() {
        res.push_str(block.to_string(false).as_str());
      }
      res.push_str("}");
    } else {
      res.push_str(";");
    }
    return res;
  }

  pub fn block_iter(&'ctx self) -> impl Iterator<Item=BlockRef<'ctx>> {
    self.instance().unwrap().blocks.iter().map(|skey| {
      ValueRef {skey: *skey, kind: VKindCode::Block}.as_ref::<Block>(self.ctx).unwrap()
    })
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
    let func = Function::from_skey(self.instance().unwrap().parent);
    func.as_ref::<Function>(self.ctx).unwrap()
  }

  pub fn get_name(&self) -> String {
    if let Some(skey) = self.is_invalid() {
      return format!("{{invalid.arg.{}}}", skey);
    }
    format!("arg.{}", self.get_skey())
  }

  pub fn get_type(&self) -> TypeRef {
    self.instance().unwrap().ty.clone()
  }

  pub fn to_string(&self) -> String {
    if let Some(_) = self.is_invalid() {
      return self.get_name();
    }
    format!("{} %{}", self.instance().unwrap().ty.to_string(self.ctx), self.get_name())
  }
}


pub struct FuncMutator<'ctx> {
  ctx: &'ctx mut Context,
  func: ValueRef
}

impl <'ctx>FuncMutator<'ctx> {

  pub fn new(ctx: &'ctx mut Context, func: ValueRef) -> Self {
    FuncMutator{ctx, func}
  }

  pub fn replace_all_uses_with(&mut self, old: ValueRef, new: ValueRef) -> bool {
    let func = self.func.as_ref::<Function>(self.ctx).unwrap();
    let mut to_replace = Vec::new();
    func.block_iter().for_each(|block| {
      for inst in block.inst_iter() {
        for i in 0..inst.get_num_operands() {
          if inst.get_operand(i).unwrap().skey == old.skey {
            to_replace.push((Instruction::from_skey(inst.get_skey()), i));
          }
        }
      }
    });
    to_replace.iter().for_each(|(before, idx)| {
      let mut inst = InstMutator::new(self.ctx, before);
      inst.set_operand(*idx, new.clone());
    });
    // Maintain the redundant information.
    if let Some(inst) = old.as_mut::<Instruction>(self.ctx) {
      inst.instance.users.clear();
    } else if let Some(block) = old.as_mut::<Block>(self.ctx) {
      block.instance.users.clear();
    }
    return !to_replace.is_empty();
  }

}

