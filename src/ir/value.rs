use crate::context::Context;
use crate::context::component::{ComponentToSelf, ComponentToSelfMut};

use super::block::Block;
use super::function::{Function, Argument};
use super::instruction::Instruction;
use super::module::Module;
use super::types::TypeRef;
use super::consts::{ConstScalar, ConstArray};

#[derive(Clone)]
pub struct ValueRef {
  pub skey: usize,
  pub v_kind: VKindCode
}

impl<'ctx> ValueRef {

  pub fn as_ref<T: WithVKindCode + ComponentToSelf<T>>(&'ctx self, context: &'ctx Context) -> Option<&'ctx T> {
    if self.v_kind == T::kind_code() {
      Some(context.get_value_ref::<T>(self.skey))
    } else {
      None
    }
  }

  pub fn as_mut<T: WithVKindCode + ComponentToSelfMut<T>>(&'ctx self, context: &'ctx mut Context) -> Option<&'ctx mut T> {
    if self.v_kind == T::kind_code() {
      Some(context.get_value_mut::<T>(self.skey))
    } else {
      None
    }
  }

  pub fn to_string(&self, ctx: &'ctx Context) -> String {
    match self.v_kind {
      VKindCode::Block => {
        let block = ctx.get_value_ref::<Block>(self.skey);
        format!("%{}", block.name)
      },
      VKindCode::Argument => {
        let arg = ctx.get_value_ref::<Argument>(self.skey);
        format!("%arg.{}", arg.arg_idx)
      },
      VKindCode::Instruction => {
        let inst = ctx.get_value_ref::<Instruction>(self.skey);
        format!("%{}", inst.name)
      },
      VKindCode::ConstScalar => {
        let const_scalar = ctx.get_value_ref::<ConstScalar>(self.skey);
        format!("{} {}", const_scalar.ty.to_string(ctx), const_scalar.value)
      },
      VKindCode::Function => {
        let func = ctx.get_value_ref::<Function>(self.skey);
        format!("{} @{}", func.get_ret_ty(ctx).to_string(ctx), func.name)
      },
      VKindCode::ConstArray => {
        let const_array = ctx.get_value_ref::<ConstArray>(self.skey);
        format!("{} @{}", const_array.ty.to_string(ctx), const_array.name)
      },
      VKindCode::Unknown => {
        format!("[unknown]")
      }
    }
  }

}

#[derive(Clone, PartialEq)]
pub enum VKindCode {
  Argument,
  Instruction,
  Function,
  Block,
  ConstScalar,
  ConstArray,
  Unknown
}

pub trait WithVKindCode {
  fn kind_code() -> VKindCode;
}

pub trait FindInstance<'ctx, T> {
  fn find_instance(module: &'ctx Module, value: &'ctx ValueRef) -> &'ctx T;
}

pub trait TypedValueRef {
  fn get_type() -> TypeRef;
}

pub trait FindInstanceMut<'ctx, T> {
  fn find_instance(module: &'ctx mut Module, value: &'ctx ValueRef) -> &'ctx mut T;
}


macro_rules! impl_as_ref {
  ($type:tt) => {
    impl $type {
      pub fn as_ref(&self) -> ValueRef {
        ValueRef { skey: self.skey.clone().unwrap(), v_kind: VKindCode::$type }
      }
    }
    impl WithVKindCode for $type {
      fn kind_code() -> VKindCode {
        VKindCode::$type
      }
    }
  };
}

impl_as_ref!(Argument);
impl_as_ref!(Block);
impl_as_ref!(Function);
impl_as_ref!(Instruction);
impl_as_ref!(ConstScalar);
impl_as_ref!(ConstArray);

