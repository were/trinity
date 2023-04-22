use crate::context::component::{ComponentToSelf, ComponentToSelfMut};

use super::block::Block;
use super::function::Function;
use super::instruction::Instruction;
use super::module::Module;
use super::types::TypeRef;

#[derive(Clone)]
pub struct Argument {
  pub(crate) skey: Option<usize>,
  pub(crate) ty: TypeRef,
  pub(crate) arg_idx: usize,
  pub(crate) parent: ValueRef
}

#[derive(Clone)]
pub struct ValueRef {
  pub skey: usize,
  pub v_kind: VKindCode
}

impl<'ctx> ValueRef {

  pub fn as_ref<T: WithVKindCode + ComponentToSelf<T>>(&'ctx self, module: &'ctx Module) -> Option<&'ctx T> {
    if self.v_kind == T::kind_code() {
      Some(module.context.get_value_ref::<T>(self.skey))
    } else {
      None
    }
  }

  pub fn as_mut<T: WithVKindCode + ComponentToSelfMut<T>>(&'ctx self, module: &'ctx mut Module) -> Option<&'ctx mut T> {
    if self.v_kind == T::kind_code() {
      Some(module.context.get_value_mut::<T>(self.skey))
    } else {
      None
    }
  }

}

pub enum Value {
  Argument(Argument),
  Instruction(Instruction),
  Function(Function),
  Block(Block),
}

#[derive(Clone, PartialEq)]
pub enum VKindCode {
  Argument,
  Instruction,
  Function,
  Block,
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

