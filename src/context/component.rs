use crate::ir::{
  types::{IntType, VoidType, StructType, PointerType, FunctionType, ArrayType, TypeRef},
  value::function::{Function, Argument},
  value::instruction::Instruction,
  value::block::Block,
  value::consts::{ConstScalar, ConstArray, ConstExpr, ConstObject, InlineAsm, Undef},
  ValueRef,
};
use crate::ir::value::VKindCode;
use crate::ir::types::TKindCode;

/// Manage the slab pointer of each IR component.
pub struct Ptr<T: Sized> {
  ptr: Option<usize>,
  // TODO(@were): Make this private later.
  pub(crate) instance: T,
}

impl<T>GetSlabKey for Ptr<T> {

  fn get_skey(&self) -> usize {
    self.ptr.unwrap()
  }

}

impl<T>SetSlabKey for Ptr<T> {

  fn set_skey(&mut self, skey: usize) {
    self.ptr = Some(skey);
  }

}

impl <T>From<T> for Ptr<T> {

  fn from(value: T) -> Self {
    Ptr { ptr: None, instance: value }
  }

}

// TODO(@were): Make this private later.
pub enum Component {
  // Types
  IntType(IntType),
  VoidType(VoidType),
  StructType(StructType),
  PointerType(PointerType),
  FunctionType(FunctionType),
  ArrayType(ArrayType),
  // Values
  Function(Function),
  Argument(Argument),
  Instruction(Instruction),
  Block(Block),
  ConstScalar(ConstScalar),
  ConstArray(ConstArray),
  ConstExpr(ConstExpr),
  ConstObject(ConstObject),
  InlineAsm(InlineAsm),
  Undef(Undef),
}

pub trait WithKindCode<T> {
  fn kind_code() -> T;
}

pub trait AsSuper {
  type SuperType;
  fn as_super(&self) -> Self::SuperType;
}

pub trait GetSlabKey {
  fn get_skey(&self) -> usize;
}

pub(crate) trait SetSlabKey {
  fn set_skey(&mut self, skey: usize);
}

pub trait ComponentToRef<T> {
  fn instance_to_self<'ctx>(value: &'ctx Component) -> &'ctx T;
}

pub trait ComponentToMut<T> {
  fn instance_to_self_mut<'ctx>(value: &'ctx mut Component) -> &'ctx mut T;
}

macro_rules! impl_component {
  ($super:tt, $code_type:tt, $type:tt) => {

    impl ComponentToRef<$type> for $type {

      fn instance_to_self<'ctx>(value: &'ctx Component) -> &'ctx $type {
        match value {
          Component::$type(v) => v,
          _ => panic!("Invalid type"),
        }
      }
    }

    impl ComponentToMut<$type> for $type {

      fn instance_to_self_mut<'ctx>(value: &'ctx mut Component) -> &'ctx mut $type {
        match value {
          Component::$type(v) => v,
          _ => panic!("Invalid type"),
        }
      }
    }

    impl AsSuper for $type {
      type SuperType = $super;

      fn as_super(&self) -> Self::SuperType {
        $super{ skey: self.ptr.unwrap(), kind: $code_type::$type }
      }

    }

    impl $type {
      pub fn from_skey(skey: usize) -> $super {
        $super { skey: skey, kind: $code_type::$type }
      }
    }

    impl From<$type> for Component {
      fn from(value: $type) -> Self {
        Component::$type(value)
      }
    }

    impl WithKindCode<$code_type> for $type {
      fn kind_code() -> $code_type {
        $code_type::$type
      }
    }

    // impl GetSlabKey for $type {
    //   fn get_skey(&self) -> usize {
    //     self.get_ptr()
    //   }
    // }

    // impl SetSlabKey for $type {
    //   fn set_skey(&mut self, skey: usize) {
    //     self.set_ptr(skey);
    //   }
    // }

  };
}


// Types
impl_component!(TypeRef, TKindCode, IntType);
impl_component!(TypeRef, TKindCode, VoidType);
impl_component!(TypeRef, TKindCode, StructType);
impl_component!(TypeRef, TKindCode, PointerType);
impl_component!(TypeRef, TKindCode, FunctionType);
impl_component!(TypeRef, TKindCode, ArrayType);
// Values
impl_component!(ValueRef, VKindCode, Function);
impl_component!(ValueRef, VKindCode, Argument);
impl_component!(ValueRef, VKindCode, Instruction);
impl_component!(ValueRef, VKindCode, Block);
impl_component!(ValueRef, VKindCode, ConstScalar);
impl_component!(ValueRef, VKindCode, ConstArray);
impl_component!(ValueRef, VKindCode, ConstExpr);
impl_component!(ValueRef, VKindCode, ConstObject);
impl_component!(ValueRef, VKindCode, InlineAsm);
impl_component!(ValueRef, VKindCode, Undef);

