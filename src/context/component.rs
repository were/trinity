use either::Either;

use crate::ir::{
  types::{
    IntType, VoidType, StructType, PointerType, FunctionType, ArrayType, TypeRef,
    IntImpl, StructImpl, PointerImpl, FuncTypeImpl, ArrayTypeImpl
  },
  value::function::{Function, Argument, FunctionImpl, ArgumentImpl},
  value::instruction::{Instruction, InstructionImpl},
  value::block::{Block, BlockImpl},
  value::consts::{
    ConstScalar, ConstArray, ConstExpr, ConstObject, InlineAsm, Undef,
    ConstScalarImpl, ConstArrayImpl, ConstExprImpl, ConstObjectImpl, InlineAsmImpl, UndefImpl
  },
  ValueRef,
};
use crate::ir::value::VKindCode;
use crate::ir::types::TKindCode;

use super::Context;

/// Manage the slab pointer of each IR component.
pub struct SlabEntry<T: Sized> {
  skey: Option<usize>,
  // TODO(@were): Make this private later.
  pub(crate) instance: T,
}

/// A trait annotates reference cast.
pub trait IsSlabEntry {
  type Impl;

  fn to_slab_entry(&self) -> &SlabEntry<Self::Impl>;

}

pub struct Reference<'ctx, T> {
  pub ctx: &'ctx Context,
  pub(crate) instance: Either<&'ctx SlabEntry<T>, usize>
}

impl <'ctx, T> Reference <'ctx, T> {

  pub fn new(ctx: &'ctx Context, instance: &'ctx SlabEntry<T>) -> Self {
    Reference { ctx, instance: Either::Left(instance) }
  }

  pub fn invalid(ctx: &'ctx Context, skey: usize) -> Self {
    Reference { ctx, instance: Either::Right(skey) }
  }

  pub fn is_invalid(&self) -> Option<usize> {
    self.instance.right()
  }

  pub fn get_skey(&self) -> usize {
    self.instance.left().unwrap().get_skey()
  }

  pub fn instance(&self) -> Option<&'ctx T> {
    if self.instance.is_left() {
      Some(&self.instance.left().unwrap().instance)
    } else {
      None
    }
  }


}

impl<T>GetSlabKey for SlabEntry<T> {

  fn get_skey(&self) -> usize {
    self.skey.unwrap()
  }

}

impl<T>SetSlabKey for SlabEntry<T> {

  fn set_skey(&mut self, skey: usize) {
    self.skey = Some(skey);
  }

}

impl <T>From<T> for SlabEntry<T> {

  fn from(value: T) -> Self {
    SlabEntry { skey: None, instance: value }
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
  fn instance_to_ref<'ctx>(value: &'ctx Component) -> &'ctx T;
}

pub trait ComponentToMut<T> {
  fn instance_to_self_mut<'ctx>(value: &'ctx mut Component) -> &'ctx mut T;
}

macro_rules! impl_component {
  ($super:tt, $code_type:tt, $type:tt, $impl: tt) => {

    impl IsSlabEntry for $type {
      type Impl = $impl;
      fn to_slab_entry(&self) -> &SlabEntry<Self::Impl> {
        self
      }
    }

    impl Reference<'_, $impl> {
      pub fn as_super(&self) -> $super {
        $super { skey: self.get_skey(), kind: $code_type::$type }
      }
    }

    impl ComponentToRef<$type> for $type {

      fn instance_to_ref<'ctx>(value: &'ctx Component) -> &'ctx $type {
        match value {
          Component::$type(v) => v,
          _ => panic!("Invalid type, expect {}", stringify!($type)),
        }
      }
    }

    impl ComponentToMut<$type> for $type {

      fn instance_to_self_mut<'ctx>(value: &'ctx mut Component) -> &'ctx mut $type {
        match value {
          Component::$type(v) => v,
          _ => panic!("Invalid type, expect {}", stringify!($type)),
        }
      }
    }

    impl AsSuper for $type {
      type SuperType = $super;

      fn as_super(&self) -> Self::SuperType {
        $super{ skey: self.skey.unwrap(), kind: $code_type::$type }
      }

    }

    impl $type {
      pub fn from_skey(skey: usize) -> $super {
        $super { skey, kind: $code_type::$type }
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

  };
}

// Types
impl_component!(TypeRef, TKindCode, IntType, IntImpl);
impl_component!(TypeRef, TKindCode, VoidType, ());
impl_component!(TypeRef, TKindCode, StructType, StructImpl);
impl_component!(TypeRef, TKindCode, PointerType, PointerImpl);
impl_component!(TypeRef, TKindCode, FunctionType, FuncTypeImpl);
impl_component!(TypeRef, TKindCode, ArrayType, ArrayTypeImpl);
// Values
impl_component!(ValueRef, VKindCode, Instruction, InstructionImpl);
impl_component!(ValueRef, VKindCode, Function, FunctionImpl);
impl_component!(ValueRef, VKindCode, Argument, ArgumentImpl);
impl_component!(ValueRef, VKindCode, Block, BlockImpl);
impl_component!(ValueRef, VKindCode, ConstScalar, ConstScalarImpl);
impl_component!(ValueRef, VKindCode, ConstArray, ConstArrayImpl);
impl_component!(ValueRef, VKindCode, ConstExpr, ConstExprImpl);
impl_component!(ValueRef, VKindCode, ConstObject, ConstObjectImpl);
impl_component!(ValueRef, VKindCode, InlineAsm, InlineAsmImpl);
impl_component!(ValueRef, VKindCode, Undef, UndefImpl);
