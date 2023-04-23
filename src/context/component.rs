use crate::ir::{
  types::{IntType, VoidType, StructType, PointerType, FunctionType, ArrayType, TypeRef},
  function::{Function, Argument},
  instruction::Instruction,
  block::Block,
  consts::{ConstScalar, ConstArray, ConstExpr, ConstObject},
  ValueRef,
};
use crate::ir::value::VKindCode;
use crate::ir::types::TKindCode;


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
}


pub trait WithKindCode<T> {
  fn kind_code() -> T;
}

pub trait AsSuper<T> {
  type SuperType;
  fn as_super(&self) -> Self::SuperType;
}

impl Component {

  // TODO(@were): Make this "more" private later.
  pub(crate) fn set_skey(&mut self, skey: usize) {
    match self {
      Component::IntType(v) => v.skey = Some(skey),
      Component::VoidType(v) => v.skey = Some(skey),
      Component::StructType(v) => v.skey = Some(skey),
      Component::PointerType(v) => v.skey = Some(skey),
      Component::FunctionType(v) => v.skey = Some(skey),
      Component::Function(v) => v.skey = Some(skey),
      Component::Argument(v) => v.skey = Some(skey),
      Component::Instruction(v) => v.skey = Some(skey),
      Component::Block(v) => v.skey = Some(skey),
      Component::ConstScalar(v) => v.skey = Some(skey),
      Component::ArrayType(v) => v.skey = Some(skey),
      Component::ConstArray(v) => v.skey = Some(skey),
      Component::ConstExpr(v) => v.skey = Some(skey),
      Component::ConstObject(v) => v.skey = Some(skey),
    }
  }

}


pub trait ComponentToRef<T> {
  fn instance_to_self<'ctx>(value: &'ctx Component) -> &'ctx T;
}

pub trait ComponentToMut<T> {
  fn instance_to_self_mut<'ctx>(value: &'ctx mut Component) -> &'ctx mut T;
}

macro_rules! impl_component_to_xx {
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

    impl AsSuper<$super> for $type {
      type SuperType = $super;

      fn as_super(&self) -> Self::SuperType {
        $super{ skey: self.skey.clone().unwrap(), kind: $code_type::$type }
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
impl_component_to_xx!(TypeRef, TKindCode, IntType);
impl_component_to_xx!(TypeRef, TKindCode, VoidType);
impl_component_to_xx!(TypeRef, TKindCode, StructType);
impl_component_to_xx!(TypeRef, TKindCode, PointerType);
impl_component_to_xx!(TypeRef, TKindCode, FunctionType);
impl_component_to_xx!(TypeRef, TKindCode, ArrayType);
// Values
impl_component_to_xx!(ValueRef, VKindCode, Function);
impl_component_to_xx!(ValueRef, VKindCode, Argument);
impl_component_to_xx!(ValueRef, VKindCode, Instruction);
impl_component_to_xx!(ValueRef, VKindCode, Block);
impl_component_to_xx!(ValueRef, VKindCode, ConstScalar);
impl_component_to_xx!(ValueRef, VKindCode, ConstArray);
impl_component_to_xx!(ValueRef, VKindCode, ConstExpr);
impl_component_to_xx!(ValueRef, VKindCode, ConstObject);


