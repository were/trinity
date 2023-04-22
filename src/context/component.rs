use crate::ir::{
  types::{IntType, VoidType, StructType, PointerType, FunctionType},
  function::{Function, Argument},
  instruction::Instruction,
  block::Block, consts::ConstValue,
};

// TODO(@were): Make this private later.
pub enum Component {
  // Types
  IntType(IntType),
  VoidType(VoidType),
  StructType(StructType),
  PointerType(PointerType),
  FunctionType(FunctionType),
  // Values
  Function(Function),
  Argument(Argument),
  Instruction(Instruction),
  Block(Block),
  ConstValue(ConstValue),
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
      Component::ConstValue(v) => v.skey = Some(skey),
    }
  }

}

macro_rules! impl_component_related {
  ($type:tt) => {

    impl ComponentToSelf<$type> for $type {
      fn instance_to_self<'ctx>(value: &'ctx Component) -> &'ctx $type {
        match value {
          Component::$type(v) => v,
          _ => panic!("Invalid type"),
        }
      }
    }

    impl ComponentToSelfMut<$type> for $type {
      fn instance_to_self_mut<'ctx>(value: &'ctx mut Component) -> &'ctx mut $type {
        match value {
          Component::$type(v) => v,
          _ => panic!("Invalid type"),
        }
      }
    }

    impl From<$type> for Component {
      fn from(value: $type) -> Self {
        Component::$type(value)
      }
    }

  };
}


// Types
impl_component_related!(IntType);
impl_component_related!(VoidType);
impl_component_related!(StructType);
impl_component_related!(PointerType);
impl_component_related!(FunctionType);
// Values
impl_component_related!(Function);
impl_component_related!(Argument);
impl_component_related!(Instruction);
impl_component_related!(Block);
impl_component_related!(ConstValue);

pub trait ComponentToSelf<T> {
  fn instance_to_self<'ctx>(value: &'ctx Component) -> &'ctx T;
}

pub trait ComponentToSelfMut<T> {
  fn instance_to_self_mut<'ctx>(value: &'ctx mut Component) -> &'ctx mut T;
}

