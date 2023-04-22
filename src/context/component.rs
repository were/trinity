use crate::ir::{
  types::{IntType, VoidType, StructType, PointerType},
  function::Function,
  value::Argument,
  instruction::Instruction,
  block::Block,
};

// TODO(@were): Make this private later.
pub enum Component {
  // Types
  IntType(IntType),
  VoidType(VoidType),
  StructType(StructType),
  PointerType(PointerType),
  // Values
  Function(Function),
  Argument(Argument),
  Instruction(Instruction),
  Block(Block),
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
// Values
impl_component_related!(Function);
impl_component_related!(Argument);
impl_component_related!(Instruction);
impl_component_related!(Block);

pub trait ComponentToSelf<T> {
  fn instance_to_self<'ctx>(value: &'ctx Component) -> &'ctx T;
}

pub trait ComponentToSelfMut<T> {
  fn instance_to_self_mut<'ctx>(value: &'ctx mut Component) -> &'ctx mut T;
}

