use slab::Slab;

pub mod component;

pub use component::*;

use component::{
  Component, ComponentToMut, ComponentToRef, AsSuper, GetSlabKey
};
use crate::ir::{
  types::{self, IntType, VoidType, TypeRef, TKindCode },
  value::consts::ConstScalar, value::ValueRef
};

pub struct Context {
  /// All the instance of the IR components managed by the slab.
  slab: Slab<Component>,
}

impl<'ctx> Context {

  /// Create a new context.
  pub fn new() -> Context {
    Context {
      slab: Slab::new(),
    }
  }

  pub(crate) fn get_value_ref<T: ComponentToRef<T> + GetSlabKey>(&'ctx self, skey: usize) -> &'ctx T {
    T::instance_to_self(&self.slab[skey])
  }

  pub(crate) fn get_value_mut<T: ComponentToMut<T> + GetSlabKey>(&'ctx mut self, skey: usize) -> &'ctx mut T {
    T::instance_to_self_mut(&mut self.slab[skey])
  }

  pub(crate) fn num_components(&self) -> usize {
    self.slab.len()
  }

  fn add_component(&mut self, instance: Component) -> usize {
    let res = self.slab.insert(instance);
    res
  }

  pub fn add_instance<T: Into<Component> + AsSuper<U> + ComponentToRef<T> + ComponentToMut<T> + GetSlabKey, U>(&mut self, instance: T) -> T::SuperType {
    let skey = self.add_component(instance.into());
    self.slab[skey].set_skey(skey);
    let instance_ref = self.get_value_ref::<T>(skey);
    instance_ref.as_super()
  }

  // TODO(@were): Move these to the context.
  /// Get an integer type
  pub fn int_type(&mut self, bits: usize) -> types::TypeRef {
    let skey = self.add_component(Component::IntType(IntType::new(bits)));
    self.get_value_ref::<IntType>(skey).as_super()
  }

  /// Get a void type
  pub fn void_type(&mut self) -> types::TypeRef {
    let instance = types::VoidType{skey: None};
    self.add_instance::<VoidType, _>(instance)
  }

  pub fn const_value(&mut self, ty: TypeRef, value: u64) -> ValueRef {
    assert!(ty.kind == TKindCode::IntType);
    let instance = ConstScalar{
      skey: None,
      ty: ty.clone(),
      value: value as u64
    };
    self.add_instance::<ConstScalar, _>(instance)
  }

}

