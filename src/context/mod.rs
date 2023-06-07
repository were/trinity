use slab::Slab;

pub mod component;

pub use component::*;

mod pod;

use component::{
  Component, ComponentToMut, ComponentToRef, AsSuper, GetSlabKey
};

use crate::ir::{ types::TypeRef, value::ValueRef };

pub struct Context {
  /// All the instance of the IR components managed by the slab.
  slab: Slab<Component>,
  /// The cache of Plain Old Data (POD) related objects.
  pod_cache: pod::Cache,
}

impl<'ctx> Context {

  /// Create a new context.
  pub fn new() -> Context {
    Context {
      slab: Slab::new(),
      pod_cache: pod::Cache::new(),
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

  pub(crate) fn get_name(&self, prefix: &str) -> String {
    format!("{}.{}", prefix, self.num_components())
  }

  fn add_component(&mut self, instance: Component) -> usize {
    let res = self.slab.insert(instance);
    res
  }

  pub(super) fn add_instance<T, U>(&mut self, instance: T) -> T::SuperType
    where T: Into<Component> + AsSuper<U> + ComponentToRef<T> + ComponentToMut<T> + GetSlabKey {
    let skey = self.add_component(instance.into());
    self.slab[skey].set_skey(skey);
    let instance_ref = self.get_value_ref::<T>(skey);
    instance_ref.as_super()
  }

  // TODO(@were): Move these to the context.
  /// Get an integer type
  pub fn int_type(&mut self, bits: usize) -> TypeRef {
    pod::Cache::int_type(self, bits)
  }

  /// Get a void type
  pub fn void_type(&mut self) -> TypeRef {
    pod::Cache::void_type(self)
  }

  /// Get a pointer type
  pub fn pointer_type(&mut self, pointee: TypeRef) -> TypeRef {
    pod::Cache::pointer_type(self, pointee)
  }

  /// Get an array type
  pub fn array_type(&mut self, element: TypeRef, num_elements: usize) -> TypeRef {
    pod::Cache::array_type(self, element, num_elements)
  }

  /// Get a function type
  pub fn function_type(&mut self, return_type: TypeRef, param_types: Vec<TypeRef>) -> TypeRef {
    pod::Cache::function_type(self, return_type, param_types)
  }

  pub fn const_value(&mut self, ty: TypeRef, value: u64) -> ValueRef {
    assert!(pod::is_pod(&ty));
    pod::Cache::const_scalar(self, ty, value)
    // let instance = ConstScalar{
    //   skey: None, ty,
    //   value: value as u64
    // };
    // self.add_instance(instance)
  }


}

