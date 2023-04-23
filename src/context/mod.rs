use slab::Slab;

pub mod component;

use component::{
  Component, ComponentToMut, ComponentToRef,
};

use crate::ir::types::{self, IntType, VoidType };

use self::component::AsSuper;

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

  pub(crate) fn get_value_ref<T: ComponentToRef<T>>(&'ctx self, skey: usize) -> &'ctx T {
    T::instance_to_self(&self.slab[skey])
  }

  pub(crate) fn get_value_mut<T: ComponentToMut<T>>(&'ctx mut self, skey: usize) -> &'ctx mut T {
    T::instance_to_self_mut(&mut self.slab[skey])
  }

  pub(crate) fn num_components(&self) -> usize {
    self.slab.len()
  }

  pub fn add_component(&mut self, instance: Component) -> usize {
    let res = self.slab.insert(instance);
    self.slab[res].set_skey(res);
    res
  }

  pub fn add_instance<T: Into<Component> + AsSuper<T> + ComponentToRef<T>>(&mut self, instance: T) -> T::SuperType {
    let skey = self.slab.insert(instance.into());
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
    let skey = self.slab.insert(instance.into());
    let mut_ref = self.get_value_mut::<VoidType>(skey);
    mut_ref.skey = Some(skey);
    mut_ref.as_super()
  }


}

