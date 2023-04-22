use slab::Slab;

pub mod component;

use component::{
  Component, ComponentToSelf, ComponentToSelfMut,
};

use crate::ir::types::{self, AsTypeRef, IntType, VoidType};

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

  pub(crate) fn get_value_ref<T: ComponentToSelf<T>>(&'ctx self, skey: usize) -> &'ctx T {
    T::instance_to_self(&self.slab[skey])
  }

  pub(crate) fn get_value_mut<T: ComponentToSelfMut<T>>(&'ctx mut self, skey: usize) -> &'ctx mut T {
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

  // TODO(@were): Move these to the context.
  /// Get an integer type
  pub fn int_type(&mut self, bits: usize) -> types::TypeRef {
    let skey = self.slab.insert(IntType::new(bits).into());
    let mut_ref = self.get_value_mut::<IntType>(skey);
    mut_ref.skey = Some(skey);
    mut_ref.as_type_ref()
  }

  /// Get a void type
  pub fn void_type(&mut self) -> types::TypeRef {
    let instance = types::VoidType{skey: None};
    let skey = self.slab.insert(instance.into());
    let mut_ref = self.get_value_mut::<VoidType>(skey);
    mut_ref.skey = Some(skey);
    mut_ref.as_type_ref()
  }


}

