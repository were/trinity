use slab::Slab;

pub mod component;

pub use component::*;

mod pod;

use crate::ir::{types::TypeRef, value::ValueRef};

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

  pub fn dispose(&mut self, skey: usize) {
    self.slab.remove(skey);
  }

  pub fn get_value_ref<T>(&'ctx self, skey: usize) -> Option<&'ctx T>
    where T: ComponentToRef<T> + GetSlabKey {
    if !self.slab.get(skey).is_some() {
      None
    } else {
      Some(T::instance_to_ref(&self.slab[skey]))
    }
  }

  pub fn get_value_mut<T>(&'ctx mut self, skey: usize) -> &'ctx mut T
    where T: ComponentToMut<T> + GetSlabKey {
    if !self.slab.get(skey).is_some() {
      panic!("Invalid slab key: {}", skey);
    }
    T::instance_to_self_mut(&mut self.slab[skey])
  }

  pub fn capacity(&self) -> usize {
    self.slab.capacity()
  }

  pub(super) fn add_instance<T>(&mut self, instance: T) -> T::SuperType
    where T: Into<Component> + AsSuper + ComponentToRef<T> + ComponentToMut<T> + GetSlabKey + SetSlabKey {
    let skey = self.slab.insert(instance.into());
    let instance_mut = self.get_value_mut::<T>(skey);
    instance_mut.set_skey(skey);
    instance_mut.as_super()
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
  }

  pub fn undef(&mut self, ty: TypeRef) -> ValueRef {
    pod::Cache::undef(self, ty)
  }

  /// `src` uses these `operands`.
  pub(crate) fn add_user_redundancy(&mut self, src: &ValueRef, operands: Vec<(ValueRef, usize)>) {
    for (operand, idx) in operands.iter() {
      match self.slab.get_mut(operand.skey).unwrap() {
        Component::Instruction(inst) => inst.add_user(src, *idx),
        Component::Block(block) => block.add_user(src, *idx),
        Component::Function(func) => func.add_user(src, *idx),
        _ => {}
      }
    }
  }

  pub(crate) fn remove_user_redundancy(&mut self, operand: ValueRef, user: ValueRef, idx: usize) {
    // let tuple = (user, idx);
    match self.slab.get_mut(operand.skey).unwrap() {
      Component::Instruction(inst) => inst.remove_user(&user, idx),
      Component::Block(block) => block.remove_user(&user, idx),
      Component::Function(func) => func.remove_user(&user, idx),
      _ => {}
    }
    // if let Some(operand) = operand.as_mut::<Instruction>(self) {
    //   operand.instance.users.retain(|u| *u != tuple);
    // }
    // if let Some(block) = operand.as_mut::<Block>(self) {
    //   block.instance.users.retain(|u| *u != tuple);
    // }
    // if let Some(func) = operand.as_mut::<Function>(self) {
    //   func.instance.callers.retain(|u| *u != tuple.0.skey);
    // }
  }



}

