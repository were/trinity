use slab::Slab;

pub mod component;

pub use component::*;

mod pod;

use crate::ir::{types::TypeRef, value::ValueRef, ddg::{Edge, EdgeImpl}};

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
    where T: ComponentToRef<T> + WithSlabKey {
    if !self.slab.get(skey).is_some() {
      None
    } else {
      Some(T::instance_to_ref(&self.slab[skey]))
    }
  }

  pub fn get_value_mut<T>(&'ctx mut self, skey: usize) -> &'ctx mut T
    where T: ComponentToMut<T> + WithSlabKey {
    if !self.slab.get(skey).is_some() {
      panic!("Invalid slab key: {}", skey);
    }
    T::instance_to_self_mut(&mut self.slab[skey])
  }

  pub fn capacity(&self) -> usize {
    self.slab.capacity()
  }

  pub(super) fn add_instance<T, U>(&mut self, instance: T) -> T::SuperType
    where T: Into<Component> + WithSuperType<U> + ComponentToRef<T> + ComponentToMut<T> + WithSlabKey {
    let skey = self.slab.insert(instance.into());
    let instance_mut = self.get_value_mut::<T>(skey);
    instance_mut.set_skey(skey);
    instance_mut.as_super()
  }

  pub(crate) fn edge(&mut self, def: ValueRef, user: ValueRef) -> usize {
    let instance = Edge::from(EdgeImpl::new(def, user)).into();
    self.add_edge(instance)
  }

  pub(super) fn add_edge(&mut self, edge: Edge) -> usize {
    let res = self.slab.insert(edge.into());
    self.get_value_mut::<Edge>(res).set_skey(res);
    res
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
  pub(crate) fn add_user_redundancy(&mut self, operands: Vec<usize>) {
    for edge_skey in operands.into_iter() {
      let edge = self.get_value_ref::<Edge>(edge_skey).unwrap();
      let def = edge.def().clone();
      match self.slab.get_mut(def.skey).unwrap() {
        Component::Instruction(inst) => inst.instance.users.push(edge_skey),
        Component::Block(block) => block.instance.users.push(edge_skey),
        Component::Function(func) => { func.instance.callers.insert(edge_skey); }
        _ => {}
      }
    }
  }

  /// Remove operand's user whose edge is `edge_skey`.
  pub(crate) fn remove_user_redundancy(&mut self, edge_skey: usize) {
    // let tuple = (user, idx);
    let edge = self.get_value_ref::<Edge>(edge_skey).unwrap();
    let operand = edge.def().clone();
    match self.slab.get_mut(operand.skey).unwrap() {
      Component::Instruction(inst) => inst.instance.users.retain(|x| *x != edge_skey),
      Component::Block(block) => block.instance.users.retain(|x| *x != edge_skey),
      Component::Function(func) => func.instance.callers.retain(|x| *x != edge_skey),
      _ => {}
    }
  }

}

