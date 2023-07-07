/// The cache of Plain Old Data (POD) related objects.

use std::collections::HashMap;

use crate::ir::types::{TypeRef, TKindCode};
use crate::ir::{types, ValueRef};
use crate::ir::value::consts;

use super::Context;

#[derive(Hash, PartialEq, Eq)]
struct IntType {
  bits: usize,
}

#[derive(Hash, PartialEq, Eq)]
struct VoidType {}

#[derive(Hash, PartialEq, Eq)]
struct PointerType {
  pointee: TypeRef,
}

#[derive(Hash, PartialEq, Eq)]
struct ArrayType {
  element: TypeRef,
  num_elements: usize,
}

#[derive(Hash, PartialEq, Eq)]
struct FunctionType {
  return_type: TypeRef,
  param_types: Vec<TypeRef>,
}

#[derive(Hash, PartialEq, Eq)]
enum TypeHash {
  IntType(IntType),
  VoidType(VoidType),
  ArrayType(ArrayType),
  PointerType(PointerType),
  FunctionType(FunctionType),
}

#[derive(Hash, PartialEq, Eq)]
struct ConstScalar {
  ty: TypeRef,
  value: u64,
}

#[derive(Hash, PartialEq, Eq)]
struct Undef {
  ty: TypeRef,
}

#[derive(Hash, PartialEq, Eq)]
enum ConstHash {
  ConstScalar(ConstScalar),
  Undef(Undef),
}

pub(super) struct Cache {
  type_cache: HashMap<TypeHash, TypeRef>,
  const_cache: HashMap<ConstHash, ValueRef>,
}

pub(super) fn is_pod(ty: &TypeRef) -> bool {
  match ty.kind {
    TKindCode::IntType => true,
    TKindCode::VoidType => true,
    TKindCode::PointerType => true,
    _ => false,
  }
}

impl Cache {

  pub fn new() -> Self {
    Cache {
      type_cache: HashMap::new(),
      const_cache: HashMap::new(),
    }
  }

  fn get_type(&self, key: &TypeHash) -> Option<&TypeRef> {
    self.type_cache.get(key)
  }

  fn insert_type(&mut self, key: TypeHash, ty: TypeRef) {
    self.type_cache.insert(key, ty);
  }

  fn get_const(&self, key: &ConstHash) -> Option<&ValueRef> {
    self.const_cache.get(key)
  }

  fn insert_const(&mut self, key: ConstHash, value: ValueRef) {
    self.const_cache.insert(key, value);
  }

  pub fn int_type(ctx: &mut Context, bits: usize) -> TypeRef {
    let key = TypeHash::IntType(IntType{ bits });
    if let Some(res) = ctx.pod_cache.get_type(&key) {
      res.clone()
    } else {
      let instance = types::IntType::new(bits);
      let res = ctx.add_instance(instance);
      ctx.pod_cache.insert_type(key, res.clone());
      res
    }
  }

  pub fn void_type(ctx: &mut Context) -> TypeRef {
    let key = TypeHash::VoidType(VoidType{});
    if let Some(res) = ctx.pod_cache.get_type(&key) {
      res.clone()
    } else {
      let instance = types::VoidType{skey: None};
      let res = ctx.add_instance(instance);
      ctx.pod_cache.insert_type(key, res.clone());
      res
    }
  }

  pub fn function_type(ctx: &mut Context, return_type: TypeRef, param_types: Vec<TypeRef>) -> TypeRef {
    let key = TypeHash::FunctionType(FunctionType{ return_type: return_type.clone(), param_types: param_types.clone() });
    if let Some(res) = ctx.pod_cache.get_type(&key) {
      res.clone()
    } else {
      let instance = types::FunctionType::new(return_type, param_types);
      let res = ctx.add_instance(instance);
      ctx.pod_cache.insert_type(key, res.clone());
      res
    }
  }

  pub fn pointer_type(ctx: &mut Context, pointee: TypeRef) -> TypeRef {
    let key = TypeHash::PointerType(PointerType{ pointee: pointee.clone() });
    if let Some(res) = ctx.pod_cache.get_type(&key) {
      res.clone()
    } else {
      let instance = types::PointerType::new(pointee);
      let res = ctx.add_instance(instance);
      ctx.pod_cache.insert_type(key, res.clone());
      res
    }
  }

  pub fn array_type(ctx: &mut Context, element: TypeRef, num_elements: usize) -> TypeRef {
    let key = TypeHash::ArrayType(ArrayType{ element: element.clone(), num_elements });
    if let Some(res) = ctx.pod_cache.get_type(&key) {
      res.clone()
    } else {
      let instance = types::ArrayType::new(element, num_elements);
      let res = ctx.add_instance(instance);
      ctx.pod_cache.insert_type(key, res.clone());
      res
    }
  }

  pub fn const_scalar(ctx: &mut Context, ty: TypeRef, value: u64) -> ValueRef {
    let key = ConstHash::ConstScalar(ConstScalar{ ty: ty.clone(), value });
    if let Some(res) = ctx.pod_cache.get_const(&key) {
      res.clone()
    } else {
      let instance = consts::ConstScalar::new(ty, value);
      let res = ctx.add_instance(instance);
      ctx.pod_cache.insert_const(key, res.clone());
      res
    }
  }

  pub fn undef(ctx: &mut Context, ty: TypeRef) -> ValueRef {
    let key = ConstHash::Undef(Undef{ ty: ty.clone() });
    if let Some(res) = ctx.pod_cache.get_const(&key) {
      res.clone()
    } else {
      let instance = consts::Undef::new(ty);
      let res = ctx.add_instance(instance);
      ctx.pod_cache.insert_const(key, res.clone());
      res
    }
  }

}

