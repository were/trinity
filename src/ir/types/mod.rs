use std::fmt;

pub mod arraytype;
pub mod functype;

pub use arraytype::{PointerType, ArrayType};
pub use functype::FunctionType;

use crate::context::Context;
use crate::context::component::{ComponentToRef, ComponentToMut, WithKindCode, GetSlabKey};
use crate::ir::value::consts::ConstArray;

use super::value::ValueRef;

// Register all the types here.

#[derive(Clone, PartialEq)]
pub enum TKindCode {
  IntType,
  VoidType,
  StructType,
  PointerType,
  FunctionType,
  ArrayType,
  BlockType,
}


/// Very basic integer type
#[derive(Clone)]
pub struct IntType {
  pub(crate) skey: Option<usize>,
  bits: usize,
}

impl IntType {
  
  /// Construct an integer type
  pub(crate) fn new(bits: usize) -> Self {
    IntType { skey: None, bits }
  }

  /// Return the number of bits
  pub fn get_bits(&self) -> usize {
    self.bits
  }

}

impl fmt::Display for IntType {

  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "i{}", self.bits)
  }

}

/// Void type
pub struct VoidType {
  pub(crate) skey: Option<usize>,
}

impl fmt::Display for VoidType {

  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "void")
  }

}

/// Struct type
pub struct StructType {
  pub(crate) skey: Option<usize>,
  pub(crate) name: String,
  pub(crate) attrs: Vec<TypeRef>,
}

impl StructType {

  pub fn to_string(&self, ctx: &Context) -> String {
    let attrs = self.attrs.iter().map(|attr| attr.to_string(ctx)).collect::<Vec<_>>().join(", ");
    format!("%{} = type {{ {} }}", self.name, attrs)
  }

  pub fn get_num_attrs(&self) -> usize {
    self.attrs.len()
  }

  pub fn get_attr(&self, i: usize) -> TypeRef {
    self.attrs[i].clone()
  }

  pub fn new(name: String) -> Self {
    StructType {
      skey: None,
      name,
      attrs: Vec::new(),
    }
  }

  pub fn get_name(&self) -> &str {
    &self.name
  }

  pub fn set_body(&mut self, elements: Vec<TypeRef>) {
    self.attrs = elements;
  }

}

#[derive(Clone)]
pub struct TypeRef {
  pub(crate) skey: usize,
  pub(crate) kind: TKindCode
}

impl<'ctx> TypeRef {

  pub fn kind(&self) -> &TKindCode {
    &self.kind
  }

  pub fn to_string(&self, ctx: &Context) -> String {
    match &self.kind {
      TKindCode::IntType => {
        let ty = ctx.get_value_ref::<IntType>(self.skey);
        ty.to_string()
      },
      TKindCode::VoidType => {
        let ty = ctx.get_value_ref::<VoidType>(self.skey);
        ty.to_string()
      },
      TKindCode::StructType => {
        let ty = ctx.get_value_ref::<StructType>(self.skey);
        format!("%{}", ty.get_name().to_string())
      },
      TKindCode::PointerType => {
        let ty = ctx.get_value_ref::<PointerType>(self.skey);
        ty.to_string(ctx)
      },
      TKindCode::ArrayType => {
        let ty = ctx.get_value_ref::<ArrayType>(self.skey);
        ty.to_string(ctx)
      },
      TKindCode::BlockType => {
        String::from("")
      },
      TKindCode::FunctionType => {
        todo!("Function type dump not implemented");
      },
    }
  }

  pub fn as_ref<T: WithKindCode<TKindCode> + ComponentToRef<T> + GetSlabKey>(&'ctx self, ctx: &'ctx Context) -> Option<&'ctx T> {
    if self.kind == T::kind_code() {
      Some(ctx.get_value_ref::<T>(self.skey))
    } else {
      None
    }
  }

  pub fn as_mut<T: WithKindCode<TKindCode> + ComponentToMut<T> + GetSlabKey>(&'ctx self, ctx: &'ctx mut Context) -> Option<&'ctx mut T> {
    if self.kind == T::kind_code() {
      Some(ctx.get_value_mut::<T>(self.skey))
    } else {
      None
    }
  }

  pub fn ptr_type(&self, ctx: &mut Context) -> TypeRef {
    let scalar_ty: TypeRef = self.clone();
    ctx.add_instance(PointerType{skey: None, scalar_ty})
  }

  pub fn fn_type(&self, ctx: &mut Context, args: Vec<TypeRef>) -> TypeRef {
    let fty = FunctionType{skey: None, args, ret_ty: self.clone()};
    ctx.add_instance(fty)
  }

  pub fn array_type(&self, ctx: &mut Context, size: ValueRef) -> TypeRef {
    let array_ty = ArrayType{skey: None, elem_ty: self.clone(), size};
    ctx.add_instance(array_ty)
  }

  pub fn const_array(&self, ctx: &mut Context, name: String, value: Vec<ValueRef>) -> ValueRef {
    assert!(self.kind == TKindCode::ArrayType);
    // TODO(@were): Check the types.
    let const_array = ConstArray {
      skey: None,
      name,
      ty: self.ptr_type(ctx),
      value
    };
    ctx.add_instance(const_array)
  }

}

