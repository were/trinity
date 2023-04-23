use std::fmt;

pub mod arraytype;
pub mod functype;

pub use arraytype::{PointerType, ArrayType};
pub use functype::FunctionType;

use crate::context::Context;
use crate::context::component::{ComponentToSelf, ComponentToSelfMut};

use super::consts::{ConstScalar, ConstArray};
use super::value::ValueRef;

pub trait AsTypeRef {
  fn as_type_ref(&self) -> TypeRef;
}

pub trait WithTypeKind {
  fn kind_code() -> TypeKind;
}

// Register all the types here.

macro_rules! impl_as_type_ref {
  ($type:tt) => {
    impl AsTypeRef for $type {
      fn as_type_ref(&self) -> TypeRef {
        TypeRef{ skey: self.skey.clone().unwrap(), type_kind: TypeKind::$type }
      }
    }
    impl WithTypeKind for $type {
      fn kind_code() -> TypeKind {
        TypeKind::$type
      }
    }
  };
}

impl_as_type_ref!(StructType);
impl_as_type_ref!(PointerType);
impl_as_type_ref!(FunctionType);
impl_as_type_ref!(IntType);
impl_as_type_ref!(VoidType);
impl_as_type_ref!(ArrayType);

#[derive(Clone, PartialEq)]
pub enum TypeKind {
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
    format!("%{} = {{ {} }}", self.name, attrs)
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
  pub(crate) type_kind: TypeKind
}

impl<'ctx> TypeRef {

  pub fn kind(&self) -> &TypeKind {
    &self.type_kind
  }

  pub fn to_string(&self, ctx: &Context) -> String {
    match &self.type_kind {
      TypeKind::IntType => {
        let ty = ctx.get_value_ref::<IntType>(self.skey);
        ty.to_string()
      },
      TypeKind::VoidType => {
        let ty = ctx.get_value_ref::<VoidType>(self.skey);
        ty.to_string()
      },
      TypeKind::StructType => {
        let ty = ctx.get_value_ref::<StructType>(self.skey);
        format!("%{}", ty.get_name().to_string())
      },
      TypeKind::PointerType => {
        let ty = ctx.get_value_ref::<PointerType>(self.skey);
        ty.to_string(ctx)
      },
      TypeKind::ArrayType => {
        let ty = ctx.get_value_ref::<ArrayType>(self.skey);
        ty.to_string(ctx)
      },
      TypeKind::BlockType => {
        String::from("")
      },
      TypeKind::FunctionType => {
        todo!("Function type dump not implemented");
      },
    }
  }

  pub fn as_ref<T: WithTypeKind + ComponentToSelf<T>>(&'ctx self, ctx: &'ctx Context) -> Option<&'ctx T> {
    if self.type_kind == T::kind_code() {
      Some(ctx.get_value_ref::<T>(self.skey))
    } else {
      None
    }
  }

  pub fn as_mut<T: WithTypeKind + ComponentToSelfMut<T>>(&'ctx self, ctx: &'ctx mut Context) -> Option<&'ctx mut T> {
    if self.type_kind == T::kind_code() {
      Some(ctx.get_value_mut::<T>(self.skey))
    } else {
      None
    }
  }

  pub fn ptr_type(&self, ctx: &mut Context) -> TypeRef {
    let scalar_ty: TypeRef = self.clone();
    let skey = ctx.add_component(PointerType{skey: None, scalar_ty}.into());
    let ptr_ty = ctx.get_value_mut::<PointerType>(skey);
    ptr_ty.skey = Some(skey);
    ptr_ty.as_type_ref()
  }

  pub fn fn_type(&self, ctx: &mut Context, args: Vec<TypeRef>) -> TypeRef {
    let fty = FunctionType{skey: None, args, ret_ty: self.clone()};
    let skey = ctx.add_component(fty.into());
    let fty = ctx.get_value_mut::<FunctionType>(skey);
    fty.skey = Some(skey);
    fty.as_type_ref()
  }

  pub fn const_value(&self, ctx: &mut Context, value: u64) -> ValueRef {
    assert!(self.type_kind == TypeKind::IntType);
    let instance = ConstScalar{
      skey: None,
      ty: self.clone(),
      value: value as u64
    };
    let skey = ctx.add_component(instance.into());
    ctx.get_value_ref::<ConstScalar>(skey).as_ref()
  }

  pub fn array_type(&self, ctx: &mut Context, size: ValueRef) -> TypeRef {
    let array_ty = ArrayType{skey: None, elem_ty: self.clone(), size};
    let skey = ctx.add_component(array_ty.into());
    let array_ty = ctx.get_value_ref::<ArrayType>(skey);
    array_ty.as_type_ref()
  }

  pub fn const_array(&self, ctx: &mut Context, name: String, value: Vec<u8>) -> ValueRef {
    assert!(self.type_kind == TypeKind::ArrayType);
    let res = ConstArray {
      skey: None,
      name,
      ty: self.ptr_type(ctx),
      value
    };
    let skey = ctx.add_component(res.into());
    let res = ctx.get_value_ref::<ConstArray>(skey);
    res.as_ref()
  }

}

