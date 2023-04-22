use std::rc::Rc;
use std::fmt;

use super::module::Module;
use crate::context::Context;
use crate::context::component::{ComponentToSelf, ComponentToSelfMut};

pub trait AsTypeRef {
  fn as_type_ref(&self) -> TypeRef;
}

pub trait WithTypeKind {
  fn kind_code() -> TypeKind;
}

macro_rules! impl_as_type_ref {
  ($type:tt) => {
    impl AsTypeRef for $type {
      fn as_type_ref(&self) -> TypeRef {
        TypeRef{ skey: self.skey.clone().unwrap(), kind: TypeKind::$type }
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

/// Pointer type
#[derive(Clone)]
pub struct PointerType {
  pub(crate) skey: Option<usize>,
  scalar_ty: TypeRef,
}

/// Struct type
pub struct StructType {
  pub(crate) skey: Option<usize>,
  pub(crate) name: String,
  pub(crate) attrs: Vec<TypeRef>,
}

impl StructType {

  pub fn to_string(&self, ctx: &Context) -> String {
    let attrs = self.attrs.iter().map(|attr| attr.to_string(&ctx)).collect::<Vec<_>>().join(", ");
    format!("%{} = {{ {} }}", self.name, attrs)
  }

}

impl StructType {

  pub fn new(name: String) -> Self {
    StructType {
      skey: None,
      name,
      attrs: Vec::new(),
    }
  }

  pub fn set_body(&mut self, elements: Vec<TypeRef>) {
    self.attrs = elements;
  }

}

impl PointerType {

  pub fn to_string(&self, context: &Context) -> String {
    format!("{}*", self.scalar_ty.to_string(context))
  }

}

/// A function signature type
pub struct FunctionType {
  pub(crate) skey: Option<usize>,
  pub(crate) args: Vec<TypeRef>,
  pub(crate) ret_ty: Rc<TypeRef>,
}

#[derive(Clone)]
pub struct TypeRef {
  pub(crate) skey: usize,
  pub(crate) kind: TypeKind
}

impl<'ctx> TypeRef {

  pub fn to_string(&self, ctx: &Context) -> String {
    match &self.kind {
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
        ty.to_string(ctx)
      },
      TypeKind::PointerType => {
        let ty = ctx.get_value_ref::<PointerType>(self.skey);
        ty.to_string(ctx)
      },
      TypeKind::FunctionType => {
        todo!("Function type dump not implemented");
      },
    }
  }

  pub fn as_ref<T: WithTypeKind + ComponentToSelf<T>>(&'ctx self, module: &'ctx Module) -> Option<&'ctx T> {
    if self.kind == T::kind_code() {
      Some(module.context.get_value_ref::<T>(self.skey))
    } else {
      None
    }
  }

  pub fn as_mut<T: WithTypeKind + ComponentToSelfMut<T>>(&'ctx self, module: &'ctx mut Module) -> Option<&'ctx mut T> {
    if self.kind == T::kind_code() {
      Some(module.context.get_value_mut::<T>(self.skey))
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
    let fty = FunctionType{skey: None, args, ret_ty: Rc::new(self.clone())};
    let skey = ctx.add_component(fty.into());
    let fty = ctx.get_value_mut::<FunctionType>(skey);
    fty.skey = Some(skey);
    fty.as_type_ref()
  }

}

#[derive(Clone, PartialEq)]
pub enum TypeKind {
  IntType,
  VoidType,
  StructType,
  PointerType,
  FunctionType,
}

