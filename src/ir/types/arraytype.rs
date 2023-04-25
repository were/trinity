use crate::context::Context;

use super::TypeRef;

/// Pointer type
#[derive(Clone)]
pub struct PointerType {
  pub(crate) skey: Option<usize>,
  pub(super) scalar_ty: TypeRef,
}

impl PointerType {

  pub(crate) fn new(scalar_ty: TypeRef) -> Self {
    PointerType {
      skey: None,
      scalar_ty,
    }
  }

  pub fn to_string(&self, context: &Context) -> String {
    format!("{}*", self.scalar_ty.to_string(context))
  }

  pub fn get_pointee_ty(&self) -> TypeRef {
    self.scalar_ty.clone()
  }

}

/// Array type
#[derive(Clone)]
pub struct ArrayType {
  pub(crate) skey: Option<usize>,
  pub(crate) elem_ty: TypeRef,
  pub(crate) size: usize,
}

impl ArrayType {

  pub(crate) fn new(elem_ty: TypeRef, size: usize) -> Self {
    ArrayType {
      skey: None,
      elem_ty,
      size,
    }
  }

  pub fn to_string(&self, context: &Context) -> String {
    format!("[{} x {}]", self.size, self.elem_ty.to_string(context))
  }

  pub fn get_elem_ty(&self) -> TypeRef {
    self.elem_ty.clone()
  }

  pub fn to_pointer(&self, ctx: &mut Context) -> TypeRef {
    self.elem_ty.ptr_type(ctx)
  }
}
