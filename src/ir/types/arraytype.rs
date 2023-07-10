use crate::context::{Context, SlabEntry};

use super::TypeRef;

/// Pointer type
#[derive(Clone)]
pub struct PointerImpl {
  pub(super) scalar_ty: TypeRef,
}

pub type PointerType = SlabEntry<PointerImpl>;

impl PointerType {

  pub(crate) fn new(scalar_ty: TypeRef) -> Self {
    Self::from(PointerImpl { scalar_ty, })
  }

  pub fn to_string(&self, context: &Context) -> String {
    format!("{}*", self.get_pointee_ty().to_string(context))
  }

  pub fn get_pointee_ty(&self) -> TypeRef {
    self.instance.scalar_ty.clone()
  }

}

/// Array type
#[derive(Clone)]
pub struct ArrayTypeImpl {
  pub(crate) elem_ty: TypeRef,
  pub(crate) size: usize,
}

pub type ArrayType = SlabEntry<ArrayTypeImpl>;

impl ArrayType {

  pub(crate) fn new(elem_ty: TypeRef, size: usize) -> Self {
    Self::from(ArrayTypeImpl {
      elem_ty,
      size,
    })
  }

  pub fn get_size(&self) -> usize {
    self.instance.size
  }

  pub fn to_string(&self, context: &Context) -> String {
    format!("[{} x {}]", self.get_size(), self.get_elem_ty().to_string(context))
  }

  pub fn get_elem_ty(&self) -> TypeRef {
    self.instance.elem_ty.clone()
  }

  pub fn to_pointer(&self, ctx: &mut Context) -> TypeRef {
    self.get_elem_ty().ptr_type(ctx)
  }
}
