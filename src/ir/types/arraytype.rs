use crate::context::{SlabEntry, Reference};

use super::TypeRef;

/// Pointer type
#[derive(Clone)]
pub struct PointerImpl {
  pub(super) scalar_ty: TypeRef,
}

pub type PointerType = SlabEntry<PointerImpl>;
pub type PointerTypeRef<'ctx> = Reference<'ctx, PointerImpl>;

impl PointerType {

  pub(crate) fn new(scalar_ty: TypeRef) -> Self {
    Self::from(PointerImpl { scalar_ty, })
  }

}

impl <'ctx>PointerTypeRef<'ctx> {

  pub fn to_string(&self) -> String {
    format!("{}*", self.get_pointee_ty().to_string(self.ctx))
  }

  pub fn get_pointee_ty(&self) -> TypeRef {
    self.instance().scalar_ty.clone()
  }

}

/// Array type
#[derive(Clone)]
pub struct ArrayTypeImpl {
  pub(crate) elem_ty: TypeRef,
  /// A redunant type {elem_ty}*
  pub(crate) ptr_ty: TypeRef,
  pub(crate) size: usize,
}

pub type ArrayType = SlabEntry<ArrayTypeImpl>;
pub type ArrayTypeRef<'ctx> = Reference<'ctx, ArrayTypeImpl>;

impl ArrayType {

  pub(crate) fn new(elem_ty: TypeRef, ptr_ty: TypeRef, size: usize) -> Self {
    Self::from(ArrayTypeImpl {
      elem_ty,
      ptr_ty,
      size,
    })
  }

}

impl <'ctx>ArrayTypeRef<'ctx> {

  pub fn get_size(&self) -> usize {
    self.instance().size
  }

  pub fn to_string(&self) -> String {
    format!("[{} x {}]", self.get_size(), self.get_elem_ty().to_string(self.ctx))
  }

  pub fn get_elem_ty(&self) -> TypeRef {
    self.instance().elem_ty.clone()
  }

  pub fn to_pointer(&self) -> TypeRef {
    self.instance().ptr_ty.clone()
  }
}
