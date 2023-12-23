use crate::context::{SlabEntry, Reference};

use super::TypeRef;

/// Pointer type
#[derive(Clone)]
pub struct PointerImpl { }

pub type PointerType = SlabEntry<PointerImpl>;
pub type PointerTypeRef<'ctx> = Reference<'ctx, PointerImpl>;

impl PointerType {

  pub(crate) fn new() -> Self {
    Self::from(PointerImpl { })
  }

}

impl <'ctx>PointerTypeRef<'ctx> {

  pub fn to_string(&self) -> String {
    if let Some(skey) = self.is_invalid() {
      return format!("<invalid pointer type: {}>", skey);
    }
    return String::from("ptr");
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
    self.instance().unwrap().size
  }

  pub fn to_string(&self) -> String {
    if let Some(skey) = self.is_invalid() {
      return format!("<invalid arraytype: {}>", skey);
    }
    format!("[{} x {}]", self.get_size(), self.get_elem_ty().to_string(self.ctx))
  }

  pub fn get_elem_ty(&self) -> TypeRef {
    self.instance().unwrap().elem_ty.clone()
  }

  pub fn to_pointer(&self) -> TypeRef {
    self.instance().unwrap().ptr_ty.clone()
  }
}
