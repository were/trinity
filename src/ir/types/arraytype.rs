use crate::{context::Context, ir::value::ValueRef};

use super::TypeRef;

/// Pointer type
#[derive(Clone)]
pub struct PointerType {
  pub(crate) skey: Option<usize>,
  pub(super) scalar_ty: TypeRef,
}

impl PointerType {

  pub fn to_string(&self, context: &Context) -> String {
    format!("{}*", self.scalar_ty.to_string(context))
  }

}

/// Array type
#[derive(Clone)]
pub struct ArrayType {
  pub(crate) skey: Option<usize>,
  pub(crate) size: ValueRef,
  pub(crate) elem_ty: TypeRef,
}

impl ArrayType {

  pub fn to_string(&self, context: &Context) -> String {
    format!("[{} x {}]", self.size.to_string(context), self.elem_ty.to_string(context))
  }

}
