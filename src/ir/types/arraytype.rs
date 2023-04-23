use crate::{context::Context, ir::{value::ValueRef, consts::ConstScalar}};

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

  pub fn get_scalar_ty(&self) -> TypeRef {
    self.scalar_ty.clone()
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
    let size = if let Some(const_int)  = self.size.as_ref::<ConstScalar>(context) {
      format!("{}", const_int.value)
    } else {
      self.size.to_string(context)
    };
    format!("[{} x {}]", size, self.elem_ty.to_string(context))
  }

}
