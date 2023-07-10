use crate::context::SlabEntry;

use super::TypeRef;

/// A function signature type
pub struct FuncTypeImpl {
  pub(crate) args: Vec<TypeRef>,
  pub(crate) ret_ty: TypeRef,
}

pub type FunctionType = SlabEntry<FuncTypeImpl>;

impl FunctionType {
  pub(crate) fn new(ret_ty: TypeRef, args: Vec<TypeRef>) -> Self {
    Self::from(FuncTypeImpl { args, ret_ty, })
  }

  pub fn ret_ty(&self) -> &TypeRef {
    &self.instance.ret_ty
  }

  pub fn args(&self) -> &[TypeRef] {
    &self.instance.args
  }
}
