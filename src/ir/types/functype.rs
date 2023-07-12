use crate::context::{SlabEntry, Reference};

use super::TypeRef;

/// A function signature type
pub struct FuncTypeImpl {
  pub(crate) args: Vec<TypeRef>,
  pub(crate) ret_ty: TypeRef,
}

pub type FunctionType = SlabEntry<FuncTypeImpl>;
pub type FunctionTypeRef<'ctx> = Reference<'ctx, FuncTypeImpl>;

impl FunctionType {
  pub(crate) fn new(ret_ty: TypeRef, args: Vec<TypeRef>) -> Self {
    Self::from(FuncTypeImpl { args, ret_ty, })
  }
}

impl FunctionTypeRef<'_> {

  pub fn ret_ty(&self) -> &TypeRef {
    &self.instance().unwrap().ret_ty
  }

  pub fn args(&self) -> &[TypeRef] {
    &self.instance().unwrap().args
  }

}
