use super::TypeRef;

/// A function signature type
pub struct FunctionType {
  pub(crate) skey: Option<usize>,
  pub(crate) args: Vec<TypeRef>,
  pub(crate) ret_ty: TypeRef,
}

impl FunctionType {
  pub(crate) fn new(ret_ty: TypeRef, args: Vec<TypeRef>) -> Self {
    FunctionType {
      skey: None,
      args,
      ret_ty,
    }
  }

  pub fn ret_ty(&self) -> &TypeRef {
    &self.ret_ty
  }

  pub fn args(&self) -> &[TypeRef] {
    &self.args
  }
}
