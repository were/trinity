use super::TypeRef;

/// A function signature type
pub struct FunctionType {
  pub(crate) skey: Option<usize>,
  pub(crate) args: Vec<TypeRef>,
  pub(crate) ret_ty: TypeRef,
}
