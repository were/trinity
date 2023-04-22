use super::types::TypeRef;

pub struct ConstValue {
  pub(crate) skey: Option<usize>,
  pub(crate) ty: TypeRef,
  pub(crate) value: u64
}
