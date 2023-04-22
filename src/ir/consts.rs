use crate::{context::Context, ir::types::ArrayType};

use super::types::TypeRef;

pub struct ConstScalar {
  pub(crate) skey: Option<usize>,
  pub(crate) ty: TypeRef,
  pub(crate) value: u64
}

impl ConstScalar {

  pub fn to_string(&self, ctx: &Context) -> String {
    format!("{} = {}", self.ty.to_string(ctx), self.value)
  }

}

pub struct ConstArray {
  pub(crate) skey: Option<usize>,
  pub(crate) name: String,
  pub(crate) ty: TypeRef,
  pub(crate) value: Vec<u8>
}

impl ConstArray {

  pub fn to_string(&self, ctx: &Context) -> String {
    let literal = self.value.iter().map(|x| format!("\\{:02x}", x)).collect::<Vec<String>>().join(" ");
    let aty = self.ty.as_ref::<ArrayType>(ctx).unwrap();
    format!("@{} = private unnamed_addr constant {} \"{}\", align 1", self.name, aty.elem_ty.to_string(ctx), literal)
  }

}
