use crate::{context::Context, ir::types::PointerType};

use super::{types::TypeRef, value::ValueRef, instruction::InstOpcode};

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
    let literal = self.value.iter().map(|x| format!("\\{:02x}", x)).collect::<Vec<String>>().join("");
    let pty = self.ty.as_ref::<PointerType>(ctx).unwrap();
    format!("@{} = private unnamed_addr constant {} c\"{}\", align 1", self.name, pty.get_scalar_ty().to_string(ctx), literal)
  }

}

pub struct ConstExpr {
  pub(crate) skey: Option<usize>,
  pub(crate) ty: TypeRef,
  pub(crate) opcode: InstOpcode,
  pub(crate) operands: Vec<ValueRef>,
}

impl ConstExpr {

  pub fn to_string(&self, ctx: &Context) -> String {
    let operands = self.operands.iter().map(|x| x.to_string(ctx)).collect::<Vec<String>>().join(", ");
    format!("{} {} {}", self.opcode.to_string(), self.ty.to_string(ctx), operands)
  }

}
