use crate::{context::Context, ir::types::PointerType};

use crate::ir::types::TypeRef;
use super::{ValueRef, instruction::InstOpcode};

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
    match self.opcode {
      InstOpcode::GetElementPtr(_) => {
        let ptr_scalar = self.operands[0].get_type(ctx).as_ref::<PointerType>(ctx).unwrap().get_scalar_ty();
        format!("{} {} ( {}, {} )", self.ty.to_string(ctx), self.opcode.to_string(), ptr_scalar.to_string(ctx) , operands)
      }
      _ => {
        panic!("ConstExpr::to_string: not a constant opcode {:?}", self.opcode.to_string());
      }
    }
  }

}

pub struct ConstObject {
  pub(crate) skey: Option<usize>,
  pub(crate) name: String,
  pub(crate) ty: TypeRef,
  pub(crate) value: Vec<ValueRef>
}

impl ConstObject {

  pub fn to_string(&self, ctx: &Context) -> String {
    let pty = self.ty.as_ref::<PointerType>(ctx).unwrap();
    let initializer = if self.value.len() != 0 {
      format!("{{ {} }}", self.value.iter().map(|x| x.to_string(ctx)).collect::<Vec<String>>().join(", "))
    } else {
      "zeroinitializer".to_string()
    };
    format!("@{} = dso_local global {} {}, align 8", self.name, pty.get_scalar_ty().to_string(ctx), initializer)
  }

}

/// Inline assembly
pub struct InlineAsm {
  pub(crate) skey: Option<usize>,
  pub(crate) ty: TypeRef,
  pub(crate) mnemonic: String,
  pub(crate) operands: String
}

