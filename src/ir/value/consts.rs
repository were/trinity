use crate::{context::Context, ir::types::{PointerType, StructType}};

use crate::ir::types::TypeRef;
use super::{ValueRef, instruction::InstOpcode};

pub struct ConstScalar {
  pub(crate) skey: Option<usize>,
  pub(crate) ty: TypeRef,
  pub(crate) value: u64
}

fn str2display(s: &String) -> String {
  s.chars().map(|c| if ('\x20'..'\x7e').contains(&c) { c.to_string() } else { format!("\\{:02x}", c as u8) }).collect::<Vec<String>>().join("")
}

impl ConstScalar {

  pub(crate) fn new(ty: TypeRef, value: u64) -> Self {
    Self {
      skey: None,
      ty,
      value
    }
  }

  pub fn to_string(&self, ctx: &Context) -> String {
    format!("{} = {}", self.ty.to_string(ctx), self.value)
  }

}

pub struct ConstArray {
  pub(crate) skey: Option<usize>,
  pub(crate) name: String,
  pub(crate) ty: TypeRef,
  pub(crate) value: Vec<ValueRef>
}

impl ConstArray {

  pub fn to_string(&self, ctx: &Context) -> String {
    let literal = self.value.iter().map(|x| x.to_string(ctx, true)).collect::<Vec<String>>().join(", ");
    let pty = self.ty.as_ref::<PointerType>(ctx).unwrap();
    format!("@{} = private unnamed_addr constant {} [{}], align 1", self.name, pty.get_pointee_ty().to_string(ctx), literal)
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
    let operands = self.operands.iter().map(|x| x.to_string(ctx, true)).collect::<Vec<String>>().join(", ");
    match self.opcode {
      InstOpcode::GetElementPtr(_) => {
        let ptr_scalar = self.operands[0].get_type(ctx).as_ref::<PointerType>(ctx).unwrap().get_pointee_ty();
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
      format!("{{ {} }}", self.value.iter().map(|x| x.to_string(ctx, true)).collect::<Vec<String>>().join(", "))
    } else {
      "zeroinitializer".to_string()
    };
    format!("@{} = dso_local global {} {}, align 8", self.name, pty.get_pointee_ty().to_string(ctx), initializer)
  }

}

/// Inline assembly
pub struct InlineAsm {
  pub(crate) skey: Option<usize>,
  pub(crate) ty: TypeRef,
  pub(crate) sideeffect: bool,
  pub(crate) mnemonic: String,
  pub(crate) operands: String,
}

impl InlineAsm {
  pub fn to_string(&self, ctx: &Context) -> String {
    let ty = if let Some(sty) = self.ty.as_ref::<StructType>(ctx) {
      sty.attrs.iter().map(|attr| attr.to_string(ctx)).collect::<Vec<_>>().join(", ")
    } else {
      self.ty.to_string(ctx)
    };
    let sideeffect = if self.sideeffect { "sideeffect" } else { "" };
    let mnemonic = str2display(&self.mnemonic);
    let operands = str2display(&self.operands);
    format!("{} asm {} \"{}\", \"{}\"", ty, sideeffect, mnemonic, operands)
  }
}

