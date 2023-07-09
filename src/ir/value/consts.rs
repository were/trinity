use crate::{context::{Context, Ptr}, ir::types::{PointerType, StructType}};

use crate::ir::types::TypeRef;
use super::{ValueRef, instruction::InstOpcode};

pub struct ConstScalarImpl {
  pub(crate) ty: TypeRef,
  pub(crate) value: u64
}

pub type ConstScalar = Ptr<ConstScalarImpl>;

pub struct Undef {
  pub(crate) skey: Option<usize>,
  pub(crate) ty: TypeRef
}

impl Undef {

  pub(crate) fn new(ty: TypeRef) -> Self {
    Self {
      skey: None,
      ty
    }
  }

  pub fn to_string(&self, ctx: &Context) -> String {
    format!("undef {}", self.ty.to_string(ctx))
  }

}

fn str2display(s: &String) -> String {
  s.chars().map(|c| if ('\x20'..'\x7e').contains(&c) { c.to_string() } else { format!("\\{:02x}", c as u8) }).collect::<Vec<String>>().join("")
}

impl ConstScalarImpl {
  pub(crate) fn new(ty: TypeRef, value: u64) -> Self {
    Self {
      ty,
      value
    }
  }

}

impl ConstScalar {

  pub(crate) fn new(ty: TypeRef, value: u64) -> Self {
    ConstScalar::from(ConstScalarImpl::new(ty, value))
  }

  pub fn to_string(&self, ctx: &Context) -> String {
    format!("{} = {}", self.instance.ty.to_string(ctx), self.instance.value)
  }

  pub fn get_value(&self) -> u64 {
    self.instance.value
  }

  pub fn get_type(&self) -> &TypeRef {
    &self.instance.ty
  }

}

pub struct ConstArrayImpl {
  pub(crate) name_prefix: String,
  pub(crate) ty: TypeRef,
  pub(crate) value: Vec<ValueRef>
}

pub type ConstArray = Ptr<ConstArrayImpl>;

impl ConstArray {

  pub fn new(name_prefix: String, ty: TypeRef, value: Vec<ValueRef>) -> Self {
    Self::from(ConstArrayImpl {
      name_prefix,
      ty,
      value
    })
  }

  pub fn get_name(&self) -> String {
    format!("{}.{}", self.instance.name_prefix, self.get_ptr())
  }

  pub fn get_type(&self) -> &TypeRef {
    &self.instance.ty
  }

  pub fn get_value(&self) -> &Vec<ValueRef> {
    &self.instance.value
  }

  pub fn to_string(&self, ctx: &Context) -> String {
    let literal = self.get_value().iter().map(|x| x.to_string(ctx, true)).collect::<Vec<String>>().join(", ");
    let pty = self.get_type().as_ref::<PointerType>(ctx).unwrap();
    format!("@{} = private unnamed_addr constant {} [{}], align 1", self.get_name(), pty.get_pointee_ty().to_string(ctx), literal)
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
  pub(crate) name_prefix: String,
  pub(crate) ty: TypeRef,
  pub(crate) value: Vec<ValueRef>
}

impl ConstObject {

  pub fn get_name(&self) -> String {
    format!("{}.{}", self.name_prefix, self.skey.unwrap())
  }

  pub fn to_string(&self, ctx: &Context) -> String {
    let pty = self.ty.as_ref::<PointerType>(ctx).unwrap();
    let initializer = if self.value.len() != 0 {
      format!("{{ {} }}", self.value.iter().map(|x| x.to_string(ctx, true)).collect::<Vec<String>>().join(", "))
    } else {
      "zeroinitializer".to_string()
    };
    format!("@{} = dso_local global {} {}, align 8", self.get_name(), pty.get_pointee_ty().to_string(ctx), initializer)
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

