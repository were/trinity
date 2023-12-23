use crate::{context::{SlabEntry, Reference}, ir::types::StructType};

use crate::ir::types::TypeRef;
use super::{ValueRef, instruction::InstOpcode};

pub struct ConstScalarImpl {
  pub(crate) ty: TypeRef,
  pub(crate) value: u64
}

pub type ConstScalar = SlabEntry<ConstScalarImpl>;
pub type ConstScalarRef<'ctx> = Reference<'ctx, ConstScalarImpl>;

pub struct UndefImpl {
  pub(crate) ty: TypeRef
}

pub type Undef = SlabEntry<UndefImpl>;
pub type UndefRef<'ctx> = Reference<'ctx, UndefImpl>;

impl Undef {

  pub(crate) fn new(ty: TypeRef) -> Self {
    Self::from(UndefImpl { ty })
  }

}

impl <'ctx> UndefRef<'ctx> {

  pub fn get_type(&self) -> &TypeRef {
    &self.instance().unwrap().ty
  }

  pub fn to_string(&self) -> String {
    format!("undef {}", self.get_type().to_string(self.ctx))
  }

}

fn str2display(s: &String) -> String {
  s.chars().map(|c| {
    if ('\x20'..'\x7e').contains(&c) {
      c.to_string()
    } else {
      format!("\\{:02x}", c as u8)
    }
  }).collect::<Vec<String>>().join("")
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

}

impl <'ctx>ConstScalarRef<'ctx> {

  pub fn to_string(&self) -> String {
    if let Some(skey) = self.is_invalid() {
      return format!("{{invalid.constscalar.{}}}", skey)
    }
    let lhs = self.instance().unwrap().ty.to_string(self.ctx);
    let rhs = self.instance().unwrap().value;
    format!("{} = {}", lhs, rhs)
  }

  pub fn get_value(&self) -> u64 {
    self.instance().unwrap().value
  }

  pub fn get_type(&self) -> &TypeRef {
    &self.instance().unwrap().ty
  }

}

pub struct ConstArrayImpl {
  pub(crate) name_prefix: String,
  pub(crate) scalar_ty: TypeRef,
  pub(crate) value: Vec<ValueRef>
}

pub type ConstArray = SlabEntry<ConstArrayImpl>;
pub type ConstArrayRef<'ctx> = Reference<'ctx, ConstArrayImpl>;

impl ConstArray {

  pub fn new(name_prefix: String, scalar_ty: TypeRef, value: Vec<ValueRef>) -> Self {
    Self::from(ConstArrayImpl {
      name_prefix,
      scalar_ty,
      value
    })
  }

}

impl <'ctx> ConstArrayRef<'ctx> {

  pub fn get_name(&self) -> String {
    if let Some(skey) = self.is_invalid() {
      return format!("invalid.constarray.{}", skey)
    }
    format!("{}.{}", self.instance().unwrap().name_prefix, self.get_skey())
  }

  pub fn get_scalar_type(&self) -> &TypeRef {
    &self.instance().unwrap().scalar_ty
  }

  pub fn get_value(&self) -> &Vec<ValueRef> {
    &self.instance().unwrap().value
  }

  pub fn to_string(&self) -> String {
    if let Some(_) = self.is_invalid() {
      return self.get_name()
    }
    let ctx = self.ctx;
    let raw = self.get_value().iter().map(|x| x.to_string(ctx, true)).collect::<Vec<String>>();
    let literal = raw.join(", ");
    format!("@{} = private unnamed_addr constant {} [{}], align 1",
            self.get_name(), self.get_scalar_type().to_string(ctx), literal)
  }

}

pub struct ConstExprImpl {
  pub(crate) ty: TypeRef,
  pub(crate) opcode: InstOpcode,
  pub(crate) operands: Vec<ValueRef>
}

pub type ConstExpr = SlabEntry<ConstExprImpl>;
pub type ConstExprRef<'ctx> = Reference<'ctx, ConstExprImpl>;

impl ConstExpr {

  pub fn new(ty: TypeRef, opcode: InstOpcode, operands: Vec<ValueRef>) -> Self {
    Self::from(ConstExprImpl { ty, opcode, operands })
  }

}

impl <'ctx> ConstExprRef<'ctx> {

  pub fn get_opcode(&self) -> InstOpcode {
    self.instance().unwrap().opcode.clone()
  }

  pub fn get_operand(&self, idx: usize) -> Option<ValueRef> {
    self.instance().unwrap().operands.get(idx).map(|v| v.clone())
  }

  pub fn to_string(&self) -> String {
    if let Some(skey) = self.is_invalid() {
      return format!("{{invalid.constexpr.{}}}", skey)
    }
    let ctx = self.ctx;
    let instance = self.instance().unwrap();
    let operands = instance
      .operands
      .iter()
      .map(|x| x.to_string(ctx, true)).collect::<Vec<String>>().join(", ");
    let opcode = &instance.opcode;
    match opcode {
      InstOpcode::GetElementPtr((pointee, _)) => {
        format!("{} {} ( ptr {} )",
                pointee.to_string(ctx), opcode.to_string(), operands)
      }
      _ => {
        panic!("ConstExpr::to_string: not a constant opcode {:?}", opcode.to_string());
      }
    }
  }

}

pub struct ConstObjectImpl {
  pub(crate) name_prefix: String,
  pub(crate) ty: TypeRef,
  pub(crate) value: Vec<ValueRef>
}

pub type ConstObject = SlabEntry<ConstObjectImpl>;
pub type ConstObjectRef<'ctx> = Reference<'ctx, ConstObjectImpl>;

impl ConstObject {

  pub fn new(name_prefix: String, ty: TypeRef, value: Vec<ValueRef>) -> Self {
    Self::from(ConstObjectImpl {
      name_prefix,
      ty,
      value
    })
  }

}

impl <'ctx> ConstObjectRef<'ctx> {

  pub fn get_name(&self) -> String {
    if let Some(skey) = self.is_invalid() {
      return format!("{{invalid.constobj.{}}}", skey);
    }
    format!("{}.{}", self.instance().unwrap().name_prefix, self.get_skey())
  }

  pub fn get_value_type(&self) -> &TypeRef {
    &self.instance().unwrap().ty
  }

  pub fn get_value(&self) -> &Vec<ValueRef> {
    &self.instance().unwrap().value
  }

  pub fn to_string(&self) -> String {
    if let Some(_) = self.is_invalid() {
      return self.get_name();
    }
    let ctx = self.ctx;
    let literal = self
      .instance()
      .unwrap()
      .value
      .iter()
      .map(|x| x.to_string(ctx, true))
      .collect::<Vec<String>>()
      .join(", ");
    let initializer = if self.instance().unwrap().value.len() != 0 {
      format!("{{ {} }}", literal)
    } else {
      "zeroinitializer".to_string()
    };
    let ty = self.get_value_type().to_string(ctx);
    format!("@{} = dso_local global {} {}, align 8", self.get_name(), ty, initializer)
  }

}

/// Inline assembly
pub struct InlineAsmImpl {
  pub(crate) ty: TypeRef,
  pub(crate) sideeffect: bool,
  pub(crate) mnemonic: String,
  /// Operand constraints
  pub(crate) operands: String,
}

pub type InlineAsm = SlabEntry<InlineAsmImpl>;
pub type InlineAsmRef<'ctx> = Reference<'ctx, InlineAsmImpl>;

impl InlineAsm {

  pub fn new(ty: TypeRef, sideeffect: bool, mnemonic: String, operands: String) -> Self {
    Self::from(InlineAsmImpl {
      ty,
      sideeffect,
      mnemonic,
      operands
    })
  }

}

impl <'ctx> InlineAsmRef<'ctx> {

  pub fn get_type(&self) -> &TypeRef {
    &self.instance().unwrap().ty
  }

  pub fn get_sideeffect(&self) -> bool {
    self.instance().unwrap().sideeffect
  }

  pub fn get_mnemonic(&self) -> &String {
    &self.instance().unwrap().mnemonic
  }

  pub fn get_operands(&self) -> &String {
    &self.instance().unwrap().operands
  }

  pub fn to_string(&self) -> String {
    if let Some(skey) = self.is_invalid() {
      return format!("{{invalid.inlineasm.{}}}", skey);
    }
    let ctx = self.ctx;
    let ty = if let Some(sty) = self.get_type().as_ref::<StructType>(ctx) {
      sty
        .instance()
        .unwrap()
        .attrs
        .iter()
        .map(|attr| attr.to_string(ctx))
        .collect::<Vec<_>>()
        .join(", ")
    } else {
      self.get_type().to_string(ctx)
    };
    let sideeffect = if self.get_sideeffect() { "sideeffect" } else { "" };
    let mnemonic = str2display(self.get_mnemonic());
    let operands = str2display(self.get_operands());
    format!("{} asm {} \"{}\", \"{}\"", ty, sideeffect, mnemonic, operands)
  }
}

