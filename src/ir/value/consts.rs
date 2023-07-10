use crate::{context::{Context, SlabEntry, component::GetSlabKey}, ir::types::{PointerType, StructType}};

use crate::ir::types::TypeRef;
use super::{ValueRef, instruction::InstOpcode, Instruction};

pub struct ConstScalarImpl {
  pub(crate) ty: TypeRef,
  pub(crate) value: u64
}

pub type ConstScalar = SlabEntry<ConstScalarImpl>;

pub struct UndefImpl {
  pub(crate) ty: TypeRef
}

pub type Undef = SlabEntry<UndefImpl>;

impl Undef {

  pub(crate) fn new(ty: TypeRef) -> Self {
    Self::from(UndefImpl { ty })
  }

  pub fn get_type(&self) -> &TypeRef {
    &self.instance.ty
  }

  pub fn to_string(&self, ctx: &Context) -> String {
    format!("undef {}", self.get_type().to_string(ctx))
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

pub type ConstArray = SlabEntry<ConstArrayImpl>;

impl ConstArray {

  pub fn new(name_prefix: String, ty: TypeRef, value: Vec<ValueRef>) -> Self {
    Self::from(ConstArrayImpl {
      name_prefix,
      ty,
      value
    })
  }

  pub fn get_name(&self) -> String {
    format!("{}.{}", self.instance.name_prefix, self.get_skey())
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

pub struct ConstExprImpl {
  pub(crate) inst: Instruction,
}

pub type ConstExpr = SlabEntry<ConstExprImpl>;

impl ConstExpr {

  pub fn new(ty: TypeRef, opcode: InstOpcode, operands: Vec<ValueRef>) -> Self {
    Self::from(ConstExprImpl {
      inst: Instruction::new(ty, opcode, "globalobj.".to_string(), operands)
    })
  }

  pub fn to_string(&self, ctx: &Context) -> String {
    let operands = self
      .instance
      .inst
      .instance
      .operands.iter().map(|x| x.to_string(ctx, true)).collect::<Vec<String>>().join(", ");
    match self.instance.inst.get_opcode() {
      InstOpcode::GetElementPtr(_) => {
        let ty = self
          .instance
          .inst
          .get_type();
        let ptr_scalar = self
          .instance
          .inst
          .instance
          .operands[0]
          .get_type(ctx)
          .as_ref::<PointerType>(ctx)
          .unwrap()
          .get_pointee_ty();
        let opcode = self
          .instance
          .inst
          .get_opcode();
        format!("{} {} ( {}, {} )", ty.to_string(ctx), opcode.to_string(), ptr_scalar.to_string(ctx) , operands)
      }
      _ => {
        panic!("ConstExpr::to_string: not a constant opcode {:?}", self.instance.inst.get_opcode().to_string());
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

impl ConstObject {

  pub fn new(name_prefix: String, ty: TypeRef, value: Vec<ValueRef>) -> Self {
    Self::from(ConstObjectImpl {
      name_prefix,
      ty,
      value
    })
  }

  pub fn get_name(&self) -> String {
    format!("{}.{}", self.instance.name_prefix, self.get_skey())
  }

  pub fn get_type(&self) -> &TypeRef {
    &self.instance.ty
  }

  pub fn to_string(&self, ctx: &Context) -> String {
    let pty = self.get_type().as_ref::<PointerType>(ctx).unwrap();
    let initializer = if self.instance.value.len() != 0 {
      format!("{{ {} }}", self.instance.value.iter().map(|x| x.to_string(ctx, true)).collect::<Vec<String>>().join(", "))
    } else {
      "zeroinitializer".to_string()
    };
    format!("@{} = dso_local global {} {}, align 8", self.get_name(), pty.get_pointee_ty().to_string(ctx), initializer)
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

impl InlineAsm {

  pub fn new(ty: TypeRef, sideeffect: bool, mnemonic: String, operands: String) -> Self {
    Self::from(InlineAsmImpl {
      ty,
      sideeffect,
      mnemonic,
      operands
    })
  }

  pub fn get_type(&self) -> &TypeRef {
    &self.instance.ty
  }

  pub fn get_sideeffect(&self) -> bool {
    self.instance.sideeffect
  }

  pub fn get_mnemonic(&self) -> &String {
    &self.instance.mnemonic
  }

  pub fn get_operands(&self) -> &String {
    &self.instance.operands
  }

  pub fn to_string(&self, ctx: &Context) -> String {
    let ty = if let Some(sty) = self.get_type().as_ref::<StructType>(ctx) {
      sty.instance.attrs.iter().map(|attr| attr.to_string(ctx)).collect::<Vec<_>>().join(", ")
    } else {
      self.get_type().to_string(ctx)
    };
    let sideeffect = if self.get_sideeffect() { "sideeffect" } else { "" };
    let mnemonic = str2display(self.get_mnemonic());
    let operands = str2display(self.get_operands());
    format!("{} asm {} \"{}\", \"{}\"", ty, sideeffect, mnemonic, operands)
  }
}

