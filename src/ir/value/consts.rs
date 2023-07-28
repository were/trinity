use crate::{context::{SlabEntry, Reference}, ir::types::{PointerType, StructType}};

use crate::ir::types::TypeRef;
use super::{ValueRef, instruction::{InstOpcode, InstructionRef}, Instruction};

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
    format!("{} = {}", self.instance().unwrap().ty.to_string(self.ctx), self.instance().unwrap().value)
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
  pub(crate) ty: TypeRef,
  pub(crate) value: Vec<ValueRef>
}

pub type ConstArray = SlabEntry<ConstArrayImpl>;
pub type ConstArrayRef<'ctx> = Reference<'ctx, ConstArrayImpl>;

impl ConstArray {

  pub fn new(name_prefix: String, ty: TypeRef, value: Vec<ValueRef>) -> Self {
    Self::from(ConstArrayImpl {
      name_prefix,
      ty,
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

  pub fn get_type(&self) -> &TypeRef {
    &self.instance().unwrap().ty
  }

  pub fn get_value(&self) -> &Vec<ValueRef> {
    &self.instance().unwrap().value
  }

  pub fn to_string(&self) -> String {
    if let Some(_) = self.is_invalid() {
      return self.get_name()
    }
    let ctx = self.ctx;
    let literal = self.get_value().iter().map(|x| x.to_string(ctx, true)).collect::<Vec<String>>().join(", ");
    let pty = self.get_type().as_ref::<PointerType>(ctx).unwrap();
    format!("@{} = private unnamed_addr constant {} [{}], align 1", self.get_name(), pty.get_pointee_ty().to_string(ctx), literal)
  }

}

pub struct ConstExprImpl {
  pub(crate) inst: Instruction,
}

pub type ConstExpr = SlabEntry<ConstExprImpl>;
pub type ConstExprRef<'ctx> = Reference<'ctx, ConstExprImpl>;

impl ConstExpr {

  pub fn new(ty: TypeRef, opcode: InstOpcode, operands: Vec<ValueRef>) -> Self {
    Self::from(ConstExprImpl {
      inst: Instruction::new(ty, opcode, "globalobj.".to_string(), operands)
    })
  }

}

impl <'ctx> ConstExprRef<'ctx> {

  pub fn get_inst(&self) -> &Instruction {
    &self.instance().unwrap().inst
  }

  pub fn to_string(&self) -> String {
    if let Some(skey) = self.is_invalid() {
      return format!("{{invalid.constexpr.{}}}", skey)
    }
    let ctx = self.ctx;
    let operands = self
      .instance()
      .unwrap()
      .inst
      .instance
      .operands
      .iter()
      .map(|x| x.to_string(ctx, true)).collect::<Vec<String>>().join(", ");
    // Wow, this instruction has no slab key!
    let inst = Reference::new(ctx, &self.instance().unwrap().inst);
    match inst.get_opcode() {
      InstOpcode::GetElementPtr(_) => {
        let ty = inst.get_type();
        let ptr_inst = &self.instance().unwrap().inst.instance;
        let ptr_ty = ptr_inst.operands[0].get_type(ctx);
        let ptr_ty = ptr_ty.as_ref::<PointerType>(ctx).unwrap();
        let ptr_scalar = ptr_ty.get_pointee_ty();
        let opcode = inst.get_opcode();
        format!("{} {} ( {}, {} )", ty.to_string(ctx), opcode.to_string(), ptr_scalar.to_string(ctx) , operands)
      }
      _ => {
        panic!("ConstExpr::to_string: not a constant opcode {:?}", inst.get_opcode().to_string());
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

  pub fn get_type(&self) -> &TypeRef {
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
    let pty = self.get_type().as_ref::<PointerType>(ctx).unwrap();
    let initializer = if self.instance().unwrap().value.len() != 0 {
      format!("{{ {} }}", self.instance().unwrap().value.iter().map(|x| x.to_string(ctx, true)).collect::<Vec<String>>().join(", "))
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
      sty.instance().unwrap().attrs.iter().map(|attr| attr.to_string(ctx)).collect::<Vec<_>>().join(", ")
    } else {
      self.get_type().to_string(ctx)
    };
    let sideeffect = if self.get_sideeffect() { "sideeffect" } else { "" };
    let mnemonic = str2display(self.get_mnemonic());
    let operands = str2display(self.get_operands());
    format!("{} asm {} \"{}\", \"{}\"", ty, sideeffect, mnemonic, operands)
  }
}

