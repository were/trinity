use crate::{context::Context, ir::{PointerType, ValueRef, value::instruction::InstOpcode, VoidType, TypeRef}};

use super::{Instruction, CmpPred};

/// Stack memory allocation.
pub struct Alloca<'inst> {
  inst: &'inst Instruction,
  align: usize,
}

impl<'inst> Alloca <'inst> {

  pub fn new(inst: &'inst Instruction, align: usize) -> Self {
    if let InstOpcode::Alloca(_) = inst.opcode {
      Self { inst, align, }
    } else {
      panic!("Invalid opcode for Alloca instruction.");
    }
  }

  pub fn to_string(&self, ctx: &Context) -> String {
    let ptr_ty = self.inst.ty.as_ref::<PointerType>(ctx).unwrap();
    let ptr_str = ptr_ty.get_pointee_ty().to_string(ctx);
    return format!("%{} = alloca {}, align {}", self.inst.get_name(), ptr_str, self.align);
  }

}

/// Load instruction.
pub struct Load<'inst> {
  inst: &'inst Instruction,
  align: usize,
}

impl <'inst> Load <'inst> {

  pub fn new(inst: &'inst Instruction, align: usize) -> Self {
    if let InstOpcode::Load(_) = inst.opcode {
      Self { inst, align, }
    } else {
      panic!("Invalid opcode for Load instruction.");
    }
  }

  pub fn get_ptr(&self) -> &ValueRef {
    &self.inst.operands[0]
  }

  pub fn to_string(&self, ctx: &Context) -> String {
    let inst = ctx.get_value_ref::<Instruction>(self.inst.skey.unwrap());
    format!("%{} = load {}, {}, align {}",
      inst.get_name(), inst.ty.to_string(ctx),
      self.get_ptr().to_string(ctx, true), self.align)
  }

}


/// Store instruction.
pub struct Store<'inst> {
  inst: &'inst Instruction,
  align: usize,
}

impl <'inst> Store <'inst> {

  pub fn new(inst: &'inst Instruction, align: usize) -> Self {
    if let InstOpcode::Store(_) = inst.opcode {
      Self { inst, align, }
    } else {
      panic!("Invalid opcode for Store instruction.");
    }
  }
  
  pub fn get_value(&self) -> &ValueRef {
    &self.inst.operands[0]
  }

  pub fn get_ptr(&self) -> &ValueRef {
    &self.inst.operands[1]
  }

  pub fn to_string(&self, ctx: &Context) -> String {
    format!("store {}, {}, align {}", self.get_value().to_string(ctx, true), self.get_ptr().to_string(ctx, true), self.align)
  }

}

/// GetElementPtr instruction.
pub struct GetElementPtr<'inst> {
  inst: &'inst Instruction,
  inbounds: bool,
}

impl <'inst>GetElementPtr<'inst> {

  pub fn new(inst: &'inst Instruction, inbounds: bool) -> Self {
    if let InstOpcode::GetElementPtr(_) = inst.opcode {
      Self { inst, inbounds, }
    } else {
      panic!("Invalid opcode for GetElementPtr instruction.");
    }
  }

  pub fn to_string(&self, ctx: &Context) -> String {
    let inbounds = if self.inbounds {
      "inbounds"
    } else {
      ""
    };
    // TODO(@were): What if this is not a pointer?
    let ptr_scalar = self.inst.operands[0].get_type(ctx).as_ref::<PointerType>(ctx).unwrap().get_pointee_ty();
    let ty_str = ptr_scalar.to_string(ctx);

    let operands = (0..self.inst.get_num_operands()).map(|i| {
      format!("{}", &self.inst.get_operand(i).to_string(ctx, true))
    }).collect::<Vec<_>>().join(", ");
    format!("%{} = getelementptr {} {}, {}", self.inst.get_name(), inbounds, ty_str, operands)
  }

}

/// Call a callable value.
pub struct Call<'inst> {
  pub(super) inst: &'inst Instruction,
}

impl <'inst> Call<'inst> {

  pub fn new(inst: &'inst Instruction) -> Self {
    if let InstOpcode::Call = inst.opcode {
      Self { inst, }
    } else {
      panic!("Invalid opcode for Call instruction.");
    }
  }

  pub fn get_callee(&self) -> &ValueRef {
    self.inst.operands.last().unwrap()
  }

  pub fn get_num_args(&self) -> usize {
    self.inst.operands.len() - 1
  }

  pub fn get_arg(&self, idx: usize) -> &ValueRef {
    &self.inst.operands[idx]
  }

  pub fn to_string(&self, ctx: &Context) -> String {
    let callee = self.get_callee();
    let args_str = (0..self.get_num_args()).map(|i| {
      self.get_arg(i).to_string(ctx, true)
    }).collect::<Vec<_>>().join(", ");
    if let None = self.inst.get_type().as_ref::<VoidType>(ctx) {
      format!("%{} = call {}({})", self.inst.get_name(), callee.to_string(ctx, true), args_str)
    } else {
      format!("call {}({})", callee.to_string(ctx, true), args_str)
    }
  }

}

/// Call a callable value.
pub struct Return <'inst> {
  pub(super) base: &'inst Instruction,
}

impl <'inst> Return <'inst> {

  pub fn new(inst: &'inst Instruction) -> Self {
    if let InstOpcode::Return = inst.opcode {
      Self { base: inst, }
    } else {
      panic!("Invalid opcode for Return instruction.");
    }
  }

  pub fn get_ret_val(&self) -> Option<&ValueRef> {
    self.base.operands.get(0)
  }

  pub fn to_string(&self, ctx: &Context) -> String {
    match self.get_ret_val() {
      Some(val) => format!("ret {}", val.to_string(ctx, true)),
      None => String::from("ret void")
    }
  }

}

/// Binary operation.
pub struct BinaryInst <'inst> {
  pub(super) base: &'inst Instruction,
}


impl<'inst> BinaryInst <'inst> {

  pub fn new(inst: &'inst Instruction) -> Self {
    if let InstOpcode::BinaryOp(_) = inst.opcode {
      Self { base: inst, }
    } else {
      panic!("Invalid opcode for BinaryOp instruction.");
    }
  }

  pub fn to_string(&self, ctx: &Context) -> String {
    let lhs = self.lhs();
    let rhs = self.rhs();
    let op = self.base.opcode.to_string();
    let ty = self.base.ty.to_string(ctx);
    format!("%{} = {} {} {}, {}", self.base.get_name(), op, ty, lhs.to_string(ctx, false), rhs.to_string(ctx, false))
  }

  pub fn lhs(&self) -> &ValueRef {
    &self.base.operands[0]
  }

  pub fn rhs(&self) -> &ValueRef {
    &self.base.operands[1]
  }

}

pub struct CastInst <'inst> {
  pub(super) base: &'inst Instruction,
}

impl<'inst> CastInst <'inst> {

  pub fn new(inst: &'inst Instruction) -> Self {
    if let InstOpcode::CastInst(_) = inst.opcode {
      Self { base: inst }
    } else {
      panic!("Not a cast instruction!");
    }
  }

  pub fn dest_ty(&self) -> TypeRef {
    return self.base.ty.clone()
  }

  pub fn to_string(&self, ctx: &Context) -> String {
    let operand = self.base.operands[0].to_string(ctx, true);
    let dest_type = self.dest_ty().to_string(ctx);
    format!("%{} = {} {} to {}", self.base.get_name(), self.base.opcode.to_string(), operand, dest_type)
  }

}

pub struct CompareInst <'inst> {
  pub(super) base: &'inst Instruction,
}

impl <'inst> CompareInst<'inst> {

  pub fn new(inst: &'inst Instruction) -> Self {
    if let InstOpcode::ICompare(_) = inst.opcode {
      CompareInst { base: inst }
    } else {
      panic!("Invalid opcode!")
    }
  }

  pub fn get_pred(&self) -> &CmpPred {
    match self.base.get_opcode() {
      InstOpcode::ICompare(x) => x,
      _ => { panic!("Invalid opcode!") }
    }
  }

  pub fn to_string(&self, ctx: &Context) -> String {
    let opcode = self.base.opcode.to_string();
    let pred = self.get_pred().to_string();
    let lhs = self.base.operands.get(0).unwrap();
    let ty = lhs.get_type(ctx).to_string(ctx);
    let lhs = lhs.to_string(ctx, false);
    let res = format!("%{} = {} {} {} {}", self.base.get_name(), opcode, pred, ty, lhs);
    if let Some(rhs) = self.base.operands.get(1) {
      let rhs = rhs.to_string(ctx, false);
      format!("{}, {}", res, rhs)
    } else {
      res
    }
  }

}

pub struct BranchInst<'inst> {
  pub(super) base: &'inst Instruction
}

impl <'inst> BranchInst <'inst> {

  pub fn new(inst: &'inst Instruction) -> Self {
    if let InstOpcode::Branch = inst.opcode {
      BranchInst { base: inst }
    } else {
      panic!("Invalid opcode!")
    }
  }

  pub fn is_cond_br(&self) -> bool {
    self.base.get_num_operands() == 3
  }

  pub fn cond(&self) -> Option<&ValueRef> {
    assert!(self.is_cond_br());
    self.base.operands.get(0)
  }

  pub fn true_label(&self) -> Option<&ValueRef> {
    assert!(self.is_cond_br());
    self.base.operands.get(1)
  }

  pub fn false_label(&self) -> Option<&ValueRef> {
    assert!(self.is_cond_br());
    self.base.operands.get(2)
  }

  pub fn dest_label(&self) -> Option<&ValueRef> {
    assert!(!self.is_cond_br());
    self.base.operands.get(0)
  }

  pub fn to_string(&self, ctx: &Context) -> String {
    if self.base.get_num_operands() == 3 {
      let cond = self.base.operands.get(0).unwrap();
      let cond = cond.to_string(ctx, true);
      let true_label = self.base.operands.get(1).unwrap();
      let true_label = true_label.to_string(ctx, false);
      let false_label = self.base.operands.get(2).unwrap();
      let false_label = false_label.to_string(ctx, false);
      format!("br {}, label {}, label {}", cond, true_label, false_label)
    } else {
      format!("br label {}", self.base.operands.get(0).unwrap().to_string(ctx, false))
    }
  }
}
