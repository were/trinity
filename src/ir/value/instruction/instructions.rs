use crate::{context::Context, ir::{PointerType, ValueRef, value::instruction::InstOpcode}};

use super::Instruction;

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
    let ptr_str = ptr_ty.get_scalar_ty().to_string(ctx);
    return format!("%{} = alloca {}, align {}", self.inst.name, ptr_str, self.align);
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

  pub fn get_ptr(&self) -> ValueRef {
    self.inst.operands[0].clone()
  }

  pub fn to_string(&self, ctx: &Context) -> String {
    let inst = ctx.get_value_ref::<Instruction>(self.inst.skey.unwrap());
    format!("%{} = {} load {}, align {}", inst.get_name(), inst.to_string(ctx), self.get_ptr().to_string(ctx), self.align)
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
  
  pub fn get_value(&self) -> ValueRef {
    self.inst.operands[0].clone()
  }

  pub fn get_ptr(&self) -> ValueRef {
    self.inst.operands[1].clone()
  }

  pub fn to_string(&self, ctx: &Context) -> String {
    format!("store {}, {}, align {}", self.get_value().to_string(ctx), self.get_ptr().to_string(ctx), self.align)
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
    // Embrace the legacy!
    let ty_str = self.inst.ty.to_string(ctx);
    let operands = (0..self.inst.get_num_operands()).map(|i| {
      format!("{}", &self.inst.get_operand(i).to_string(ctx))
    }).collect::<Vec<_>>().join(", ");
    format!("  %{} = getelementptr {} {} {}", self.inst.name, inbounds, ty_str, operands)
  }

}

/// Call a callable value.
pub struct Call<'inst> {
  pub(super) base: &'inst Instruction,
}

impl <'inst> Call<'inst> {

  pub fn new(inst: &'inst Instruction) -> Self {
    if let InstOpcode::Call = inst.opcode {
      Self { base: inst, }
    } else {
      panic!("Invalid opcode for Call instruction.");
    }
  }

  pub fn get_callee(&self) -> ValueRef {
    self.base.operands.last().unwrap().clone()
  }

  pub fn get_num_args(&self) -> usize {
    self.base.operands.len() - 1
  }

  pub fn get_arg(&self, idx: usize) -> ValueRef {
    self.base.operands[idx].clone()
  }

  pub fn to_string(&self, ctx: &Context) -> String {
    let callee = self.get_callee();
    let args_str = (0..self.get_num_args()).map(|i| {
      self.get_arg(i).to_string(ctx)
    }).collect::<Vec<_>>().join(", ");
    format!("call {} {}({})", self.base.ty.to_string(ctx), callee.to_string(ctx), args_str)
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
      Some(val) => format!("ret {}", val.to_string(ctx)),
      None => String::from("ret void")
    }
  }

}

