use crate::ir::{PointerType, ValueRef, value::instruction::InstOpcode, VoidType, TypeRef};

use super::{CmpPred, InstructionRef, BinaryOp};

/// Stack memory allocation.
pub struct Alloca<'inst> {
  inst: &'inst InstructionRef<'inst>,
}

pub trait SubInst<'inst, T> {

  fn new(inst: &'inst InstructionRef<'inst>) -> Option<T>;

  fn to_string(&self) -> String;
}

macro_rules! impl_sub_inst {
  ($pat: pat, $ty: tt, $to_string: item) => {

    impl <'inst> SubInst <'inst, $ty<'inst>> for $ty<'inst> {

      fn new(inst: &'inst InstructionRef<'inst>) -> Option<$ty<'inst>> {
        if let $pat = inst.get_opcode() {
          Some($ty { inst, })
        } else {
          None
        }
      }

      $to_string

    }

  };
}

impl_sub_inst!(InstOpcode::Alloca(_), Alloca, 

  fn to_string(&self) -> String {
    let ctx = self.inst.ctx;
    let ptr_ty = self.inst.get_type().as_ref::<PointerType>(ctx).unwrap();
    let ptr_str = ptr_ty.get_pointee_ty().to_string(ctx);
    return format!("%{} = alloca {}, align {}", self.inst.get_name(), ptr_str, self.get_align());
  }

);

impl_sub_inst!(InstOpcode::Load(_), Load,

  fn to_string(&self) -> String {
    let ctx = self.inst.ctx;
    format!("%{} = load {}, {}, align {}",
      self.inst.get_name(), self.inst.get_type().to_string(ctx),
      self.get_ptr().to_string(ctx, true), self.get_align())
  }

);

impl_sub_inst!(InstOpcode::Store(_), Store, 

  fn to_string(&self) -> String {
    let ctx = self.inst.ctx;
    format!("store {}, {}, align {}", self.get_value().to_string(ctx, true), self.get_ptr().to_string(ctx, true), self.get_align())
  }

);

impl_sub_inst!(InstOpcode::GetElementPtr(_), GetElementPtr,

  fn to_string(&self) -> String {
    let ctx = self.inst.ctx;
    let inbounds = if let InstOpcode::GetElementPtr(inbounds) = self.inst.get_opcode() {
      if *inbounds { "inbounds" } else { "" }
    } else {
      ""
    };
    // TODO(@were): What if this is not a pointer?
    let ptr_ty = self.inst.get_operand(0).unwrap().get_type(ctx);
    let ptr_ty = ptr_ty.as_ref::<PointerType>(ctx).unwrap();
    let ty_str = ptr_ty.get_pointee_ty().to_string(ctx);

    let operands = (0..self.inst.get_num_operands()).map(|i| {
      format!("{}", &self.inst.get_operand(i).unwrap().to_string(ctx, true))
    }).collect::<Vec<_>>().join(", ");
    format!("%{} = getelementptr {} {}, {}", self.inst.get_name(), inbounds, ty_str, operands)
  }
);

impl_sub_inst!(InstOpcode::Call, Call,

  fn to_string(&self) -> String {
    let ctx = self.inst.ctx;
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

);

impl_sub_inst!(InstOpcode::Return, Return,

  fn to_string(&self) -> String {
    let ctx = self.inst.ctx;
    match self.get_ret_val() {
      Some(val) => format!("ret {}", val.to_string(ctx, true)),
      None => String::from("ret void")
    }
  }

);

impl_sub_inst!(InstOpcode::Phi, PhiNode,
  fn to_string(&self) -> String {
    let ctx = self.inst.ctx;
    let ty = self.inst.get_type().to_string(ctx);
    let mut res = format!("%{} = phi {} ", self.inst.get_name(), ty);
    for i in (0..self.inst.get_num_operands()).step_by(2) {
      if i != 0 {
        res.push_str(", ");
      }
      let operand = self.inst.get_operand(i).unwrap();
      res.push_str(&format!("[ {}, ", operand.to_string(ctx, false)));
      let operand = self.inst.get_operand(i + 1).unwrap();
      res.push_str(&format!("{} ]", operand.to_string(ctx, false)));
    }
    res
  }
);

impl_sub_inst!(InstOpcode::Branch, BranchInst,

  fn to_string(&self) -> String {
    let ctx = self.inst.ctx;
    if self.inst.get_num_operands() == 3 {
      let cond = self.inst.get_operand(0).unwrap();
      let cond = cond.to_string(ctx, true);
      let true_label = self.inst.get_operand(1).unwrap();
      let true_label = true_label.to_string(ctx, false);
      let false_label = self.inst.get_operand(2).unwrap();
      let false_label = false_label.to_string(ctx, false);
      format!("br {}, label {}, label {}", cond, true_label, false_label)
    } else {
      format!("br label {}", self.inst.get_operand(0).unwrap().to_string(ctx, false))
    }
  }

);

impl_sub_inst!(InstOpcode::BinaryOp(_), BinaryInst,

  fn to_string(&self) -> String {
    let ctx = self.inst.ctx;
    let lhs = self.lhs();
    let rhs = self.rhs();
    let op = self.inst.get_opcode().to_string();
    let ty = self.inst.get_type().to_string(ctx);
    format!("%{} = {} {} {}, {}", self.inst.get_name(), op, ty, lhs.to_string(ctx, false), rhs.to_string(ctx, false))
  }


);

impl_sub_inst!(InstOpcode::CastInst(_), CastInst,

  fn to_string(&self) -> String {
    let ctx = self.inst.ctx;
    let operand = self.inst.get_operand(0).unwrap().to_string(ctx, true);
    let dest_type = self.dest_ty().to_string(ctx);
    format!("%{} = {} {} to {}", self.inst.get_name(), self.inst.get_opcode().to_string(), operand, dest_type)
  }

);

impl_sub_inst!(InstOpcode::ICompare(_), CompareInst,

  fn to_string(&self) -> String {
    let ctx = self.inst.ctx;
    let opcode = self.inst.get_opcode().to_string();
    let pred = self.get_pred().to_string();
    let lhs = self.inst.get_operand(0).unwrap();
    let ty = lhs.get_type(ctx).to_string(ctx);
    let lhs = lhs.to_string(ctx, false);
    let res = format!("%{} = {} {} {} {}", self.inst.get_name(), opcode, pred, ty, lhs);
    if let Some(rhs) = self.inst.get_operand(1) {
      let rhs = rhs.to_string(ctx, false);
      format!("{}, {}", res, rhs)
    } else {
      res
    }
  }

);


impl<'inst> Alloca <'inst> {

  pub fn get_align(&self) -> usize {
    if let InstOpcode::Alloca(align) = self.inst.get_opcode() {
      *align
    } else {
      panic!("Invalid opcode for Alloca instruction.");
    }
  }

}

/// Load instruction.
pub struct Load<'inst> {
  inst: &'inst InstructionRef<'inst>,
}

impl <'inst> Load <'inst> {

  pub fn get_ptr(&self) -> &ValueRef {
    &self.inst.get_operand(0).unwrap()
  }

  pub fn get_align(&self) -> usize {
    if let InstOpcode::Load(align) = self.inst.get_opcode() {
      *align
    } else {
      panic!("Invalid opcode for Load instruction.");
    }
  }

}


/// Store instruction.
pub struct Store<'inst> {
  inst:&'inst InstructionRef<'inst>,
}

impl <'inst> Store <'inst> {

  pub fn get_value(&self) -> &ValueRef {
    &self.inst.get_operand(0).unwrap()
  }

  pub fn get_ptr(&self) -> &ValueRef {
    &self.inst.get_operand(1).unwrap()
  }

  pub fn get_align(&self) -> usize {
    if let InstOpcode::Store(align) = self.inst.get_opcode() {
      *align
    } else {
      panic!("Invalid opcode for Store instruction.");
    }
  }

}

/// GetElementPtr instruction.
pub struct GetElementPtr<'inst> {
  inst:&'inst InstructionRef<'inst>,
}

impl <'inst>GetElementPtr<'inst> {

}

/// Call a callable value.
pub struct Call<'inst> {
  pub(super) inst:&'inst InstructionRef<'inst >,
}

impl <'inst> Call<'inst> {

  pub fn get_callee(&self) -> &ValueRef {
    &self.inst.get_operand(self.inst.get_num_operands() - 1).unwrap()
  }

  pub fn get_num_args(&self) -> usize {
    self.inst.get_num_operands() - 1
  }

  pub fn get_arg(&self, idx: usize) -> &ValueRef {
    &self.inst.get_operand(idx).unwrap()
  }

}

/// Call a callable value.
pub struct Return <'inst> {
  pub(super) inst: &'inst InstructionRef<'inst>,
}

impl <'inst> Return <'inst> {

  pub fn get_ret_val(&self) -> Option<&ValueRef> {
    if let Some(ret) = self.inst.get_operand(0) {
      Some(&ret)
    } else {
      None
    }
  }

}

/// Binary operation.
pub struct BinaryInst <'inst> {
  pub(super) inst:&'inst InstructionRef<'inst>,
}


impl<'inst> BinaryInst <'inst> {

  pub fn get_op(&self) -> BinaryOp {
    match self.inst.get_opcode() {
      InstOpcode::BinaryOp(op) => op.clone(),
      _ => { panic!("Invalid opcode!") }
    }
  }

  pub fn lhs(&self) -> &ValueRef {
    self.inst.get_operand(0).unwrap()
  }

  pub fn rhs(&self) -> &ValueRef {
    self.inst.get_operand(1).unwrap()
  }

}

pub struct CastInst <'inst> {
  pub(super) inst:&'inst InstructionRef<'inst>,
}

impl<'inst> CastInst <'inst> {

  pub fn dest_ty(&self) -> TypeRef {
    return self.inst.get_type().clone()
  }

}

pub struct CompareInst <'inst> {
  pub(super) inst:&'inst InstructionRef<'inst>,
}

impl <'inst> CompareInst<'inst> {

  pub fn get_pred(&self) -> &CmpPred {
    match self.inst.get_opcode() {
      InstOpcode::ICompare(x) => x,
      _ => { panic!("Invalid opcode!") }
    }
  }

}

pub struct BranchInst<'inst> {
  pub(super) inst:&'inst InstructionRef<'inst>
}

impl <'inst> BranchInst <'inst> {

  pub fn is_cond_br(&self) -> bool {
    self.inst.get_num_operands() == 3
  }

  pub fn cond(&self) -> Option<&ValueRef> {
    assert!(self.is_cond_br());
    self.inst.get_operand(0)
  }

  pub fn true_label(&self) -> Option<&ValueRef> {
    assert!(self.is_cond_br());
    self.inst.get_operand(1)
  }

  pub fn false_label(&self) -> Option<&ValueRef> {
    assert!(self.is_cond_br());
    self.inst.get_operand(2)
  }

  pub fn dest_label(&self) -> Option<&ValueRef> {
    assert!(!self.is_cond_br());
    self.inst.get_operand(0)
  }

  pub fn get_successors(&self) -> Vec<ValueRef> {
    if self.is_cond_br() {
      vec![self.inst.get_operand(1).unwrap().clone(), self.inst.get_operand(2).unwrap().clone()]
    } else {
      vec![self.inst.get_operand(0).unwrap().clone()]
    }
  }

}

/// The PHI node for SSA form.
/// 0-instd, each even operand is the incoming value, and each odd one is the incoming block.
pub struct PhiNode<'inst> {
  pub(super) inst: &'inst InstructionRef<'inst>
}

impl <'inst>PhiNode<'inst> {

  pub fn get_num_incomings(&self) -> usize {
    assert_eq!(self.inst.get_num_operands() % 2, 0);
    self.inst.get_num_operands() / 2
  }

  pub fn get_incoming_block(&self, index: usize) -> Option<&ValueRef> {
    if index < self.get_num_incomings() {
      Some(&self.inst.get_operand(index * 2 + 1).unwrap())
    } else {
      None
    }
  }

}
