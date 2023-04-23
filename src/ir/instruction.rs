use super::value::ValueRef;
use super::types::{self, PointerType};

#[derive(Clone)]
pub struct Instruction {
  pub(crate) skey: Option<usize>,
  pub(crate) ty: types::TypeRef,
  pub(crate) opcode: InstOpcode,
  pub(crate) name: String,
  pub(crate) operands: Vec<ValueRef>,
  pub(crate) parent: ValueRef,
}

impl Instruction {

  pub fn get_name(&self) -> &str {
    &self.name
  }

  pub fn get_type(&self) -> &types::TypeRef {
    &self.ty
  }

  pub fn get_num_operands(&self) -> usize {
    self.operands.len()
  }

  pub fn get_operand(&self, idx: usize) -> ValueRef {
    self.operands[idx].clone()
  }

  pub fn set_operand(&mut self, idx: usize, new_value: ValueRef) {
    self.operands[idx] = new_value;
  }

  pub fn to_string(&self, ctx: &crate::context::Context) -> String {
    let mut res = String::new();
    match &self.opcode {
      InstOpcode::Alloca(align) => {
        res.push_str(format!("%{} = ", self.name).as_str());
        let ptr_ty = self.ty.as_ref::<PointerType>(ctx).unwrap();
        res.push_str(format!("alloca {}, align {}", ptr_ty.get_scalar_ty().to_string(ctx), align).as_str());
      },
      InstOpcode::GetElementPtr(inbounds) => {
        res.push_str(format!("  %{} = ", self.name).as_str());
        res.push_str("getelementptr ");
        if *inbounds {
          res.push_str("inbounds ");
        }
        res.push_str(format!("{}, ", self.ty.to_string(ctx)).as_str());
        res.push_str(
          (0..self.get_num_operands()).map(|i| {
            format!("{}", &self.get_operand(i).to_string(ctx))
          }).collect::<Vec<_>>().join(", ").as_str());
      }
      InstOpcode::Return => {
        res.push_str("ret");
        if self.get_num_operands() == 0 {
          res.push_str(" void");
        } else {
          res.push_str(format!(" {}", self.get_operand(0).to_string(ctx)).as_str());
        }
      },
      InstOpcode::Load(align) => {
        res.push_str(format!("%{} = ", self.name).as_str());
        res.push_str(format!("{} load {}, align {}", self.ty.to_string(ctx), self.get_operand(0).to_string(ctx), align).as_str());
      },
      InstOpcode::Store(align) => {
        let value = self.get_operand(0).to_string(ctx);
        let ptr = self.get_operand(1).to_string(ctx);
        res.push_str(format!("store {}, {} align {}", value, ptr, align).as_str());
      },
    }
    return res;
  }

}


// TODO(@were): Revisit this idea of code organization.
/// This is not only the opcode, but also the additional information of
/// these sub-instructions.
#[derive(Clone)]
pub enum InstOpcode {
  /// Memory allocation(alignment).
  Alloca(usize),
  /// Return instruction.
  Return,
  /// GetElementPtr instruction(inbounds).
  GetElementPtr(bool),
  /// Load instruction(alignment).
  Load(usize),
  /// Store instruction(alignment).
  Store(usize),
}

impl ToString for InstOpcode {
  fn to_string(&self) -> String {
    match self {
      InstOpcode::Alloca(_) => format!("alloca"),
      InstOpcode::Return => "ret".to_string(),
      InstOpcode::GetElementPtr(_) => format!("getelementptr"),
      InstOpcode::Load(_) => format!("load"),
      InstOpcode::Store(_) => format!("store"),
    }
  }
}

