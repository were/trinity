use super::value::ValueRef;
use super::types;

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
        res.push_str(format!("  %{} = ", self.name).as_str());
        res.push_str(format!("alloca {}, align {}", self.ty.to_string(ctx), align).as_str());
      },
      InstOpcode::Return => {
        res.push_str("  ret void");
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
  /// The alignment of the allocated memory.
  Alloca(usize),
  /// Return instruction.
  Return,
}

