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

}


/// This is not only the opcode, but also the additional information of
/// these sub-instructions.
#[derive(Clone)]
pub enum InstOpcode {
  /// The alignment of the allocated memory.
  Alloca(usize),
  /// Return instruction.
  Return,
}

