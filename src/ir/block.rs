use super::value::ValueRef;

pub struct Block {
  pub(crate) skey: Option<usize>,
  pub(crate) name: String,
  pub(crate) insts: Vec<usize>,
  pub(crate) parent: ValueRef,
}

