use crate::context::SlabEntry;

/// The module of data dependence graph (DDG).

use super::ValueRef;

#[derive(PartialEq, Eq, Clone)]
pub struct EdgeImpl {
  def: ValueRef,
  user: ValueRef,
}

impl EdgeImpl {

  pub(crate) fn new(def: ValueRef, user: ValueRef) -> Self {
    Self { def, user }
  }

}

pub type Edge = SlabEntry<EdgeImpl>;

impl Edge {

  pub fn def(&self) -> &ValueRef {
    &self.instance.def
  }

  pub fn user(&self) -> &ValueRef {
    &self.instance.user
  }

}
