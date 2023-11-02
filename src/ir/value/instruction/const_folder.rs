use crate::{ir::{ValueRef, ConstScalar, TKindCode}, context::Context};

use super::{BinaryOp, CmpPred};

pub fn fold_binary_op(op: &BinaryOp, ctx: &mut Context, a: &ValueRef, b: &ValueRef)
  -> Option<ValueRef> {
  if let Some(const_a) = a.as_ref::<ConstScalar>(ctx) {
    if const_a.get_type().kind != TKindCode::IntType {
      return None;
    }
    if let Some(const_b) = b.as_ref::<ConstScalar>(ctx) {
      if *const_b.get_type() != *const_a.get_type() {
        return None;
      }
      let raw = match op {
        BinaryOp::Add => const_a.get_value() + const_b.get_value(),
        BinaryOp::Sub => ((const_a.get_value() as i64) - (const_b.get_value() as i64)) as u64,
        BinaryOp::Mul => const_a.get_value() * const_b.get_value(),
        BinaryOp::SDiv => ((const_a.get_value() as i64) / (const_b.get_value() as i64)) as u64,
        BinaryOp::SRem => const_a.get_value() % const_b.get_value(),
        BinaryOp::Shl => const_a.get_value() << const_b.get_value(),
        BinaryOp::AShr => const_a.get_value() >> const_b.get_value(),
        BinaryOp::And => const_a.get_value() & const_b.get_value(),
        BinaryOp::Or => const_a.get_value() | const_b.get_value(),
        BinaryOp::Xor => const_a.get_value() ^ const_b.get_value(),
        _ => { return None }
      };
      let ty = const_a.get_type().clone();
      return Some(ctx.const_value(ty, raw));
    }
  }
  return None;
}

pub fn fold_cmp_op(op: &CmpPred, ctx: &mut Context, a: &ValueRef, b: &ValueRef)
  -> Option<ValueRef> {
  if let Some(const_a) = a.as_ref::<ConstScalar>(ctx) {
    if const_a.get_type().kind != TKindCode::IntType {
      return None;
    }
    if let Some(const_b) = b.as_ref::<ConstScalar>(ctx) {
      if *const_b.get_type() != *const_a.get_type() {
        return None;
      }
      let raw = match op {
        CmpPred::EQ => const_a.get_value() == const_b.get_value(),
        CmpPred::NE => const_a.get_value() != const_b.get_value(),
        CmpPred::SLT => (const_a.get_value() as i64) < (const_b.get_value() as i64),
        CmpPred::SLE => (const_a.get_value() as i64) <= (const_b.get_value() as i64),
        CmpPred::SGT => (const_a.get_value() as i64) > (const_b.get_value() as i64),
        CmpPred::SGE => (const_a.get_value() as i64) >= (const_b.get_value() as i64),
      };
      let ty = ctx.int_type(1);
      return Some(ctx.const_value(ty, raw as u64));
    }
  }
  return None;
}
