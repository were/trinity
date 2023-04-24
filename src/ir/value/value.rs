use crate::context::Context;
use crate::context::component::{ComponentToRef, ComponentToMut, WithKindCode};
use crate::ir::ConstExpr;
use crate::ir::types::{TypeRef, TKindCode, PointerType};
use crate::ir::module::Module;

use super::consts::ConstObject;
use super::block::Block;
use super::function::{Function, Argument};
use super::instruction::Instruction;
use super::consts::{ConstScalar, ConstArray };

#[derive(Clone)]
pub struct ValueRef {
  pub skey: usize,
  pub kind: VKindCode
}

impl<'ctx> ValueRef {

  pub fn undef() -> Self {
    Self { skey: 0, kind: VKindCode::Unknown }
  }

  pub fn as_ref<T: WithKindCode<VKindCode> + ComponentToRef<T>>(&'ctx self, context: &'ctx Context) -> Option<&'ctx T> {
    if self.kind == T::kind_code() {
      Some(context.get_value_ref::<T>(self.skey))
    } else {
      None
    }
  }

  pub fn as_mut<T: WithKindCode<VKindCode> + ComponentToMut<T>>(&'ctx self, context: &'ctx mut Context) -> Option<&'ctx mut T> {
    if self.kind == T::kind_code() {
      Some(context.get_value_mut::<T>(self.skey))
    } else {
      None
    }
  }

  pub fn to_string(&self, ctx: &'ctx Context) -> String {
    match self.kind {
      VKindCode::Block => {
        let block = ctx.get_value_ref::<Block>(self.skey);
        format!("%{}", block.name)
      },
      VKindCode::Argument => {
        let arg = ctx.get_value_ref::<Argument>(self.skey);
        format!("%arg.{}", arg.arg_idx)
      },
      VKindCode::Instruction => {
        let inst = ctx.get_value_ref::<Instruction>(self.skey);
        format!("%{}", inst.name)
      },
      VKindCode::ConstScalar => {
        let const_scalar = ctx.get_value_ref::<ConstScalar>(self.skey);
        format!("{} {}", const_scalar.ty.to_string(ctx), const_scalar.value)
      },
      VKindCode::Function => {
        let func = ctx.get_value_ref::<Function>(self.skey);
        format!("{} @{}", func.get_ret_ty(ctx).to_string(ctx), func.name)
      },
      VKindCode::ConstArray => {
        let const_array = ctx.get_value_ref::<ConstArray>(self.skey);
        format!("{} @{}", const_array.ty.to_string(ctx), const_array.name)
      },
      VKindCode::ConstExpr => {
        let const_expr = ctx.get_value_ref::<ConstExpr>(self.skey);
        format!("{}", const_expr.to_string(ctx))
      },
      VKindCode::ConstObject => {
        let const_object = ctx.get_value_ref::<ConstObject>(self.skey);
        let ptr_ty = const_object.ty.as_ref::<PointerType>(ctx).unwrap();
        format!("{} @{}", ptr_ty.get_scalar_ty().to_string(ctx), const_object.name)
      },
      VKindCode::Unknown => {
        format!("[unknown]")
      }
    }
  }

  pub fn get_type(&self, ctx: &'ctx Context) -> TypeRef {
    match self.kind {
      VKindCode::Block => {
        TypeRef { skey: 0, kind: TKindCode::BlockType }
      },
      VKindCode::Argument => {
        let arg = ctx.get_value_ref::<Argument>(self.skey);
        arg.ty.clone()
      },
      VKindCode::Instruction => {
        let inst = ctx.get_value_ref::<Instruction>(self.skey);
        inst.get_type().clone()
      },
      VKindCode::ConstScalar => {
        let const_scalar = ctx.get_value_ref::<ConstScalar>(self.skey);
        const_scalar.ty.clone()
      },
      VKindCode::Function => {
        let func = ctx.get_value_ref::<Function>(self.skey);
        func.fty.clone()
      },
      VKindCode::ConstArray => {
        let const_array = ctx.get_value_ref::<ConstArray>(self.skey);
        const_array.ty.clone()
      },
      VKindCode::ConstExpr => {
        let const_expr = ctx.get_value_ref::<ConstExpr>(self.skey);
        const_expr.ty.clone()
      },
      VKindCode::ConstObject => {
        let const_object = ctx.get_value_ref::<ConstArray>(self.skey);
        let ptr_ty = const_object.ty.as_ref::<PointerType>(ctx).unwrap();
        ptr_ty.get_scalar_ty()
      },
      VKindCode::Unknown => {
        panic!("Unknown value type")
      }
    }
  }

  /// Returns true if the value is a constant.
  pub fn is_const(&self) -> bool {
    match self.kind {
      VKindCode::ConstScalar | VKindCode::ConstArray | VKindCode::ConstExpr | VKindCode::ConstObject => true,
      _ => false
    }
  }

  pub fn is_callable(&self) -> bool {
    match self.kind {
      VKindCode::Function => true,
      _ => false
    }
  }
}

#[derive(Clone, PartialEq)]
pub enum VKindCode {
  Argument,
  Instruction,
  Function,
  Block,
  ConstScalar,
  ConstArray,
  ConstExpr,
  ConstObject,
  Unknown
}

pub trait FindInstance<'ctx, T> {
  fn find_instance(module: &'ctx Module, value: &'ctx ValueRef) -> &'ctx T;
}

pub trait FindInstanceMut<'ctx, T> {
  fn find_instance(module: &'ctx mut Module, value: &'ctx ValueRef) -> &'ctx mut T;
}

