use crate::context::{Context, Reference};
use crate::context::component::{ComponentToRef, ComponentToMut, WithKindCode, GetSlabKey};
use crate::ir::ConstExpr;
use crate::ir::types::{TypeRef, TKindCode};
use crate::ir::module::{Module, namify};

use super::consts::{ConstObject, Undef};
use super::block::Block;
use super::function::{Function, Argument, FunctionRef};
use super::instruction::Instruction;
use super::consts::{ConstScalar, ConstArray, InlineAsm};

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct ValueRef {
  pub skey: usize,
  pub kind: VKindCode
}

impl<'ctx> ValueRef {

  pub fn as_ref<T>(&self, context: &'ctx Context) -> Option<&'ctx T> 
    where T: WithKindCode<VKindCode> + ComponentToRef<T> + GetSlabKey {
    if self.kind == T::kind_code() {
      Some(context.get_value_ref::<T>(self.skey))
    } else {
      None
    }
  }

  pub fn as_mut<T>(&self, context: &'ctx mut Context) -> Option<&'ctx mut T> 
    where T: WithKindCode<VKindCode> + ComponentToMut<T> + GetSlabKey {
    if self.kind == T::kind_code() {
      Some(context.get_value_mut::<T>(self.skey))
    } else {
      None
    }
  }

  fn type_to_string(&self, ctx: &'ctx Context, with_type: bool) -> String {
    self.get_type(ctx).to_string(ctx);
    if with_type {
      format!("{} ", self.get_type(ctx).to_string(ctx))
    } else {
      "".to_string()
    }
  }

  pub fn to_string(&self, ctx: &'ctx Context, with_type: bool) -> String {
    match self.kind {
      VKindCode::Block => {
        let block = ctx.get_value_ref::<Block>(self.skey);
        format!("%{}", block.get_name())
      },
      VKindCode::Argument => {
        let arg = self.as_ref::<Argument>(ctx).unwrap();
        let arg = Reference::new(self.skey, ctx, arg);
        format!("{}%{}", self.type_to_string(ctx, with_type), arg.get_name())
      },
      VKindCode::Instruction => {
        let inst = ctx.get_value_ref::<Instruction>(self.skey);
        let inst = Reference::new(inst.get_skey(), ctx, inst);
        format!("{}%{}", self.type_to_string(ctx, with_type), inst.get_name())
      },
      VKindCode::ConstScalar => {
        let const_scalar = ctx.get_value_ref::<ConstScalar>(self.skey);
        format!("{}{}", self.type_to_string(ctx, with_type), const_scalar.get_value())
      },
      VKindCode::Function => {
        let func = ctx.get_value_ref::<Function>(self.skey);
        let func = FunctionRef::new(func.get_skey(), ctx, func);
        format!("{}@{}", if with_type { func.get_ret_ty().to_string(ctx) + " " } else { "".to_string() }, namify(&func.get_name()))
      },
      VKindCode::ConstArray => {
        let const_array = ctx.get_value_ref::<ConstArray>(self.skey);
        format!("{}@{}", self.type_to_string(ctx, with_type), const_array.get_name())
      },
      VKindCode::ConstExpr => {
        let const_expr = ctx.get_value_ref::<ConstExpr>(self.skey);
        format!("{}", const_expr.to_string(ctx))
      },
      VKindCode::ConstObject => {
        let const_object = ctx.get_value_ref::<ConstObject>(self.skey);
        format!("{}@{}", self.type_to_string(ctx, with_type), const_object.get_name())
      },
      VKindCode::InlineAsm => {
        let inline_asm = ctx.get_value_ref::<InlineAsm>(self.skey);
        inline_asm.to_string(ctx)
      },
      VKindCode::Undef => {
        format!("[undef]")
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
        arg.instance.ty.clone()
      },
      VKindCode::Instruction => {
        let inst = ctx.get_value_ref::<Instruction>(self.skey);
        let inst = Reference::new(inst.get_skey(), ctx, inst);
        inst.get_type().clone()
      },
      VKindCode::ConstScalar => {
        let const_scalar = ctx.get_value_ref::<ConstScalar>(self.skey);
        const_scalar.get_type().clone()
      },
      VKindCode::Function => {
        let func = ctx.get_value_ref::<Function>(self.skey);
        let func = Reference::new(func.get_skey(), ctx, func);
        func.get_type()
      },
      VKindCode::ConstArray => {
        let const_array = self.as_ref::<ConstArray>(ctx).unwrap();
        const_array.get_type().clone()
      },
      VKindCode::ConstExpr => {
        let const_expr = ctx.get_value_ref::<ConstExpr>(self.skey);
        let inst = &const_expr.instance.inst;
        let inst = Reference::new(inst.get_skey(), ctx, inst);
        inst.get_type().clone()
      },
      VKindCode::ConstObject => {
        let const_object = ctx.get_value_ref::<ConstObject>(self.skey);
        const_object.get_type().clone()
      },
      VKindCode::InlineAsm => {
        let inline_asm = ctx.get_value_ref::<InlineAsm>(self.skey);
        inline_asm.get_type().clone()
      },
      VKindCode::Undef => {
        let undef = ctx.get_value_ref::<Undef>(self.skey);
        undef.get_type().clone()
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

  /// Returns true if this value is callable.
  pub fn is_callable(&self) -> bool {
    match self.kind {
      VKindCode::Function => true,
      VKindCode::InlineAsm => true,
      _ => false
    }
  }
}

#[derive(Clone, PartialEq, Hash, Eq)]
pub enum VKindCode {
  Argument,
  Instruction,
  Function,
  Block,
  ConstScalar,
  ConstArray,
  ConstExpr,
  ConstObject,
  InlineAsm,
  Undef,
  Unknown,
}

pub trait FindInstance<'ctx, T> {
  fn find_instance(module: &'ctx Module, value: &'ctx ValueRef) -> &'ctx T;
}

pub trait FindInstanceMut<'ctx, T> {
  fn find_instance(module: &'ctx mut Module, value: &'ctx ValueRef) -> &'ctx mut T;
}

