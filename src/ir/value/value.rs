use crate::context::{Context, Reference, IsSlabEntry};
use crate::context::component::{ComponentToRef, ComponentToMut, WithKindCode, GetSlabKey};
use crate::ir::ConstExpr;
use crate::ir::types::{TypeRef, TKindCode};
use crate::ir::module::{Module, namify};

use super::consts::{ConstObject, Undef};
use super::block::Block;
use super::function::{Function, Argument};
use super::instruction::Instruction;
use super::consts::{ConstScalar, ConstArray, InlineAsm};

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct ValueRef {
  pub skey: usize,
  pub kind: VKindCode
}

impl<'ctx> ValueRef {

  pub fn as_ref<T>(&self, context: &'ctx Context) -> Option<Reference<'ctx, T::Impl>> 
    where T: WithKindCode<VKindCode> + ComponentToRef<T> + GetSlabKey + IsSlabEntry + 'ctx {
    if self.kind == T::kind_code() {
      let instance_ref = context.get_value_ref::<T>(self.skey);
      Some(Reference::new(context, instance_ref.to_slab_entry()))
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
        let block = self.as_ref::<Block>(ctx).unwrap();
        format!("%{}", block.get_name())
      },
      VKindCode::Argument => {
        let arg = self.as_ref::<Argument>(ctx).unwrap();
        format!("{}%{}", self.type_to_string(ctx, with_type), arg.get_name())
      },
      VKindCode::Instruction => {
        let inst = self.as_ref::<Instruction>(ctx).unwrap();
        format!("{}%{}", self.type_to_string(ctx, with_type), inst.get_name())
      },
      VKindCode::ConstScalar => {
        let const_scalar = self.as_ref::<ConstScalar>(ctx).unwrap();
        format!("{}{}", self.type_to_string(ctx, with_type), const_scalar.get_value())
      },
      VKindCode::Function => {
        let func = self.as_ref::<Function>(ctx).unwrap();
        format!("{}@{}", if with_type { func.get_ret_ty().to_string(ctx) + " " } else { "".to_string() }, namify(&func.get_name()))
      },
      VKindCode::ConstArray => {
        let const_array = self.as_ref::<ConstArray>(ctx).unwrap();
        format!("{}@{}", self.type_to_string(ctx, with_type), const_array.get_name())
      },
      VKindCode::ConstExpr => {
        let const_expr = self.as_ref::<ConstExpr>(ctx).unwrap();
        format!("{}", const_expr.to_string())
      },
      VKindCode::ConstObject => {
        let const_object = self.as_ref::<ConstObject>(ctx).unwrap();
        format!("{}@{}", self.type_to_string(ctx, with_type), const_object.get_name())
      },
      VKindCode::InlineAsm => {
        let inline_asm = self.as_ref::<InlineAsm>(ctx).unwrap();
        inline_asm.to_string()
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
        let arg = self.as_ref::<Argument>(ctx).unwrap();
        arg.get_type()
      },
      VKindCode::Instruction => {
        let inst = self.as_ref::<Instruction>(ctx).unwrap();
        inst.get_type().clone()
      },
      VKindCode::ConstScalar => {
        let const_scalar = ctx.get_value_ref::<ConstScalar>(self.skey);
        let const_scalar = Reference::new(ctx, const_scalar);
        const_scalar.get_type().clone()
      },
      VKindCode::Function => {
        let func = ctx.get_value_ref::<Function>(self.skey);
        let func = Reference::new(ctx, func);
        func.get_type()
      },
      VKindCode::ConstArray => {
        let const_array = self.as_ref::<ConstArray>(ctx).unwrap();
        const_array.get_type().clone()
      },
      VKindCode::ConstExpr => {
        let const_expr = ctx.get_value_ref::<ConstExpr>(self.skey);
        let inst = &const_expr.instance.inst;
        let inst = Reference::new(ctx, inst);
        inst.get_type().clone()
      },
      VKindCode::ConstObject => {
        let const_object = ctx.get_value_ref::<ConstObject>(self.skey);
        let const_object = Reference::new(ctx, const_object);
        const_object.get_type().clone()
      },
      VKindCode::InlineAsm => {
        let inline_asm = ctx.get_value_ref::<InlineAsm>(self.skey);
        let inline_asm = Reference::new(ctx, inline_asm);
        inline_asm.get_type().clone()
      },
      VKindCode::Undef => {
        let undef = ctx.get_value_ref::<Undef>(self.skey);
        let undef = Reference::new(ctx, undef);
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

