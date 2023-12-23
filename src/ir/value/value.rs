use crate::context::{Context, Reference, IsSlabEntry};
use crate::context::component::{ComponentToRef, ComponentToMut, WithSlabKey, WithSuperType};
use crate::ir::ConstExpr;
use crate::ir::types::{TypeRef, TKindCode};
use crate::ir::module::{Module, namify};

use super::consts::{ConstObject, Undef};
use super::block::Block;
use super::function::{Function, Argument};
use super::instruction::Instruction;
use super::consts::{ConstScalar, ConstArray, InlineAsm};

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ValueRef {
  pub skey: usize,
  pub kind: VKindCode
}

impl<'ctx> ValueRef {

  pub fn as_ref<T>(&self, context: &'ctx Context) -> Option<Reference<'ctx, T::Impl>> 
    where T: WithSuperType<VKindCode> + ComponentToRef<T> + WithSlabKey + IsSlabEntry + 'ctx {
    if self.kind == T::kind_code() {
      match context.get_value_ref::<T>(self.skey) {
        Some(instance) => Some(Reference::new(context, instance.to_slab_entry())),
        None => Some(Reference::invalid(context, self.skey))
      }
    } else {
      None
    }
  }

  pub fn as_mut<T>(&self, context: &'ctx mut Context) -> Option<&'ctx mut T> 
    where T: WithSuperType<VKindCode> + ComponentToMut<T> + WithSlabKey {
    if self.kind == T::kind_code() {
      Some(context.get_value_mut::<T>(self.skey))
    } else {
      None
    }
  }

  fn type_to_string(&self, ctx: &'ctx Context, with_type: bool) -> String {
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
        let func_name = if with_type {
          func.get_ret_ty().to_string(ctx) + " "
        } else {
          "".to_string()
        };
        format!("{}@{}", func_name, namify(&func.get_name()))
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
        format!("undef")
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
        let const_scalar = self.as_ref::<ConstScalar>(ctx).unwrap();
        const_scalar.get_type().clone()
      },
      VKindCode::Function => {
        let func = self.as_ref::<Function>(ctx).unwrap();
        func.instance().unwrap().fty.clone()
      },
      VKindCode::ConstArray => {
        ctx.pointer_type()
      },
      VKindCode::ConstExpr => {
        let const_expr = self.as_ref::<ConstExpr>(ctx).unwrap();
        const_expr.instance().unwrap().ty.clone()
      },
      VKindCode::ConstObject => {
        ctx.pointer_type()
      },
      VKindCode::InlineAsm => {
        let inline_asm = self.as_ref::<InlineAsm>(ctx).unwrap();
        inline_asm.get_type().clone()
      },
      VKindCode::Undef => {
        let undef = self.as_ref::<Undef>(ctx).unwrap();
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
      VKindCode::ConstScalar |
      VKindCode::ConstArray |
      VKindCode::ConstExpr |
      VKindCode::ConstObject => true,
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

#[derive(Clone, PartialEq, Hash, Eq, Debug)]
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

