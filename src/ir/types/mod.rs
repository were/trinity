use std::fmt;

pub mod arraytype;
pub mod functype;

pub use arraytype::{PointerType, ArrayType};
pub use functype::FunctionType;

use crate::context::{Context, Ptr};
use crate::context::component::{ComponentToRef, ComponentToMut, WithKindCode, GetSlabKey};
use crate::ir::value::consts::ConstArray;

use super::module::Module;
use super::value::ValueRef;

// Register all the types here.

#[derive(Clone, PartialEq, Hash, Eq)]
pub enum TKindCode {
  IntType,
  VoidType,
  StructType,
  PointerType,
  FunctionType,
  ArrayType,
  BlockType,
}


/// Very basic integer type
pub struct IntImpl {
  bits: usize,
}

pub type IntType = Ptr<IntImpl>;

impl IntType {
  
  /// Construct an integer type
  pub(crate) fn new(bits: usize) -> Self {
    Self::from(IntImpl { bits })
  }

  /// Return the number of bits
  pub fn get_bits(&self) -> usize {
    self.instance.bits
  }

}

impl fmt::Display for IntType {

  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "i{}", self.get_bits())
  }

}

/// Void type
pub type VoidType = Ptr<()>;

impl fmt::Display for VoidType {

  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "void")
  }

}

/// Struct type
pub struct StructImpl {
  pub(crate) name: String,
  pub(crate) attrs: Vec<TypeRef>,
}

pub type StructType = Ptr<StructImpl>;

impl StructType {

  pub fn to_string(&self, ctx: &Context) -> String {
    let attrs = self.instance.attrs.iter().map(|attr| attr.to_string(ctx)).collect::<Vec<_>>().join(", ");
    format!("%{} = type {{ {} }}", self.get_name(), attrs)
  }

  pub fn get_num_attrs(&self) -> usize {
    self.instance.attrs.len()
  }

  pub fn get_attr(&self, i: usize) -> TypeRef {
    self.instance.attrs[i].clone()
  }

  pub fn new(name: String) -> Self {
    Self::from(StructImpl {
      name,
      attrs: Vec::new(),
    })
  }

  pub fn get_name(&self) -> &str {
    &self.instance.name
  }

  pub fn set_body(&mut self, elements: Vec<TypeRef>) {
    self.instance.attrs = elements;
  }

}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct TypeRef {
  pub(crate) skey: usize,
  pub(crate) kind: TKindCode
}

impl<'ctx> TypeRef {

  pub fn kind(&self) -> &TKindCode {
    &self.kind
  }

  pub fn to_string(&self, ctx: &Context) -> String {
    match &self.kind {
      TKindCode::IntType => {
        let ty = ctx.get_value_ref::<IntType>(self.skey);
        ty.to_string()
      },
      TKindCode::VoidType => {
        let ty = ctx.get_value_ref::<VoidType>(self.skey);
        ty.to_string()
      },
      TKindCode::StructType => {
        let ty = ctx.get_value_ref::<StructType>(self.skey);
        format!("%{}", ty.get_name().to_string())
      },
      TKindCode::PointerType => {
        let ty = ctx.get_value_ref::<PointerType>(self.skey);
        ty.to_string(ctx)
      },
      TKindCode::ArrayType => {
        let ty = ctx.get_value_ref::<ArrayType>(self.skey);
        ty.to_string(ctx)
      },
      TKindCode::BlockType => {
        String::from("")
      },
      TKindCode::FunctionType => {
        todo!("Function type dump not implemented");
      },
    }
  }

  pub fn as_ref<T: WithKindCode<TKindCode> + ComponentToRef<T> + GetSlabKey>(&'ctx self, ctx: &'ctx Context) -> Option<&'ctx T> {
    if self.kind == T::kind_code() {
      Some(ctx.get_value_ref::<T>(self.skey))
    } else {
      None
    }
  }

  pub fn as_mut<T: WithKindCode<TKindCode> + ComponentToMut<T> + GetSlabKey>(&'ctx self, ctx: &'ctx mut Context) -> Option<&'ctx mut T> {
    if self.kind == T::kind_code() {
      Some(ctx.get_value_mut::<T>(self.skey))
    } else {
      None
    }
  }

  pub fn ptr_type(&self, ctx: &mut Context) -> TypeRef {
    ctx.pointer_type(self.clone())
  }

  pub fn fn_type(&self, ctx: &mut Context, args: Vec<TypeRef>) -> TypeRef {
    ctx.function_type(self.clone(), args)
  }

  pub fn array_type(&self, ctx: &mut Context, size: usize) -> TypeRef {
    ctx.array_type(self.clone(), size)
  }

  pub fn get_scalar_size_in_bits(&self, module: &Module) -> usize {
    let ctx = &module.context;
    let tm = &module.tm;
    match self.kind {
      TKindCode::IntType => {
        let it = self.as_ref::<IntType>(ctx).unwrap();
        it.get_bits()
      }
      TKindCode::VoidType => {
        1
      }
      TKindCode::StructType => {
        let st = self.as_ref::<StructType>(ctx).unwrap();
        st.instance.attrs.iter().map(|x| x.get_scalar_size_in_bits(module)).fold(0, |x, acc| acc + x)
      }
      TKindCode::ArrayType => {
        let at = self.as_ref::<ArrayType>(ctx).unwrap();
        at.get_elem_ty().get_scalar_size_in_bits(module)
      }
      TKindCode::PointerType => {
        tm.get_pointer_size_in_bits()
      }
      TKindCode::BlockType => {
        // TODO(@were): Later have program pointer size.
        tm.get_pointer_size_in_bits()
      }
      TKindCode::FunctionType => {
        // TODO(@were): Later have program pointer size.
        tm.get_pointer_size_in_bits()
      }
    }
  }

  pub fn const_array(&self, ctx: &mut Context, name_prefix: String, value: Vec<ValueRef>) -> ValueRef {
    assert!(self.kind == TKindCode::ArrayType);
    // TODO(@were): Check the types.
    let const_array = ConstArray::new(name_prefix, self.ptr_type(ctx), value);
    ctx.add_instance(const_array)
  }

}

