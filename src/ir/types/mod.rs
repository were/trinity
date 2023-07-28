use std::fmt;

pub mod arraytype;
pub mod functype;

pub use arraytype::{PointerType, ArrayType, PointerImpl, ArrayTypeImpl};
pub use functype::{FunctionType, FuncTypeImpl};

use crate::context::{Context, SlabEntry, Reference, IsSlabEntry};
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

pub type IntType = SlabEntry<IntImpl>;
pub type IntTypeRef<'ctx> = Reference<'ctx, IntImpl>;

impl IntType {
  
  /// Construct an integer type
  pub(crate) fn new(bits: usize) -> Self {
    Self::from(IntImpl { bits })
  }

}

impl <'ctx>IntTypeRef<'ctx> {

  /// Return the number of bits
  pub fn get_bits(&self) -> usize {
    self.instance().unwrap().bits
  }

}

impl fmt::Display for IntTypeRef<'_> {

  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "i{}", self.get_bits())
  }

}

/// Void type
pub type VoidType = SlabEntry<()>;

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

pub type StructType = SlabEntry<StructImpl>;
pub type StructTypeRef<'ctx> = Reference<'ctx, StructImpl>;

impl StructType {

  pub fn new(name: String) -> Self {
    Self::from(StructImpl {
      name,
      attrs: Vec::new(),
    })
  }

  pub fn set_body(&mut self, elements: Vec<TypeRef>) {
    self.instance.attrs = elements;
  }

}

impl <'ctx>StructTypeRef<'ctx> {

  pub fn to_string(&self) -> String {
    let attrs = self
      .instance()
      .unwrap()
      .attrs
      .iter()
      .map(|attr| attr.to_string(self.ctx))
      .collect::<Vec<_>>()
      .join(", ");
    format!("%{} = type {{ {} }}", self.get_name(), attrs)
  }

  pub fn get_num_attrs(&self) -> usize {
    self.instance().unwrap().attrs.len()
  }

  pub fn get_attr(&self, i: usize) -> TypeRef {
    self.instance().unwrap().attrs[i].clone()
  }

  pub fn get_name(&self) -> String {
    self.instance().unwrap().name.to_string()
  }

  pub fn get_offset_in_bytes(&self, module: &Module, idx: usize) -> usize {
    let mut offset = 0;
    let align = self.as_super().get_align_in_bits(module) / 8;
    for i in 0..idx {
      offset += self.instance().unwrap().attrs[i].get_scalar_size_in_bits(module) / 8;
      let rem = offset % align;
      if rem != 0 {
        offset += align - rem;
      }
    }
    offset
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
        let ty = self.as_ref::<IntType>(ctx).unwrap();
        ty.to_string()
      },
      TKindCode::VoidType => {
        "void".to_string()
      },
      TKindCode::StructType => {
        let ty = self.as_ref::<StructType>(ctx).unwrap();
        format!("%{}", ty.get_name())
      },
      TKindCode::PointerType => {
        let ty = self.as_ref::<PointerType>(ctx).unwrap();
        ty.to_string()
      },
      TKindCode::ArrayType => {
        let ty = self.as_ref::<ArrayType>(ctx).unwrap();
        ty.to_string()
      },
      TKindCode::BlockType => {
        String::from("")
      },
      TKindCode::FunctionType => {
        todo!("Function type dump not implemented");
      },
    }
  }

  pub fn as_ref<T>(&self, ctx: &'ctx Context) -> Option<Reference<'ctx, T::Impl>>
    where T: WithKindCode<TKindCode> + ComponentToRef<T> + GetSlabKey + IsSlabEntry + 'ctx {
    if self.kind == T::kind_code() {
      let instance_ref = ctx.get_value_ref::<T>(self.skey);
      match instance_ref {
        Some(instance_ref) => Some(Reference::new(ctx, instance_ref.to_slab_entry())),
        None => Some(Reference::invalid(ctx, self.skey)),
      }
    } else {
      None
    }
  }

  pub fn as_mut<T>(&'ctx self, ctx: &'ctx mut Context) -> Option<&'ctx mut T>
    where T: WithKindCode<TKindCode> + ComponentToMut<T> + GetSlabKey {
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

  /// For now, most types are aligned to their size.
  pub fn get_align_in_bits(&self, module: &Module) -> usize {
    let ctx = &module.context;
    match self.kind {
      TKindCode::StructType => {
        let st = self.as_ref::<StructType>(ctx).unwrap();
        st.instance().unwrap().attrs.iter()
          .map(|x| x.get_scalar_size_in_bits(module))
          .fold(0, |x, acc| std::cmp::max(x, acc))
      }
      _ => {
        self.get_scalar_size_in_bits(module)
      }
    }
  }

  pub fn get_scalar_size_in_bits(&self, module: &Module) -> usize {
    let ctx = &module.context;
    let tm = &module.tm;
    match self.kind {
      TKindCode::IntType => {
        let it = self.as_ref::<IntType>(ctx).unwrap();
        it.get_bits()
      }
      TKindCode::VoidType => { 1 }
      TKindCode::StructType => {
        let st = self.as_ref::<StructType>(ctx).unwrap();
        let res = st.get_offset_in_bytes(module, st.get_num_attrs()) * 8;
        res
      }
      TKindCode::ArrayType => {
        let at = self.as_ref::<ArrayType>(ctx).unwrap();
        at.get_elem_ty().get_scalar_size_in_bits(module) * at.get_size()
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

