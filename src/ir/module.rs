use std::fmt;


use crate::context::Context;
use crate::machine::{TargetTriple, DataLayout, TargetMachine};

use super::Function;
use super::value::consts::ConstObject;
use super::{value::function, ValueRef, ConstArray};
use super::types::{StructType, StructTypeRef};

pub struct Module {
  /// Context of this module.
  pub context: Context,
  /// The associated information of the target machine.
  pub tm: TargetMachine,
  /// The name of the module.
  mod_name: String,
  /// The source code file name.
  src_name: String,
  /// The function keys in this module.
  pub(crate) functions: Vec<usize>,
  /// The struct keys in this module.
  pub(crate) structs: Vec<usize>,
  /// The global values in this module.
  pub(crate) global_values: Vec<ValueRef>,
  /// LLVM loop latches.
  pub(crate) llvm_loop: Vec<usize>,
}

impl<'ctx> Module {

  /// Construct a module
  pub fn new(mod_name: String, src_name: String, tt: String, layout: String) -> Module {
    Module {
      mod_name,
      src_name,
      tm: TargetMachine {
        target_triple: TargetTriple::new(tt),
        data_layout: DataLayout::new(layout)
      },
      context: Context::new(),
      functions: Vec::new(),
      structs: Vec::new(),
      global_values: Vec::new(),
      llvm_loop: Vec::new()
    }
  }

  /// Get the number of structs in the module.
  pub fn num_structs(&self) -> usize {
    self.structs.len()
  }

  /// Get the struct reference by name
  pub fn get_struct(&'ctx self, i: usize) -> Option<StructTypeRef<'ctx>> {
    self.structs.get(i).map(|x| {
      let ty = StructType::from_skey(*x);
      ty.as_ref::<StructType>(&self.context).unwrap()
    })
  }

  /// Iterate over the structs defined in this module.
  pub fn struct_iter(&'ctx self) -> impl Iterator<Item = StructTypeRef<'ctx>> {
    self.structs.iter().map(|x| {
      let ty = StructType::from_skey(*x);
      ty.as_ref::<StructType>(&self.context).unwrap()
    })
  }

  /// Get the struct mutable reference by name
  pub fn get_struct_mut(&'ctx mut self, i: usize) -> &mut StructType {
    self.context.get_value_mut::<StructType>(self.structs[i])
  }

  /// The number of functions in the module.
  pub fn get_num_functions(&self) -> usize {
    self.functions.len()
  }

  /// The number of global values in the module.
  pub fn get_num_gvs(&self) -> usize {
    self.global_values.len()
  }

  /// Get the global value by indices.
  pub fn get_gv(&self, i: usize) -> ValueRef {
    self.global_values[i].clone()
  }

  /// Iterate over the global values defined in this module.
  pub fn gv_iter(&self) -> impl Iterator<Item = &ValueRef> {
    self.global_values.iter().map(|x| x)
  }

  /// Get the function by indices.
  pub fn get_function(&'ctx self, idx: usize) -> Option<function::FunctionRef<'ctx>> {
    self.functions.get(idx).map(|x| {
      Function::from_skey(*x).as_ref::<function::Function>(&self.context).unwrap()
    })
  }

  pub fn func_iter(&'ctx self) -> impl Iterator<Item = function::FunctionRef<'ctx>> {
    self.functions.iter().map(|x| {
      Function::from_skey(*x).as_ref::<function::Function>(&self.context).unwrap()
    })
  }

}

/// Make the name emission ready.
pub fn namify(name: &String) -> String {
  let mut res = String::new();
  name.chars().into_iter().for_each(|c| match c {
    'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '.' => res.push(c),
    _ => res.push_str(&format!("_{:x}_", c as u32)),
  });
  res
}

impl fmt::Display for Module {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "; ModuleID = '{}'\n", self.mod_name).unwrap();
    write!(f, "source_filename = \"{}\"\n", self.src_name).unwrap();
    write!(f, "target triple = \"{}\"\n", self.tm.target_triple.to_string()).unwrap();
    write!(f, "target datalayout = \"{}\"\n", self.tm.data_layout.to_string()).unwrap();
    write!(f, "\n").unwrap();
    for elem in self.struct_iter() {
      write!(f, "{}\n", elem.to_string()).unwrap();
    }
    for elem in self.gv_iter() {
      match elem.kind {
        super::VKindCode::ConstArray => {
          let array = elem.as_ref::<ConstArray>(&self.context).unwrap();
          write!(f, "{}\n", array.to_string()).unwrap();
        }
        super::VKindCode::ConstObject => {
          let obj = elem.as_ref::<ConstObject>(&self.context).unwrap();
          write!(f, "{}\n", obj.to_string()).unwrap();
        }
        _ => (),
      }
    }
    write!(f, "\n").unwrap();
    for func in self.func_iter() {
      write!(f, "{}", func.to_string()).unwrap();
      // TODO(@were): More linkage policies
      write!(f, "\n\n").unwrap();
    }
    for i in self.llvm_loop {
      write!(f, "!{} = !{{ !{} }}\n", i, i).unwrap();
    }
    Ok(())
  }
}


