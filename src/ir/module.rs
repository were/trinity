use std::fmt;


use crate::context::{Context, Reference};
use crate::machine::{TargetTriple, DataLayout, TargetMachine};

use super::value::function::FunctionRef;
use super::{Function, TypeRef};
use super::value::consts::ConstObject;
use super::{value::function, ValueRef, ConstArray};
use super::types::StructType;

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
  /// The number of loop metadata.
  pub(crate) llvm_loop: usize,
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
      llvm_loop: 0
    }
  }

  /// Get the number of structs in the module.
  pub fn num_structs(&self) -> usize {
    self.structs.len()
  }

  /// Get the struct reference by name
  pub fn get_struct(&'ctx self, i: usize) -> TypeRef {
    StructType::from_skey(self.structs[i])
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

  /// Get the function by indices.
  pub fn get_function(&'ctx self, idx: usize) -> &'ctx function::Function {
    self.context.get_value_ref::<function::Function>(self.functions[idx]).unwrap()
  }

  pub fn func_iter(&'ctx self) -> impl Iterator<Item =function::FunctionRef<'ctx>> {
    self.functions.iter().map(|x| {
      Function::from_skey(*x).as_ref::<function::Function>(&self.context).unwrap()
    })
  }

}

/// Make the name emission ready.
pub fn namify(name: &String) -> String {
  let mut res = String::new();
  name.chars().into_iter().for_each(|c| match c {
    'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => res.push(c),
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
    for i in 0..self.num_structs() {
      let elem = self.get_struct(i);
      let elem = elem.as_ref::<StructType>(&self.context).unwrap();
      write!(f, "{}\n", elem.to_string()).unwrap();
    }
    for i in 0..self.get_num_gvs() {
      let elem = self.get_gv(i);
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
    for i in 0..self.get_num_functions() {
      let func = self.get_function(i);
      let func = Reference::new(&self.context, func);
      write!(f, "{}", func.to_string()).unwrap();
      // TODO(@were): More linkage policies
      write!(f, "\n\n").unwrap();
    }
    for i in 0..self.llvm_loop {
      write!(f, "!{} = !{{ !{} }}\n", i, i).unwrap();
    }
    Ok(())
  }
}


