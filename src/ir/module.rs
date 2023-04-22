use std::fmt;


use crate::context::Context;

use super::consts::ConstArray;
use super::function;
use super::types::StructType;

pub struct Module {
  /// Context of this module.
  pub context: Context,
  /// The name of the module.
  mod_name: String,
  /// The source code file name.
  src_name: String,
  /// The function keys in this module.
  pub(crate) functions: Vec<usize>,
  /// The struct keys in this module.
  pub(crate) structs: Vec<usize>,
  /// The global values in this module.
  pub(crate) global_values: Vec<usize>,
}

impl<'ctx> Module {

  /// Construct a module
  pub fn new(mod_name: String, src_name: String) -> Module {
    Module {
      mod_name,
      src_name,
      context: Context::new(),
      functions: Vec::new(),
      structs: Vec::new(),
      global_values: Vec::new(),
    }
  }

  /// Get the number of structs in the module.
  pub fn num_structs(&self) -> usize {
    self.structs.len()
  }

  /// Get the struct reference by name
  pub fn get_struct(&'ctx self, i: usize) -> &StructType {
    self.context.get_value_ref::<StructType>(self.structs[i])
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
  pub fn get_gv(&self, i: usize) -> &ConstArray {
    self.context.get_value_ref::<ConstArray>(self.global_values[i])
  }

  /// Get the function by indices.
  pub fn get_function(&'ctx self, idx: usize) -> &'ctx function::Function {
    self.context.get_value_ref::<function::Function>(self.functions[idx])
  }

  pub fn get_function_mut(&'ctx mut self, idx: usize) -> &'ctx mut function::Function {
    self.context.get_value_mut::<function::Function>(self.functions[idx])
  }

}

pub(crate) fn namify(name: &String) -> String {
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
    write!(f, "source_filename = '{}'\n\n", self.src_name).unwrap();
    for i in 0..self.num_structs() {
      let elem = self.get_struct(i);
      write!(f, "{}\n", elem.to_string(&self.context)).unwrap();
    }
    for i in 0..self.get_num_gvs() {
      let elem = self.get_gv(i);
      write!(f, "{}\n", elem.to_string(&self.context)).unwrap();
    }
    write!(f, "\n").unwrap();
    for i in 0..self.get_num_functions() {
      let func = self.get_function(i);
      write!(f, "{}", func.to_string(&self.context)).unwrap();
      // TODO(@were): More linkage policies
      write!(f, "\n\n").unwrap();
    }
    Ok(())
  }
}


