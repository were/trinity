use std::fmt;


use crate::context::Context;

use super::block;
use super::function;
use super::value::Argument;
use super::types::StructType;

pub struct Module {
  /// Context of this module.
  pub(crate) context: Context,
  /// The name of the module.
  mod_name: String,
  /// The source code file name.
  src_name: String,
  /// The function keys in the context slab.
  functions: Vec<usize>,
  /// The struct keys in the context slab.
  structs: Vec<usize>
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

  /// Get the function by indices.
  pub fn get_function(&'ctx self, idx: usize) -> &'ctx function::Function {
    self.context.get_value_ref::<function::Function>(self.functions[idx])
  }

  pub fn get_function_mut(&'ctx mut self, idx: usize) -> &'ctx mut function::Function {
    self.context.get_value_mut::<function::Function>(self.functions[idx])
  }

}

fn namify(name: &String) -> String {
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
    write!(f, "\n").unwrap();
    for i in 0..self.get_num_functions() {
      let func = self.get_function(i);
      // TODO(@were): More linkage policies
      write!(f, "define dso_local {} @{}(", func.fty.ret_ty.to_string(&self.context), namify(&func.name)).unwrap();
      for i in 0..func.get_num_args() {
        if i != 0 {
          write!(f, ", ").unwrap();
        }
        let arg_ref = func.get_arg(i);
        let arg = arg_ref.as_ref::<Argument>(self).unwrap();
        write!(f, "{}", arg.to_string(&self.context)).unwrap();
      }
      write!(f, ")").unwrap();
      if func.blocks.len() != 0 {
        write!(f, " {{\n").unwrap();
        for i in 0..func.get_num_blocks() {
          let block_ref = func.get_block(i);
          let block = block_ref.as_ref::<block::Block>(self).unwrap();
          write!(f, "{}:\n", block.name).unwrap();
        }
        write!(f, "}}").unwrap();
      } else {
        write!(f, ";").unwrap();
      }
      write!(f, "\n\n").unwrap();
    }
    Ok(())
  }
}


