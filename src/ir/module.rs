use std::fmt;

use slab::Slab;

use super::block;
use super::function;
use super::value;
use super::types;
use super::instruction;
use super::value::Argument;
use super::types::StructType;

pub struct Module {
  /// The name of the module.
  mod_name: String,
  /// The source code file name.
  src_name: String,
  /// All the components of a module is managed by slabs.
  /// All the functions in this module.
  pub(crate) func_buffer: Slab<function::Function>,
  /// This a redundant data structure to maintain the uniformity of the interfaces,
  /// across all the levels of the IR. The keys to index the functions in the slab.
  pub(crate) func_keys: Vec<usize>,
  /// All the structs in this module.
  pub(crate) struct_buffer: Slab<types::StructType>,
  /// All the blocks in this module.
  pub(crate) block_buffer: Slab<block::Block>,
  /// All the instructions in this module.
  pub(crate) inst_buffer: Slab<instruction::Instruction>,
  /// All the function arguments in this module.
  pub(crate) arg_buffer: Slab<value::Argument>,
}

impl<'ctx> Module {

  /// Construct a module
  pub fn new(mod_name: String, src_name: String) -> Module {
    Module {
      mod_name,
      src_name,
      func_buffer: Slab::new(),
      func_keys: Vec::new(),
      struct_buffer: Slab::new(),
      block_buffer: Slab::new(),
      inst_buffer: Slab::new(),
      arg_buffer: Slab::new(),
    }
  }

  /// Add a struct without body to the module
  pub fn add_struct_decl(&mut self, name: String) {
    self.struct_buffer.insert(types::StructType::new(name));
  }

  /// Get the struct reference by name
  pub fn get_struct(&'ctx self, name: &String) -> Option<&'ctx types::StructType> {
    for (_, elem) in self.struct_buffer.iter() {
      if elem.name == *name {
        return Some(elem);
      }
    }
    None
  }

  /// Get the mutable struct reference by name
  pub fn get_struct_mut(&'ctx mut self, name: &String) -> Option<&'ctx mut types::StructType> {
    for (_, elem) in self.struct_buffer.iter_mut() {
      if elem.name == *name {
        return Some(elem);
      }
    }
    None
  }

  /// The number of functions in the module.
  pub fn get_num_functions(&self) -> usize {
    self.func_buffer.len()
  }

  /// Get the function by indices.
  pub fn get_function(&'ctx self, idx: usize) -> &'ctx function::Function {
    self.func_buffer.get(self.func_keys[idx]).unwrap()
  }

  pub fn get_function_mut(&'ctx mut self, idx: usize) -> &'ctx mut function::Function {
    &mut self.func_buffer[self.func_keys[idx]]
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
    for (_, elem) in &self.struct_buffer {
      write!(f, "{}\n", elem.to_string()).unwrap();
    }
    write!(f, "\n").unwrap();
    for (_, func) in &self.func_buffer {
      // TODO(@were): More linkage policies
      write!(f, "define dso_local {} @{}(", func.fty.ret_ty.as_ref(), namify(&func.name)).unwrap();
      for i in 0..func.get_num_args() {
        if i != 0 {
          write!(f, ", ").unwrap();
        }
        let arg_ref = func.get_arg(i);
        let arg = arg_ref.as_typed_ref::<Argument>(self).unwrap();
        write!(f, "{} {}", arg.ty, &arg.name()).unwrap();
      }
      write!(f, ")").unwrap();
      if func.blocks.len() != 0 {
        write!(f, " {{\n").unwrap();
        for i in 0..func.get_num_blocks() {
          let block_ref = func.get_block(i);
          let block = block_ref.as_typed_ref::<block::Block>(self).unwrap();
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


