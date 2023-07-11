use std::fmt;


use crate::context::{Context, Reference};
use crate::context::component::{AsSuper, GetSlabKey};
use crate::machine::{TargetTriple, DataLayout, TargetMachine};

use super::{Function, Instruction, Block};
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

  /// Remove the given instruction.
  pub fn remove_inst(&'ctx mut self, v: ValueRef, dispose: bool) -> Option<ValueRef> {
    let inst = v.as_ref::<Instruction>(&self.context).unwrap();
    let inst_ref = Reference::new(inst.get_skey(), &self.context, inst);
    let block = inst_ref.get_parent().as_super();
    let block = block.as_mut::<Block>(&mut self.context).unwrap();
    block.instance.insts.retain(|x| *x != v.skey);
    if dispose {
      self.context.dispose(v.skey);
      None
    } else {
      Some(v)
    }
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
    self.context.get_value_ref::<function::Function>(self.functions[idx])
  }

  pub fn iter(&'ctx self) -> ModuleFuncIter<'ctx> {
    return ModuleFuncIter{i: 0, module: self}
  }


  /// Replace old instruction with new value.
  pub fn replace_all_uses_with(&mut self, old: ValueRef, new: ValueRef) -> bool {
    let old_inst = old.as_ref::<Instruction>(&self.context).unwrap();
    let old_inst_ref = Reference::new(old.skey, &self.context, old_inst);
    let old_parent = old_inst_ref.get_parent();
    let old_parent = Reference::new(old_parent.get_skey(), &self.context, old_parent);
    let func = old_parent.get_parent();
    let func = func.as_ref::<Function>(&self.context).unwrap();
    let func = Reference::new(func.get_skey(), &self.context, func);
    let to_replace = func.iter().map(|block| {
      let block = Reference::new(block.get_skey(), &self.context, block);
      for inst in block.inst_iter() {
        for i in 0..inst.get_num_operands() {
          if inst.get_operand(i).unwrap().skey == old.skey {
            return Some((Instruction::from_skey(inst.skey), i))
          }
        }
      }
      None
    }).collect::<Vec<_>>();
    let res = to_replace.iter().fold(false, |_, elem| {
      if let Some((inst, idx)) = elem {
        let inst = inst.as_mut::<Instruction>(&mut self.context).unwrap();
        inst.set_operand(*idx, new.clone());
        true
      } else {
        false
      }
    });
    old.as_mut::<Instruction>(&mut self.context)
      .unwrap()
      .instance
      .users
      .clear();
    return res;
  }

}

pub struct ModuleFuncIter <'ctx> {
  i: usize,
  module: &'ctx Module
}

impl<'ctx> Iterator for ModuleFuncIter<'ctx> {

  type Item = &'ctx Function;

  fn next(&mut self) -> Option<Self::Item> {
    if self.i < self.module.functions.len() {
      let skey = self.module.functions[self.i];
      self.i += 1;
      Some(self.module.context.get_value_ref::<Function>(skey))
    } else {
      None
    }
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
    write!(f, "source_filename = \"{}\"\n", self.src_name).unwrap();
    write!(f, "target triple = \"{}\"\n", self.tm.target_triple.to_string()).unwrap();
    write!(f, "target datalayout = \"{}\"\n", self.tm.data_layout.to_string()).unwrap();
    write!(f, "\n").unwrap();
    for i in 0..self.num_structs() {
      let elem = self.get_struct(i);
      write!(f, "{}\n", elem.to_string(&self.context)).unwrap();
    }
    for i in 0..self.get_num_gvs() {
      let elem = self.get_gv(i);
      match elem.kind {
        super::VKindCode::ConstArray => {
          let array = elem.as_ref::<ConstArray>(&self.context).unwrap();
          write!(f, "{}\n", array.to_string(&self.context)).unwrap();
        }
        super::VKindCode::ConstObject => {
          let obj = elem.as_ref::<ConstObject>(&self.context).unwrap();
          write!(f, "{}\n", obj.to_string(&self.context)).unwrap();
        }
        _ => (),
      }
    }
    write!(f, "\n").unwrap();
    for i in 0..self.get_num_functions() {
      let func = self.get_function(i);
      let func = Reference::new(func.get_skey(), &self.context, func);
      write!(f, "{}", func.to_string()).unwrap();
      // TODO(@were): More linkage policies
      write!(f, "\n\n").unwrap();
    }
    Ok(())
  }
}


