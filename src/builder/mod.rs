
use crate::ir::{
  module::Module,
  value::{ValueRef, VKindCode},
  types::FunctionType,
  block::Block,
  function::Function,
  function::{self, Argument},
  types::{self, StructType, AsTypeRef, TypeRef, PointerType},
  instruction::{self, Instruction}
};

use crate::context::Context;


pub struct Builder {
  pub module: Module,
  func: Option<ValueRef>,
  block: Option<ValueRef>,
  inst_idx: Option<usize>,
}

impl<'ctx> Builder {

  pub fn new(module: Module) -> Builder {
    Builder { module, func: None, block: None, inst_idx: None }
  }

  pub fn context(&mut self) -> &mut Context {
    &mut self.module.context
  }

  /// Add a function to the module
  pub fn add_function(&mut self, name: String, fty_ref: TypeRef) -> ValueRef {
    // Create the function.
    let func = function::Function {
      skey: None,
      name, args: Vec::new(), fty: fty_ref.clone(),
      blocks: Vec::new(),
    };
    // Add the function to module.
    let skey = self.context().add_component(func.into());
    let args = fty_ref.as_ref::<FunctionType>(self.context()).unwrap().args.clone();
    self.module.functions.push(skey);
    let fidx = self.module.get_num_functions() - 1;
    // Generate the arguments.
    let fargs: Vec<usize> = args.iter().enumerate().map(|(i, ty)| {
       let arg = Argument {
         skey: None,
         ty: ty.clone(),
         arg_idx: i,
         parent: skey
       };
       let arg_ref = self.context().add_component(arg.into());
       arg_ref
    }).collect();
    // Finalize the arguments.
    let func = self.module.get_function_mut(fidx);
    func.args = fargs;
    func.as_ref()
  }

  /// Set the current function to insert.
  pub fn set_current_function(&mut self, func: ValueRef) {
    assert!(func.v_kind == VKindCode::Function, "Given value is not a function");
    self.func = Some(func);
  }

  /// Add a block to the current function.
  pub fn add_block(&mut self, name: String) -> ValueRef {
    let block_name = if name != "" { name } else { format!("block{}", self.context().num_components()) };
    let func_ref = self.func.clone().unwrap();
    let skey = self.context().add_component(Block{
      skey: None,
      name: block_name,
      insts: Vec::new(),
      parent: func_ref.skey,
    }.into());
    let func = func_ref.as_mut::<Function>(self.context()).unwrap();
    func.blocks.push(skey);
    let block = self.context().get_value_mut::<Block>(skey);
    block.as_ref()
  }

  /// Add a struct declaration to the context.
  pub fn create_struct(&mut self, name: String) -> types::TypeRef {
    let skey = self.context().add_component(StructType::new(name).into());
    self.module.structs.push(skey);
    let sty_mut = self.context().get_value_mut::<StructType>(skey);
    sty_mut.as_type_ref()
  }

  /// Set the current block to insert.
  pub fn set_current_block(&mut self, block: ValueRef) {
    assert!(block.v_kind == VKindCode::Block, "Given value is not a block");
    self.block = Some(block);
    self.inst_idx = None
  }

  /// Set the instruction as the insert point.
  pub fn set_insert_point(&mut self, inst_ref: ValueRef) {
    assert!(inst_ref.v_kind == VKindCode::Instruction, "Given value is not a instruction");
    let inst = inst_ref.as_ref::<Instruction>(&self.module.context).unwrap();
    let block = inst.parent.as_ref::<Block>(&self.module.context).unwrap();
    let idx = block.insts.iter().position(|i| *i == inst_ref.skey).unwrap();
    self.inst_idx = Some(idx);
  }

  fn add_instruction(&mut self, inst: instruction::Instruction) -> ValueRef {
    let block_ref = self.block.clone().unwrap();
    let skey = self.context().add_component(inst.into());
    let inst_ref = {
      let inst = self.context().get_value_mut::<Instruction>(skey);
      inst.parent = block_ref.clone();
      inst.as_ref()
    };
    let block = block_ref.as_mut::<Block>(&mut self.module.context).unwrap();
    if let Some(inst_idx) = self.inst_idx {
      block.insts.insert(inst_idx, skey);
    } else {
      block.insts.push(skey);
    }
    inst_ref
  }

  pub fn create_return(&mut self, val: Option<ValueRef>) -> ValueRef {
    let ret_ty = self.context().void_type();
    let inst = instruction::Instruction {
      skey: None,
      ty: ret_ty,
      opcode: instruction::InstOpcode::Return,
      name: format!("ret.{}", self.context().num_components()),
      operands: if let None = val { vec![] } else {vec![val.unwrap()]},
      parent: ValueRef{skey: 0, v_kind: VKindCode::Unknown}
    };
    self.add_instruction(inst)
  }

  pub fn alloca(&mut self, ty: types::TypeRef) -> ValueRef {
    let inst = instruction::Instruction {
      skey: None,
      ty,
      // TODO(@were): Make this alignment better
      opcode: instruction::InstOpcode::Alloca(8),
      name: format!("alloca.{}", self.context().num_components()),
      operands: Vec::new(),
      parent: ValueRef{skey: 0, v_kind: VKindCode::Unknown}
    };
    self.add_instruction(inst)
  }

  pub fn create_string(&mut self, val: String) -> ValueRef {
    let val = format!("{}\0", val);
    let size = self.context().int_type(32).const_value(self.context(), val.len() as u64);
    let array_ty = self.context().int_type(8).array_type(self.context(), size);
    let id = self.context().num_components();
    let res = array_ty.const_array(self.context(), format!("str.{}", id), val.into_bytes());
    self.module.global_values.push(res.skey);
    res
  }

  pub fn create_gep(&mut self, ptr: ValueRef, indices: Vec<ValueRef>, inbounds: bool) -> ValueRef {
    let ty = ptr.get_type(self.context());
    let pty = ty.as_ref::<PointerType>(self.context()).unwrap();
    let res_ty = pty.get_scalar_ty();
    let inst = instruction::Instruction {
      skey: None,
      ty: res_ty,
      opcode: instruction::InstOpcode::GetElementPtr(inbounds),
      name: format!("gep.{}", self.context().num_components()),
      operands: indices,
      parent: ValueRef{skey: 0, v_kind: VKindCode::Unknown}
    };
    self.add_instruction(inst)
  }

  pub fn create_inbounds_gep(&mut self, ptr: ValueRef, indices: Vec<ValueRef>) -> ValueRef {
    self.create_gep(ptr, indices, true)
  }

}

