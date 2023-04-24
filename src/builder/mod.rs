use crate::context::component::AsSuper;

use crate::ir::types::VoidType;
use crate::ir::value::consts::InlineAsm;
use crate::ir::{
  module::Module,
  value::{ValueRef, VKindCode},
  types::FunctionType,
  value::block::Block,
  value::function::Function,
  value::function::{self, Argument},
  types::{self, StructType, TypeRef, PointerType},
  value::instruction::{self, Instruction},
  value::consts::{ConstExpr, ConstObject}
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
    let func_ref = self.context().add_instance(func);
    let args = fty_ref.as_ref::<FunctionType>(self.context()).unwrap().args.clone();
    self.module.functions.push(func_ref.skey);
    let fidx = self.module.get_num_functions() - 1;
    // Generate the arguments.
    let fargs: Vec<usize> = args.iter().enumerate().map(|(i, ty)| {
       let arg = Argument {
         skey: None,
         ty: ty.clone(),
         arg_idx: i,
         parent: func_ref.skey
       };
       self.context().add_instance(arg).skey
    }).collect();
    // Finalize the arguments.
    let func = self.module.get_function_mut(fidx);
    func.args = fargs;
    func.as_super()
  }

  /// Set the current function to insert.
  pub fn set_current_function(&mut self, func: ValueRef) {
    assert!(func.kind == VKindCode::Function, "Given value is not a function");
    self.func = Some(func);
  }

  /// Add a block to the current function.
  pub fn add_block(&mut self, name: String) -> ValueRef {
    let block_name = if name != "" { name } else { format!("block{}", self.context().num_components()) };
    let func_ref = self.func.clone().unwrap();
    let block = Block{
      skey: None,
      name: block_name,
      insts: Vec::new(),
      parent: func_ref.skey,
    };
    let block_ref = self.context().add_instance(block);
    let func = func_ref.as_mut::<Function>(self.context()).unwrap();
    func.blocks.push(block_ref.skey);
    block_ref
  }

  /// Add a struct declaration to the context.
  pub fn create_struct(&mut self, name: String) -> types::TypeRef {
    let sty_ref = self.context().add_instance(StructType::new(name));
    self.module.structs.push(sty_ref.skey);
    sty_ref
  }

  /// Set the current block to insert.
  pub fn set_current_block(&mut self, block: ValueRef) {
    assert!(block.kind == VKindCode::Block, "Given value is not a block");
    self.block = Some(block);
    self.inst_idx = None
  }

  /// Set the instruction as the insert point.
  pub fn set_insert_point(&mut self, inst_ref: ValueRef) {
    assert!(inst_ref.kind == VKindCode::Instruction, "Given value is not a instruction");
    let inst = inst_ref.as_ref::<Instruction>(&self.module.context).unwrap();
    let block = inst.parent.as_ref::<Block>(&self.module.context).unwrap();
    let idx = block.insts.iter().position(|i| *i == inst_ref.skey).unwrap();
    self.inst_idx = Some(idx);
  }

  fn add_instruction(&mut self, inst: instruction::Instruction) -> ValueRef {
    let block_ref = self.block.clone().unwrap();
    let inst_ref = self.context().add_instance(inst);
    let block = block_ref.as_mut::<Block>(&mut self.module.context).unwrap();
    if let Some(inst_idx) = self.inst_idx {
      block.insts.insert(inst_idx, inst_ref.skey);
    } else {
      block.insts.push(inst_ref.skey);
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
      parent: ValueRef{skey: 0, kind: VKindCode::Unknown}
    };
    self.add_instruction(inst)
  }

  pub fn alloca(&mut self, ty: types::TypeRef) -> ValueRef {
    let ptr_ty = ty.ptr_type(self.context());
    let inst = instruction::Instruction {
      skey: None,
      ty: ptr_ty,
      // TODO(@were): Make this alignment better
      opcode: instruction::InstOpcode::Alloca(8),
      name: format!("alloca.{}", self.context().num_components()),
      operands: Vec::new(),
      parent: ValueRef{skey: 0, kind: VKindCode::Unknown}
    };
    self.add_instruction(inst)
  }

  pub fn create_string(&mut self, val: String) -> ValueRef {
    let val = format!("{}\0", val);
    let ty = self.context().int_type(32);
    let size = self.context().const_value(ty, val.len() as u64);
    let array_ty = self.context().int_type(8).array_type(self.context(), size);
    let id = self.context().num_components();
    let res = array_ty.const_array(self.context(), format!("str.{}", id), val.into_bytes());
    self.module.global_values.push(res.clone());
    res
  }

  pub fn create_gep(&mut self, ty: TypeRef, ptr: ValueRef, indices: Vec<ValueRef>, inbounds: bool) -> ValueRef {
    let mut operands = vec![ptr];
    operands.extend(indices);
    // All constants
    if operands.iter().fold(true, |acc, val| acc && val.is_const()) {
      let res = ConstExpr{
        skey: None,
        ty,
        opcode: instruction::InstOpcode::GetElementPtr(inbounds),
        operands,
      };
      let expr = self.context().add_instance::<ConstExpr, _>(res);
      return expr
    } else {
      let inst = instruction::Instruction {
        skey: None,
        ty,
        opcode: instruction::InstOpcode::GetElementPtr(inbounds),
        name: format!("gep.{}", self.context().num_components()),
        operands,
        parent: ValueRef{skey: 0, kind: VKindCode::Instruction}
      };
      self.add_instruction(inst)
    }
  }

  pub fn create_inbounds_gep(&mut self, ptr: ValueRef, indices: Vec<ValueRef>) -> ValueRef {
    let ty = ptr.get_type(self.context());
    let pty = ty.as_ref::<PointerType>(self.context()).unwrap();
    let res_ty = pty.get_pointee_ty();
    self.create_gep(res_ty, ptr, indices, true)
  }

  // TODO(@were): Add alignment
  pub fn create_store(&mut self, value: ValueRef, ptr: ValueRef) {
    let inst = instruction::Instruction {
      skey: None,
      ty: self.context().void_type(),
      opcode: instruction::InstOpcode::Store(8),
      name: format!("store.{}", self.context().num_components()),
      operands: vec![value, ptr],
      parent: ValueRef{skey: 0, kind: VKindCode::Instruction}
    };
    self.add_instruction(inst);
  }

  pub fn create_typed_call(&mut self, ty: TypeRef, callee: ValueRef, args: Vec<ValueRef>) -> ValueRef {
    let mut args = args.clone();
    args.push(callee);
    let inst = instruction::Instruction{
      skey: None,
      ty,
      opcode: instruction::InstOpcode::Call,
      name: format!("call.{}", self.context().num_components()),
      operands: args,
      parent: self.block.clone().unwrap()
    };
    self.add_instruction(inst)
  }

  pub fn create_func_call(&mut self, callee: ValueRef, args: Vec<ValueRef>) -> ValueRef {
    let fty = callee.get_type(self.context());
    let ty = fty.as_ref::<FunctionType>(self.context()).unwrap().ret_ty.clone();
    self.create_typed_call(ty, callee, args)
  }


  pub fn create_load(&mut self, ptr: ValueRef) -> ValueRef {
    let ty = ptr.get_type(self.context());
    let pty = ty.as_ref::<PointerType>(self.context()).unwrap();
    let res_ty = pty.get_pointee_ty();
    self.create_typed_load(res_ty, ptr)
  }

  // TODO(@were): Add alignment
  pub fn create_typed_load(&mut self, ty: TypeRef, ptr: ValueRef) -> ValueRef {
    let inst = instruction::Instruction {
      skey: None,
      ty,
      opcode: instruction::InstOpcode::Load(8),
      name: format!("load.{}", self.context().num_components()),
      operands: vec![ptr],
      parent: ValueRef{skey: 0, kind: VKindCode::Instruction}
    };
    self.add_instruction(inst)
  }

  pub fn create_global_struct(&mut self, ty: TypeRef, init: Vec<ValueRef>) -> ValueRef {
    let gvs = ConstObject {
      skey: None,
      name: format!("globalobj.{}", self.context().num_components()),
      ty: ty.ptr_type(self.context()),
      value: init
    };
    let gvs_ref = self.context().add_instance(gvs);
    self.module.global_values.push(gvs_ref.clone());
    gvs_ref
  }

  pub fn create_inline_asm(&mut self, ty: TypeRef, mnemonic: String, operands: String, sideeffect: bool) -> ValueRef {
    let mut sideeffect = sideeffect;
    if let Some(_) = ty.as_ref::<VoidType>(self.context()) {
      sideeffect = true;
    }
    let asm = InlineAsm {
      skey: None,
      ty,
      sideeffect,
      mnemonic,
      operands,
    };
    self.context().add_instance(asm)
  }

}

