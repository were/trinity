use crate::context::component::GetSlabKey;
use crate::ir::types::{VoidType, TKindCode};
use crate::ir::value::consts::InlineAsm;
use crate::ir::value::instruction::{CastOp, InstOpcode, CmpPred};
use crate::ir::{
  module::Module,
  value::{ValueRef, VKindCode},
  types::FunctionType,
  value::block::Block,
  value::function::Function,
  value::function::{self, Argument},
  types::{self, StructType, TypeRef, PointerType},
  value::instruction::{self, Instruction, BinaryOp},
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

  pub fn get_current_block(&self) -> Option<ValueRef> {
    self.block.clone()
  }

  pub fn get_insert_before(&self) -> Option<ValueRef> {
    if let Some(idx) = self.inst_idx {
      let inst = self.block
        .clone()
        .unwrap()
        .as_ref::<Block>(&self.module.context)
        .unwrap()
        .get_inst(idx);
      inst
    } else {
      None
    }
  }

  pub fn context(&mut self) -> &mut Context {
    &mut self.module.context
  }

  /// Add a function to the module
  pub fn create_function(&mut self, name: String, fty: TypeRef) -> ValueRef {
    // Generate the arguments.
    let fty_ref = fty.as_mut::<FunctionType>(self.context()).unwrap();
    let fargs = Argument::from_fty(fty_ref);
    let mut arg_ptrs = Vec::new();
    for arg in fargs {
      arg_ptrs.push(self.context().add_instance(arg).skey);
    }
    // Create the function.
    let func = function::Function::new(name, fty, arg_ptrs);
    // Add the function to module.
    let func_ref = self.context().add_instance(func);
    self.module.functions.push(func_ref.skey);
    // Finalize the arguments.
    {
      let func = func_ref.as_ref::<Function>(&self.module.context).unwrap();
      let args = (0..func.get_num_args()).map(|i| { func.get_arg(i) }).collect::<Vec<_>>();
      args.iter().for_each(|arg| arg.as_mut::<Argument>(self.context()).unwrap().instance.parent = func_ref.skey);
    }
    func_ref
  }

  /// Get the current function to insert.
  pub fn get_current_function(&self) -> Option<ValueRef> {
    self.func.clone()
  }

  /// Set the current function to insert.
  pub fn set_current_function(&mut self, func: ValueRef) {
    assert!(func.kind == VKindCode::Function, "Given value is not a function");
    self.func = Some(func);
  }

  /// Add a block to the current function.
  pub fn add_block(&mut self, name: String) -> ValueRef {
    let func_ref = self.func.clone().unwrap();
    let block = Block::new(name, &func_ref);
    let block_ref = self.context().add_instance(block);
    let func = func_ref.as_mut::<Function>(self.context()).unwrap();
    func.basic_blocks_mut().push(block_ref.skey);
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
  pub fn set_insert_before(&mut self, inst_ref: ValueRef) {
    assert!(inst_ref.kind == VKindCode::Instruction, "Given value is not a instruction");
    let inst = inst_ref.as_ref::<Instruction>(&self.module.context).unwrap();
    let block = inst.get_parent();
    let block = block.as_ref::<Block>(&self.module.context).unwrap();
    let idx = block.inst_iter(&self.module.context).position(|i| i.get_skey() == inst_ref.skey).unwrap();
    self.inst_idx = Some(idx);
  }

  fn add_instruction(&mut self, mut inst: instruction::Instruction) -> ValueRef {
    let block_ref = self.block.clone().unwrap();
    inst.instance.parent = Some(block_ref.skey);
    let (insert_idx, closed) = {
      let block = block_ref.as_ref::<Block>(&self.module.context).unwrap();
      let (idx, last)  = if let Some(inst_idx) = self.inst_idx {
        (inst_idx, inst_idx == block.get_num_insts() - 1)
      } else {
        (block.get_num_insts(), true)
      };
      let closed_block = if last {
        block.closed(&self.module.context)
      } else {
        false
      };
      (idx, closed_block)
    };
    if closed {
      eprintln!("Instruction abandoned, because the block already has an end, branch instruction");
      ValueRef { skey: 0, kind: VKindCode::Unknown }
    } else {
      let inst_ref = self.context().add_instance(inst);
      let block = block_ref.as_mut::<Block>(&mut self.module.context).unwrap();
      block.instance.insts.insert(insert_idx, inst_ref.skey);
      inst_ref
    }
  }

  pub fn create_return(&mut self, val: Option<ValueRef>) -> ValueRef {
    let ret_ty = self.context().void_type();
    let inst = instruction::Instruction::new(
      ret_ty,
      instruction::InstOpcode::Return,
      "ret".to_string(),
      if let None = val { vec![] } else {vec![val.unwrap()]},
    );
    self.add_instruction(inst)
  }

  pub fn create_alloca(&mut self, ty: types::TypeRef) -> ValueRef {
    let ptr_ty = ty.ptr_type(self.context());
    let inst = instruction::Instruction::new(
      ptr_ty,
      // TODO(@were): Make this alignment better
      instruction::InstOpcode::Alloca(8),
      "alloca".to_string(),
      Vec::new(),
    );
    self.add_instruction(inst)
  }

  pub fn create_string(&mut self, val: String) -> ValueRef {
    let val = format!("{}\0", val);
    let size = val.len();
    let array_ty = self.context().int_type(8).array_type(self.context(), size);
    let i8ty = self.context().int_type(8);
    let init = val.chars().map(|x| { self.context().const_value(i8ty.clone(), x as u64) }).collect::<Vec<_>>();
    let name = "str".to_string();
    let res = array_ty.const_array(self.context(), name, init);
    self.module.global_values.push(res.clone());
    res
  }

  pub fn create_gep(&mut self, ty: TypeRef, ptr: ValueRef, indices: Vec<ValueRef>, inbounds: bool) -> ValueRef {
    let mut operands = vec![ptr];
    operands.extend(indices);
    // All constants
    if operands.iter().fold(true, |acc, val| acc && val.is_const()) {
      let res = ConstExpr::new(ty, instruction::InstOpcode::GetElementPtr(inbounds), operands);
      let expr = self.context().add_instance::<ConstExpr>(res);
      return expr
    } else {
      let inst = instruction::Instruction::new(
        ty,
        instruction::InstOpcode::GetElementPtr(inbounds),
        "gep".to_string(),
        operands,
      );
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
  pub fn create_store(&mut self, value: ValueRef, ptr: ValueRef) -> Result<ValueRef, String> {
    let ptr_ty = ptr.get_type(&self.context());
    let pointee_ty = ptr_ty.as_ref::<PointerType>(&self.context()).unwrap().get_pointee_ty();
    let value_ty = value.get_type(&self.context());
    if pointee_ty != value_ty {
      let pointee_ty = pointee_ty.to_string(&self.module.context);
      let value_ty = value_ty.to_string(&self.module.context);
      return Err(format!("PointerType: {} mismatches ValueType: {}", pointee_ty, value_ty))
    }
    let inst = instruction::Instruction::new(
      self.context().void_type(),
      instruction::InstOpcode::Store(8),
      "store".to_string(),
      vec![value, ptr],
    );
    Ok(self.add_instruction(inst))
  }

  pub fn create_typed_call(&mut self, ty: TypeRef, callee: ValueRef, args: Vec<ValueRef>) -> ValueRef {
    let mut args = args.clone();
    args.push(callee);
    let inst = instruction::Instruction::new(
      ty,
      instruction::InstOpcode::Call,
      "call".to_string(),
      args,
    );
    self.add_instruction(inst)
  }

  pub fn create_func_call(&mut self, callee: ValueRef, args: Vec<ValueRef>) -> ValueRef {
    let fty = callee.get_type(self.context());
    let ty = fty.as_ref::<FunctionType>(self.context()).unwrap().ret_ty().clone();
    self.create_typed_call(ty, callee, args)
  }

  pub fn create_binary_op(&mut self, op: BinaryOp, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
    // @were: Check type equality.
    let ty = lhs.get_type(self.context());
    let inst = instruction::Instruction::new(
      ty,
      instruction::InstOpcode::BinaryOp(op),
      "binop".to_string(),
      vec![lhs, rhs],
    );
    self.add_instruction(inst)
  }

  pub fn create_add(&mut self, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
    return self.create_binary_op(BinaryOp::Add, lhs, rhs)
  }

  pub fn create_sub(&mut self, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
    return self.create_binary_op(BinaryOp::Sub, lhs, rhs)
  }

  pub fn create_mul(&mut self, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
    return self.create_binary_op(BinaryOp::Mul, lhs, rhs)
  }

  pub fn create_sdiv(&mut self, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
    return self.create_binary_op(BinaryOp::SDiv, lhs, rhs)
  }

  pub fn create_srem(&mut self, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
    return self.create_binary_op(BinaryOp::SRem, lhs, rhs)
  }

  pub fn create_load(&mut self, ptr: ValueRef) -> ValueRef {
    let ty = ptr.get_type(self.context());
    let pty = ty.as_ref::<PointerType>(self.context()).unwrap();
    let res_ty = pty.get_pointee_ty();
    self.create_typed_load(res_ty, ptr)
  }

  // TODO(@were): Add alignment
  pub fn create_typed_load(&mut self, ty: TypeRef, ptr: ValueRef) -> ValueRef {
    let inst = instruction::Instruction::new(
      ty,
      instruction::InstOpcode::Load(8),
      "load".to_string(),
      vec![ptr],
    );
    self.add_instruction(inst)
  }

  pub fn create_global_struct(&mut self, ty: TypeRef, init: Vec<ValueRef>) -> ValueRef {
    let gvs = ConstObject::new(
      "globalobj".to_string(),
      ty.ptr_type(self.context()),
      init);
    let gvs_ref = self.context().add_instance(gvs);
    self.module.global_values.push(gvs_ref.clone());
    gvs_ref
  }

  pub fn create_inline_asm(&mut self, ty: TypeRef, mnemonic: String,
                           operands: String, sideeffect: bool) -> ValueRef {
    let mut sideeffect = sideeffect;
    if let Some(_) = ty.as_ref::<VoidType>(self.context()) {
      sideeffect = true;
    }
    let asm = InlineAsm::new(
      ty,
      sideeffect,
      mnemonic,
      operands,
    );
    self.context().add_instance(asm)
  }

  pub fn create_op_cast(&mut self, cast_op: CastOp, val: ValueRef, dest: TypeRef) -> ValueRef {
    // Skip casting if the same types.
    let src_ty = val.get_type(&self.context());
    if src_ty.skey == dest.skey {
      return val;
    }
    let op = InstOpcode::CastInst(cast_op);
    let inst = instruction::Instruction::new(
      dest,
      op,
      "cast".to_string(),
      vec![val],
    );
    self.add_instruction(inst)
  }

  pub fn create_bitcast(&mut self, val: ValueRef, dest: TypeRef) -> ValueRef {
    self.create_op_cast(CastOp::Bitcast, val, dest)
  }

  pub fn create_cast(&mut self, val: ValueRef, dest: TypeRef) -> ValueRef {
    let src_ty = val.get_type(&self.context());
    // TODO(@were): This is messy, fix this later.
    let cast_op = if src_ty.kind == TKindCode::IntType && dest.kind == TKindCode::IntType {
      let src_bits = src_ty.get_scalar_size_in_bits(&self.module);
      let dst_bits = dest.get_scalar_size_in_bits(&self.module);
      if src_bits < dst_bits {
        CastOp::SignExt
      } else if src_bits > dst_bits {
        CastOp::Trunc
      } else {
        return val
      }
    } else if (src_ty.kind == TKindCode::IntType && dest.kind == TKindCode::PointerType) ||
              (src_ty.kind == TKindCode::PointerType && dest.kind == TKindCode::IntType) {
      assert_eq!(src_ty.get_scalar_size_in_bits(&self.module), dest.get_scalar_size_in_bits(&self.module));
      CastOp::Bitcast
    } else {
      panic!("Not supported casting!");
    };
    self.create_op_cast(cast_op, val, dest)
  }

  pub fn create_compare(&mut self, pred: CmpPred, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
    let inst = instruction::Instruction::new(
      self.context().int_type(1),
      instruction::InstOpcode::ICompare(pred),
      "slt".to_string(),
      vec![lhs, rhs],
    );
    self.add_instruction(inst)
  }

  pub fn create_slt(&mut self, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
    return self.create_compare(CmpPred::SLT, lhs, rhs)
  }

  pub fn create_sgt(&mut self, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
    return self.create_compare(CmpPred::SGT, lhs, rhs)
  }

  pub fn create_sle(&mut self, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
    return self.create_compare(CmpPred::SLE, lhs, rhs)
  }

  pub fn create_sge(&mut self, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
    return self.create_compare(CmpPred::SGE, lhs, rhs)
  }

  pub fn create_eq(&mut self, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
    return self.create_compare(CmpPred::EQ, lhs, rhs)
  }

  fn add_block_predecessor(&mut self, bb: &ValueRef, pred: &ValueRef) {
    let bb = bb.as_mut::<Block>(&mut self.module.context).unwrap();
    bb.instance.predecessors.push(pred.skey);
  }

  pub fn create_unconditional_branch(&mut self, bb: ValueRef) -> ValueRef {
    assert!(bb.get_type(self.context()).kind == TKindCode::BlockType);
    let inst = instruction::Instruction::new(
      self.context().void_type(),
      instruction::InstOpcode::Branch,
      "br".to_string(),
      vec![bb.clone()],
    );
    let res = self.add_instruction(inst);
    match res.kind {
      VKindCode::Unknown => {},
      _ => { self.add_block_predecessor(&bb, &res); }
    }
    res
  }

  pub fn create_conditional_branch(&mut self, cond: ValueRef, true_bb: ValueRef, false_bb: ValueRef) -> ValueRef {
    let inst = instruction::Instruction::new(
      self.context().void_type(),
      instruction::InstOpcode::Branch,
      "br".to_string(),
      vec![cond, true_bb.clone(), false_bb.clone()],
    );
    let res = self.add_instruction(inst);
    match res.kind {
      VKindCode::Unknown => {},
      _ => {
        self.add_block_predecessor(&true_bb, &res);
        self.add_block_predecessor(&false_bb, &res);
      }
    }
    res
  }

  pub fn create_phi(&mut self, ty: TypeRef, operands: Vec<ValueRef>) -> ValueRef {
    let inst = instruction::Instruction::new(
      ty,
      instruction::InstOpcode::Phi,
      "phi".to_string(),
      operands,
    );
    self.add_instruction(inst)
  }

}

