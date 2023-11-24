use crate::ir::types::{VoidType, TKindCode};
use crate::ir::value::consts::InlineAsm;
use crate::ir::value::instruction::const_folder::{fold_binary_op, fold_cmp_op};
use crate::ir::value::instruction::{CastOp, InstOpcode, CmpPred, InstMutator};
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
  inst: Option<ValueRef>,
}

impl<'ctx> Builder {

  pub fn new(module: Module) -> Builder {
    Builder { module, func: None, block: None, inst: None }
  }

  pub fn get_current_block(&self) -> Option<ValueRef> {
    self.block.clone()
  }

  pub fn get_insert_before(&self) -> Option<ValueRef> {
    if let Some(_) = self.inst {
      let block = self.block
        .clone()
        .unwrap()
        .as_ref::<Block>(&self.module.context)
        .unwrap();
      block.get_inst(self.inst_idx()).map(|i| Instruction::from_skey(i.get_skey()))
    } else {
      None
    }
  }

  pub fn context(&mut self) -> &mut Context {
    &mut self.module.context
  }

  /// Add a function to the module
  pub fn create_function(&mut self, name: &String, fty: &TypeRef) -> ValueRef {
    // Generate the arguments.
    let fty_ref = fty.as_mut::<FunctionType>(self.context()).unwrap();
    let fargs = Argument::from_fty(fty_ref);
    let mut arg_ptrs = Vec::new();
    for arg in fargs {
      arg_ptrs.push(self.context().add_instance(arg).skey);
    }
    // Create the function.
    let func = function::Function::new(name.clone(), fty.clone(), arg_ptrs);
    // Add the function to module.
    let func_ref = self.context().add_instance(func);
    self.module.functions.push(func_ref.skey);
    // Finalize the arguments.
    {
      let func = func_ref.as_ref::<Function>(&self.module.context).unwrap();
      let args = (0..func.get_num_args()).map(|i| { func.get_arg(i) }).collect::<Vec<_>>();
      let set_key = |arg: &ValueRef| {
        arg.as_mut::<Argument>(self.context()).unwrap().instance.parent = func_ref.skey;
      };
      args.iter().for_each(set_key);
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
  pub fn create_block(&mut self, name: String) -> ValueRef {
    let func_ref = self.func.clone().unwrap();
    let block = Block::new(name, &func_ref);
    let block_ref = self.context().add_instance(block);
    let func = func_ref.as_mut::<Function>(self.context()).unwrap();
    func.basic_blocks_mut().push(block_ref.skey);
    block_ref
  }

  /// Split a block A into two blocks.
  /// A.0 ... value ... jmp A.rest
  /// A.rest ... rest of A
  pub fn split_block(&mut self, value: &ValueRef) -> ValueRef {
    // Restore them later.
    let old_block = self.get_current_block();
    let old_idx = self.get_insert_before();
    let old_func = self.get_current_function();

    let inst = value.as_ref::<Instruction>(&self.module.context).unwrap();
    let block_ref = inst.get_parent();
    let block = block_ref.as_super();
    let idx = block_ref.inst_iter().position(|i| i.get_skey() == value.skey).unwrap();
    let name = block_ref.get_name();
    let rest_slice = ((idx+1)..block_ref.get_num_insts())
      .map(|i| block_ref.get_inst(i).unwrap().as_super())
      .collect::<Vec<_>>();
    let res = self.create_block(format!("{}.rest", name));
    for inst in rest_slice.into_iter() {
      let mut mutator = InstMutator::new(self.context(), &inst);
      mutator.move_to_block(&res, None);
    }
    self.set_current_block(block);
    self.create_unconditional_branch(res.clone());

    // TODO(@were): Make this a function later.
    // Restore the old insert points.
    if old_block.is_some() {
      self.set_current_block(old_block.unwrap());
    }
    if old_idx.is_some() {
      self.set_insert_before(old_idx.unwrap());
    }
    if old_func.is_some() {
      self.set_current_function(old_func.unwrap());
    }
    res
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
    self.inst = None;
  }

  /// Set the instruction as the insert point.
  pub fn set_insert_before(&mut self, inst_ref: ValueRef) {
    assert!(inst_ref.kind == VKindCode::Instruction, "Given value is not a instruction");
    self.inst = Some(inst_ref);
  }

  fn inst_idx(&self) -> usize {
    if let Some(inst) = &self.inst {
      let inst = inst.as_ref::<Instruction>(&self.module.context).unwrap();
      let block = inst.get_parent();
      let idx = block.inst_iter().position(|i| i.get_skey() == inst.get_skey()).unwrap();
      return idx;
    }
    let bb = self.block.clone().unwrap().as_ref::<Block>(&self.module.context).unwrap();
    return bb.get_num_insts();
  }

  fn add_instruction(&mut self, mut inst: instruction::Instruction) -> ValueRef {
    let block_ref = self.block.clone().unwrap();
    inst.instance.parent = Some(block_ref.skey);
    let (insert_idx, closed) = {
      let block = block_ref.as_ref::<Block>(&self.module.context).unwrap();
      let (idx, last) = {
        let inst_idx = self.inst_idx();
        (inst_idx, inst_idx == block.get_num_insts())
      };
      let closed_block = if last {
        block.closed()
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
      let inst_value = Instruction::from_skey(inst_ref.skey);
      // Maintain the instruction redundancy.
      let inst_ref = inst_ref.as_ref::<Instruction>(&self.module.context).unwrap();
      let operands = inst_ref.operand_iter().map(|v| v.clone()).collect::<Vec<_>>();
      self.module.context.add_user_redundancy(
        &inst_value,
        operands.iter().enumerate().map(|(i, v)| (v.clone(), i)).collect::<Vec<_>>());
      let block = block_ref.as_mut::<Block>(&mut self.module.context).unwrap();
      block.instance.insts.insert(insert_idx, inst_value.skey);
      inst_value.clone()
    }
  }

  pub fn create_instruction(
    &mut self, ty: TypeRef, op: InstOpcode, operands: Vec<ValueRef>, name: String) -> ValueRef {
    let inst = instruction::Instruction::new(ty, op, name, operands);
    self.add_instruction(inst)
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

  pub fn create_alloca(&mut self, ty: types::TypeRef, name: String) -> ValueRef {
    let ptr_ty = ty.ptr_type(self.context());
    let inst = instruction::Instruction::new(
      ptr_ty,
      // TODO(@were): Make this alignment better
      instruction::InstOpcode::Alloca(8),
      name,
      Vec::new(),
    );
    self.add_instruction(inst)
  }

  pub fn create_string(&mut self, val: String) -> ValueRef {
    let val = format!("{}\0", val);
    let size = val.len();
    let array_ty = self.context().int_type(8).array_type(self.context(), size);
    let i8ty = self.context().int_type(8);
    let f = |x| { self.context().const_value(i8ty.clone(), x as u64) };
    let init = val.chars().map(f).collect::<Vec<_>>();
    let name = "str".to_string();
    let res = array_ty.const_array(self.context(), name, init);
    self.module.global_values.push(res.clone());
    res
  }

  /// Return the pointer to the struct field.
  pub fn get_struct_field(
    &mut self,
    value: ValueRef,
    idx: usize,
    name: &str) -> Result<ValueRef, String> {
    let ty = value.get_type(&self.module.context);
    let (attr_ty, name) = if let Some(ty) = ty.as_ref::<PointerType>(&self.module.context) {
      if let Some(sty) = ty.get_pointee_ty().as_ref::<StructType>(&self.module.context) {
        let res_ty = sty.get_attr(idx);
        if name.is_empty() {
          (res_ty, format!("{}.{}", sty.get_name(), idx))
        } else {
          (res_ty, name.to_string())
        }
      } else {
        return Err(format!("Expect a pointer to struct, but got {}", ty.to_string()));
      }
    } else {
      let ty_str = ty.to_string(&self.module.context);
      return Err(format!("Only pointer type supported for now, but got {}", ty_str));
    };
    let i32ty = self.context().int_type(32);
    let zero = self.context().const_value(i32ty.clone(), 0);
    let idx = self.context().const_value(i32ty, idx as u64);
    let ptr_ty = self.context().pointer_type(attr_ty);
    return Ok(self.create_gep(ptr_ty, value, vec![zero, idx], true, name));
  }

  /// Return the pointer to a[i]
  pub fn index_array(&mut self, a: ValueRef, i: ValueRef) -> Result<ValueRef, String> {
    let ty = a.get_type(&self.module.context);
    return Ok(self.create_gep(ty, a, vec![i], true, "a.i".to_string()));
  }

  /// This interface is not recommended to use, because its excessive parameters are error-prone.
  /// Please use `get_struct_field`, and `index_array` above instead.
  pub fn create_gep(
    &mut self, ty: TypeRef,
    ptr: ValueRef,
    indices: Vec<ValueRef>,
    inbounds: bool,
    name: String) -> ValueRef {
    let name = if name.is_empty() {
      "gep".to_string()
    } else {
      name
    };
    let mut operands = vec![ptr];
    operands.extend(indices);
    // All constants
    if operands[0].is_const() && operands.iter().fold(true, |acc, val| acc && val.is_const()) {
      let res = ConstExpr::new(ty, instruction::InstOpcode::GetElementPtr(inbounds), operands);
      let expr = self.context().add_instance::<ConstExpr>(res);
      return expr
    } else {
      let inst = instruction::Instruction::new(
        ty,
        instruction::InstOpcode::GetElementPtr(inbounds),
        name,
        operands,
      );
      self.add_instruction(inst)
    }
  }

  pub fn create_inbounds_gep(
    &mut self,
    ptr: ValueRef,
    indices: Vec<ValueRef>, name: String) -> ValueRef {
    let ty = ptr.get_type(self.context());
    let pty = ty.as_ref::<PointerType>(&self.module.context).unwrap();
    let res_ty = pty.get_pointee_ty();
    self.create_gep(res_ty, ptr, indices, true, name)
  }

  // TODO(@were): Add alignment
  pub fn create_store(&mut self, value: ValueRef, ptr: ValueRef) -> Result<ValueRef, String> {
    let ptr_ty = ptr.get_type(&self.module.context);
    if let Some(ptr_ty) = ptr_ty.as_ref::<PointerType>(&self.module.context) {
      let pointee_ty = ptr_ty.get_pointee_ty();
      let value_ty = value.get_type(&self.context());
      if pointee_ty != value_ty {
        return Err(format!("PointerType: {} ({}) mismatches ValueType: {} ({})",
          pointee_ty.to_string(&self.module.context),
          pointee_ty.skey,
          value_ty.to_string(&self.module.context),
          value_ty.skey))
      }
      let inst = instruction::Instruction::new(
        self.context().void_type(),
        instruction::InstOpcode::Store(8),
        "store".to_string(),
        vec![value, ptr],
      );
      Ok(self.add_instruction(inst))
    } else {
      Err(format!("Value: {} is not a pointer", ptr.to_string(&self.module.context, true)))
    }
  }

  pub fn create_typed_call(
    &mut self, ty: TypeRef,
    callee: ValueRef,
    args: Vec<ValueRef>) -> ValueRef {
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

  pub fn create_binary_op(&mut self, op: BinaryOp, lhs: ValueRef, rhs: ValueRef, name: String)
    -> ValueRef {
    // @were: Check type equality.
    let lty = lhs.get_type(&self.module.context);
    let rty = rhs.get_type(&self.module.context);
    if lty != rty {
      panic!("Binary operations should have the same types: {} != {}",
             lty.to_string(&self.module.context), rty.to_string(&self.module.context));
    }
    if let Some(folded) = fold_binary_op(&op, self.context(), &lhs, &rhs) {
      return folded;
    }
    let inst = instruction::Instruction::new(
      lty,
      instruction::InstOpcode::BinaryOp(op),
      name,
      vec![lhs, rhs],
    );
    self.add_instruction(inst)
  }

  pub fn create_xor(&mut self, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
    return self.create_binary_op(BinaryOp::Xor, lhs, rhs, "xor".to_string())
  }

  pub fn create_and(&mut self, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
    return self.create_binary_op(BinaryOp::And, lhs, rhs, "and".to_string())
  }

  pub fn create_or(&mut self, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
    return self.create_binary_op(BinaryOp::Or, lhs, rhs, "or".to_string())
  }

  pub fn create_shl(&mut self, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
    return self.create_binary_op(BinaryOp::Shl, lhs, rhs, "shl".to_string())
  }

  pub fn create_ashr(&mut self, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
    return self.create_binary_op(BinaryOp::AShr, lhs, rhs, "ashr".to_string())
  }

  pub fn create_add(&mut self, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
    return self.create_binary_op(BinaryOp::Add, lhs, rhs, "add".to_string())
  }

  pub fn create_sub(&mut self, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
    return self.create_binary_op(BinaryOp::Sub, lhs, rhs, "sub".to_string())
  }

  pub fn create_mul(&mut self, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
    return self.create_binary_op(BinaryOp::Mul, lhs, rhs, "mul".to_string())
  }

  pub fn create_sdiv(&mut self, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
    return self.create_binary_op(BinaryOp::SDiv, lhs, rhs, "sdiv".to_string())
  }

  pub fn create_srem(&mut self, lhs: ValueRef, rhs: ValueRef) -> ValueRef {
    return self.create_binary_op(BinaryOp::SRem, lhs, rhs, "srem".to_string())
  }

  pub fn create_load(&mut self, ptr: ValueRef) -> ValueRef {
    let ty = ptr.get_type(self.context());
    let pty = ty.as_ref::<PointerType>(&self.module.context).unwrap();
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
        if src_bits == 1 {
          CastOp::ZeroExt
        } else {
          CastOp::SignExt
        }
      } else if src_bits > dst_bits {
        CastOp::Trunc
      } else {
        return val
      }
    } else if (src_ty.kind == TKindCode::IntType && dest.kind == TKindCode::PointerType) ||
              (src_ty.kind == TKindCode::PointerType && dest.kind == TKindCode::IntType) {
      let src_bits = src_ty.get_scalar_size_in_bits(&self.module);
      let dst_bits = dest.get_scalar_size_in_bits(&self.module);
      assert_eq!(src_bits, dst_bits);
      CastOp::Bitcast
    } else {
      panic!("Not supported casting!");
    };
    self.create_op_cast(cast_op, val, dest)
  }

  pub fn create_compare(&mut self, pred: CmpPred, lhs: ValueRef, rhs: ValueRef, name: String)
    -> ValueRef {
    let name = if name.is_empty() { "cmp".to_string() } else { name };
    if let Some(folded) = fold_cmp_op(&pred, self.context(), &lhs, &rhs) {
      return folded;
    }
    let inst = instruction::Instruction::new(
      self.context().int_type(1),
      instruction::InstOpcode::ICompare(pred),
      name,
      vec![lhs, rhs],
    );
    self.add_instruction(inst)
  }

  pub fn create_slt(&mut self, lhs: ValueRef, rhs: ValueRef, name: String) -> ValueRef {
    return self.create_compare(CmpPred::SLT, lhs, rhs, name)
  }

  pub fn create_sgt(&mut self, lhs: ValueRef, rhs: ValueRef, name: String) -> ValueRef {
    return self.create_compare(CmpPred::SGT, lhs, rhs, name)
  }

  pub fn create_sle(&mut self, lhs: ValueRef, rhs: ValueRef, name: String) -> ValueRef {
    return self.create_compare(CmpPred::SLE, lhs, rhs, name)
  }

  pub fn create_sge(&mut self, lhs: ValueRef, rhs: ValueRef, name: String) -> ValueRef {
    return self.create_compare(CmpPred::SGE, lhs, rhs, name)
  }

  pub fn create_eq(&mut self, lhs: ValueRef, rhs: ValueRef, name: String) -> ValueRef {
    return self.create_compare(CmpPred::EQ, lhs, rhs, name)
  }

  pub fn create_ne(&mut self, lhs: ValueRef, rhs: ValueRef, name: String) -> ValueRef {
    return self.create_compare(CmpPred::NE, lhs, rhs, name)
  }

  pub fn create_unconditional_branch(&mut self, bb: ValueRef) -> ValueRef {
    assert!(bb.get_type(self.context()).kind == TKindCode::BlockType);
    let inst = instruction::Instruction::new(
      self.context().void_type(),
      instruction::InstOpcode::Branch(None),
      "br".to_string(),
      vec![bb.clone()],
    );
    self.add_instruction(inst)
  }

  pub fn create_conditional_branch(
    &mut self,
    cond: ValueRef,
    true_bb: ValueRef,
    false_bb: ValueRef,
    loop_latch: bool) -> ValueRef {

    if true_bb == false_bb {
      return self.create_unconditional_branch(true_bb);
    }
    let metadata = if loop_latch {
      let res = Some(self.module.llvm_loop);
      self.module.llvm_loop += 1;
      res
    } else {
      None
    };
    let inst = instruction::Instruction::new(
      self.context().void_type(),
      instruction::InstOpcode::Branch(metadata),
      "br".to_string(),
      vec![cond, true_bb.clone(), false_bb.clone()],
    );
    self.add_instruction(inst)
  }

  pub fn create_phi(&mut self, ty: TypeRef, values: Vec<ValueRef>, blocks: Vec<ValueRef>)
    -> ValueRef {
    let operands = values.into_iter().zip(blocks.into_iter()).flat_map(|(v, b)| vec![v, b]);
    let inst = instruction::Instruction::new(
      ty,
      instruction::InstOpcode::Phi,
      "phi".to_string(),
      operands.collect::<Vec<_>>(),
    );
    self.add_instruction(inst)
  }

}

