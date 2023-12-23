use crate::ir::ddg::{EdgeImpl, Edge};
use crate::ir::types::{VoidType, TKindCode};
use crate::ir::value::consts::{InlineAsm, Undef};
use crate::ir::value::instruction::const_folder::{fold_binary_op, fold_cmp_op};
use crate::ir::value::instruction::{
  CastOp, InstOpcode, CmpPred, InstMutator, BranchMetadata, PhiNode
};
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

use crate::context::{Context, WithSuperType};

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
    self.set_current_block(block.clone());
    self.create_unconditional_branch(res.clone(), BranchMetadata::None);

    // Maintain those A-related phi predecessors.
    {
      let mut replace_phi_blocks = Vec::new();
      let new_bb = res.as_ref::<Block>(&self.module.context).unwrap();
      for succ in new_bb.succ_iter() {
        for inst in succ.inst_iter() {
          if let Some(phi) = inst.as_sub::<PhiNode>() {
            for (idx, (in_block, _)) in phi.iter().enumerate() {
              if in_block.get_skey() == block.skey {
                // eprintln!("[SPLIT] {}\n  {} -> {}",
                //           inst.to_string(false), in_block.get_name(),
                //           res.to_string(&self.module.context, true));
                replace_phi_blocks.push((inst.as_super(), idx * 2 + 1));
              }
            }
          }
        }
      }
      for (phi, idx) in replace_phi_blocks {
        let mut mutator = InstMutator::new(self.context(), &phi);
        mutator.set_operand(idx, res.clone());
      }
    }

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
    let inst = inst_ref.as_ref::<Instruction>(&self.module.context).unwrap();
    self.block = Some(inst.get_parent().as_super());
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
      let edges = inst.instance.operands.clone();
      let inst_ref = self.context().add_instance(inst);
      let inst_value = Instruction::from_skey(inst_ref.skey);
      edges.iter().for_each(|e| {
        self.context().get_value_mut::<Edge>(*e).instance.user = inst_value.clone();
      });
      // Maintain the instruction redundancy.
      self.module.context.add_user_redundancy(edges);
      let block = block_ref.as_mut::<Block>(&mut self.module.context).unwrap();
      block.instance.insts.insert(insert_idx, inst_value.skey);
      inst_value.clone()
    }
  }

  pub fn create_instruction(
    &mut self, ty: TypeRef, op: InstOpcode, operands: Vec<ValueRef>, name: String) -> ValueRef {
    let edge = self.operand_to_edge(&operands);
    let inst = instruction::Instruction::new(ty, op, name, edge);
    self.add_instruction(inst)
  }

  pub fn create_return(&mut self, val: Option<ValueRef>) -> ValueRef {
    let ret_ty = self.context().void_type();
    self.create_instruction(
      ret_ty,
      instruction::InstOpcode::Return,
      if let None = val { vec![] } else {vec![val.unwrap()]},
      "ret".to_string(),
    )
  }

  pub fn create_alloca(&mut self, ty: types::TypeRef, name: String) -> ValueRef {
    let ptr_ty = self.context().pointer_type();
    let inst = instruction::Instruction::new(
      ptr_ty,
      // TODO(@were): Make this alignment better
      instruction::InstOpcode::Alloca((ty, 8)),
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

  /// This interface is not recommended to use, because its excessive parameters are error-prone.
  /// Please use `get_struct_field`, and `index_array` above instead.
  pub fn create_gep(
    &mut self,
    ty: TypeRef,
    ptr: ValueRef,
    indices: Vec<ValueRef>,
    inbounds: bool,
    name: String) -> ValueRef {
    let name = if name.is_empty() {
      "gep".to_string()
    } else {
      name
    };
    let ptr_ty = self.context().pointer_type();
    let mut operands = vec![ptr];
    operands.extend(indices);
    // All constants
    if operands[0].is_const() && operands.iter().fold(true, |acc, val| acc && val.is_const()) {
      let opcode = instruction::InstOpcode::GetElementPtr((ty, inbounds));
      let res = ConstExpr::new(ptr_ty, opcode, operands);
      let expr = self.context().add_instance::<ConstExpr, _>(res);
      return expr
    } else {
      let inst = self.create_instruction(
        ptr_ty,
        instruction::InstOpcode::GetElementPtr((ty, inbounds)),
        operands,
        name,
      );
      return inst
    }
  }

  // TODO(@were): Add alignment
  pub fn create_store(&mut self, value: ValueRef, ptr: ValueRef) -> Result<ValueRef, String> {
    let ptr_ty = ptr.get_type(&self.module.context);
    if let Some(_) = ptr_ty.as_ref::<PointerType>(&self.module.context) {
      // For now, we disable store's semantic enforcement.
      let vty = self.context().void_type();
      let inst = self.create_instruction(
        vty,
        instruction::InstOpcode::Store(8),
        vec![value, ptr],
        "store".to_string(),
      );
      Ok(inst)
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
    self.create_instruction(
      ty,
      instruction::InstOpcode::Call,
      args,
      "call".to_string(),
    )
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
    self.create_instruction(
      lty,
      instruction::InstOpcode::BinaryOp(op),
      vec![lhs, rhs],
      name,
    )
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

  pub fn create_load(&mut self, res_ty: TypeRef, ptr: ValueRef) -> ValueRef {
    self.create_typed_load(res_ty, ptr)
  }

  // TODO(@were): Add alignment
  pub fn create_typed_load(&mut self, ty: TypeRef, ptr: ValueRef) -> ValueRef {
    self.create_instruction(
      ty,
      instruction::InstOpcode::Load(8),
      vec![ptr],
      "load".to_string(),
    )
  }

  pub fn create_global_struct(&mut self, ty: TypeRef, init: Vec<ValueRef>) -> ValueRef {
    let gvs = ConstObject::new(
      "globalobj".to_string(),
      ty,
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
    self.create_instruction(dest, op, vec![val], "cast".to_string(),)
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
    let i1ty = self.context().int_type(1);
    let cmp = instruction::InstOpcode::ICompare(pred);
    self.create_instruction(i1ty, cmp, vec![lhs, rhs], name)
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

  pub fn create_unconditional_branch(&mut self, bb: ValueRef, metadata: BranchMetadata)
    -> ValueRef {
    assert!(bb.get_type(self.context()).kind == TKindCode::BlockType);
    let vty = self.context().void_type();
    self.create_instruction(
      vty,
      instruction::InstOpcode::Branch(metadata.clone()),
      vec![bb.clone()],
      "br".to_string(),
    )
  }

  pub fn create_conditional_branch(
    &mut self,
    cond: ValueRef,
    true_bb: ValueRef,
    false_bb: ValueRef,
    loop_latch: bool) -> ValueRef {

    if true_bb == false_bb {
      assert!(!loop_latch);
      return self.create_unconditional_branch(true_bb, BranchMetadata::None);
    }
    let metadata = if loop_latch {
      let res = instruction::BranchMetadata::LLVMLoop;
      res
    } else {
      instruction::BranchMetadata::None
    };
    let vty = self.context().void_type();
    self.create_instruction(
      vty,
      instruction::InstOpcode::Branch(metadata),
      vec![cond, true_bb.clone(), false_bb.clone()],
      "br".to_string(),
    )
  }

  pub fn create_phi(&mut self, ty: TypeRef, values: Vec<ValueRef>, blocks: Vec<ValueRef>)
    -> ValueRef {
    let operands = values.into_iter().zip(blocks.into_iter()).flat_map(|(v, b)| vec![v, b]);
    self.create_instruction(
      ty,
      instruction::InstOpcode::Phi,
      operands.collect::<Vec<_>>(),
      "phi".to_string(),
    )
  }

  fn operand_to_edge(&mut self, operands: &Vec<ValueRef>) -> Vec<usize> {
    let edge_impl = operands.iter().map(|x| {
      let edge_impl = EdgeImpl::new(x.clone(), Undef::from_skey(0));
      let edge_instance = Edge::from(edge_impl);
      edge_instance
    });
    edge_impl.into_iter().map(|x| self.context().add_edge(x)).collect()
  }

}

