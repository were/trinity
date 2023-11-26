use std::collections::HashSet;

use crate::ir::{module::Module, value::instruction::{InstOpcode, PhiNode}};


pub fn verify(m: &Module) {
  for func in m.func_iter() {
    for bb in func.block_iter() {
      for inst in bb.inst_iter() {
        match inst.get_opcode() {
          &InstOpcode::Phi => {
            let blocks = inst
              .get_parent() // Current block
              .pred_iter() // Predecessor branches
              .map(|x| x.get_parent().get_skey()) // Branches' block
              .collect::<HashSet<_>>();
            let phi = inst.as_sub::<PhiNode>().unwrap();
            for (bb, _) in phi.iter() {
              if !blocks.contains(&bb.get_skey()) {
                panic!("Predecessor-phi mismatch!\n{}\n{}",
                       inst.to_string(false),
                       inst.get_parent().to_string(false));
              }
            }
          }
          _ => {}
        }
      }
    }
  }
}

