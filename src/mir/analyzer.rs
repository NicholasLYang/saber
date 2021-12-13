/*
 * Currently just control flow analysis but perhaps more stuff in the future
 */

use crate::mir::{Block, BlockId, Function, InstructionKind, Type};

pub struct Analyzer {}

impl Analyzer {
    // We go through function block and check if there is a return instruction
    // If we find one, we know that the function returns a value
    // Otherwise, we check the blocks referenced via If/Br instructions and see
    // if they contain a return. For If, both branches need to return.
    // If we discover that there is a branch that does not return, we know that
    // the function has an empty, i.e. (), return type
    pub fn analyze_function(&self, function: &Function) -> Option<Type> {
        self.analyze_block(function, BlockId(function.body.len() - 1))
    }

    fn analyze_block(&self, function: &Function, block_id: BlockId) -> Option<Type> {
        let block = function.body.get(block_id.0).unwrap();

        for instr in block.0.iter() {
            match instr.kind {
                InstructionKind::Return(expr) => {
                    return block.0.get(expr.0).unwrap().ty;
                }
                InstructionKind::Br(block_id) => {
                    if let Some(ty) = self.analyze_block(function, block_id) {
                        return Some(ty);
                    }
                }
                InstructionKind::If(_, then_block, else_block) => {
                    match (
                        self.analyze_block(function, then_block),
                        self.analyze_block(function, else_block),
                    ) {
                        (Some(t1), Some(t2)) => {
                            if t1 != t2 {
                                panic!(
                                    "{:?} != {:?}, should have been caught by type checker",
                                    t1, t2
                                )
                            } else {
                                return Some(t1);
                            }
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        }

        None
    }
}
