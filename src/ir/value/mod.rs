pub mod function;
pub mod instruction;
pub mod block;
pub mod consts;
pub mod value;

pub use value::{ValueRef, VKindCode};
pub use function::{Function, Argument};
pub use block::Block;
pub use instruction::Instruction;
pub use consts::{ConstArray, ConstExpr, ConstScalar};

