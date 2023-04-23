pub mod module;
pub mod function;
pub mod instruction;
pub mod block;
pub mod value;
pub mod types;
pub mod consts;

pub use value::{ValueRef, VKindCode};
pub use function::{Function, Argument};
pub use block::Block;
pub use instruction::Instruction;
pub use consts::{ConstArray, ConstExpr, ConstScalar};
pub use types::{ArrayType, PointerType, StructType, FunctionType, IntType, VoidType, TypeRef, TKindCode};
