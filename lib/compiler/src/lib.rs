//! The `wasmer-compiler` crate provides the necessary abstractions
//! to create a compiler.
//!
//! It provides an universal way of parsing a module via `wasmparser`,
//! while giving the responsibility of compiling specific function
//! WebAssembly bodies to the `Compiler` implementation.

#![deny(missing_docs, trivial_numeric_casts, unused_extern_crates)]
#![warn(unused_import_braces)]
#![cfg_attr(feature = "std", deny(unstable_features))]
#![cfg_attr(feature = "clippy", plugin(clippy(conf_file = "../../clippy.toml")))]
#![cfg_attr(feature = "cargo-clippy", allow(clippy::new_without_default))]
#![cfg_attr(
    feature = "cargo-clippy",
    warn(
        clippy::float_arithmetic,
        clippy::mut_mut,
        clippy::nonminimal_bool,
        clippy::option_map_unwrap_or,
        clippy::option_map_unwrap_or_else,
        clippy::print_stdout,
        clippy::unicode_not_nfc,
        clippy::use_self
    )
)]
#![no_std]

#[cfg(not(feature = "std"))]
#[macro_use]
extern crate alloc as std;
#[cfg(feature = "std")]
#[macro_use]
extern crate std;

#[cfg(not(feature = "std"))]
use hashbrown::HashMap;
#[cfg(feature = "std")]
use std::collections::HashMap;

mod compiler;
mod config;
mod errors;
mod function;
mod jump_table;
mod libcall;
mod relocation;
mod unwind;
#[macro_use]
mod translator;
mod module;
mod sourceloc;
mod traps;
mod tunables;
mod vm;

pub use crate::compiler::Compiler;
pub use crate::config::{CompilerConfig, CpuFeature, Features, Target};
pub use crate::errors::CompileError;
pub use crate::function::{Compilation, CompiledFunction, Functions};
pub use crate::jump_table::{JumpTable, JumpTableOffsets};
pub use crate::libcall::LibCall;
pub use crate::module::{
    MemoryPlan, MemoryStyle, Module, ModuleLocal, TableElements, TablePlan, TableStyle,
};
pub use crate::relocation::{Relocation, RelocationKind, RelocationTarget, Relocations};
pub use crate::sourceloc::SourceLoc;
pub use crate::translator::{
    to_wasm_error, translate_module, FunctionBodyData, ModuleEnvironment, ModuleTranslation,
    ModuleTranslationState, WasmError, WasmResult,
};
pub use crate::traps::{TrapCode, TrapInformation};
pub use crate::tunables::Tunables;
pub use crate::unwind::{CompiledFunctionUnwindInfo, FDERelocEntry, FunctionTableReloc};
pub use crate::vm::{VMBuiltinFunctionIndex, TargetSharedSignatureIndex, VMOffsets};

/// Offset in bytes from the beginning of the function.
pub type CodeOffset = u32;

/// Addend to add to the symbol value.
pub type Addend = i64;

/// Version number of this crate.
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
