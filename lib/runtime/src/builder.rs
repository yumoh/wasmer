pub use wasmer_runtime_core::config::{Allowed, Metering};
use wasmer_runtime_core::{
    backend::{Backend, Compiler as _},
    config::CompileConfig,
    error::CompileResult,
    module::{Module, ResourceIndex},
};

// use crate::backends::Backend;
use std::marker::PhantomData;

pub struct CompilerBuilder {
    metering: Metering,
    allowed: Allowed,
    backend: Backend,
}

impl CompilerBuilder {
    pub fn metering(mut self, metering: Metering) -> Self {
        self.metering = metering;
        self
    }

    pub fn allowed(mut self, allowed: Allowed) -> Self {
        self.allowed = allowed;
        self
    }

    pub fn backend(mut self, backend: Backend) -> Self {
        self.backend = backend;
        self
    }

    pub fn build(self) -> Compiler {
        Compiler {
            metering: self.metering,
            allowed: self.allowed,
            backend: self.backend,
        }
    }
}

pub struct ModuleBuilder<'a> {
    wasm: &'a [u8],
    metering: &'a Metering,
    allowed: &'a Allowed,
    backend: Backend,
    symbol_mapper: Option<Box<dyn Fn(ResourceIndex) -> Option<String>>>,
}

impl<'a> ModuleBuilder<'a> {
    pub fn map_symbols(
        mut self,
        mapper: impl Fn(ResourceIndex) -> Option<String> + 'static,
    ) -> Self {
        self.symbol_mapper = Some(Box::new(mapper));
        self
    }

    pub fn compile(self) -> CompileResult<Module> {
        match self.backend {
            Backend::Singlepass => {
                #[cfg(feature = "backend:singlepass")]
                {
                    use wasmer_singlepass_backend::SinglePassCompiler;
                    SinglePassCompiler::new()
                        .compile(
                            self.wasm,
                            CompileConfig {
                                metering: self.metering,
                                allowed: self.allowed,
                                symbol_map: self.symbol_mapper,
                            },
                        )
                        .map(|module| Module::new(module.into()))
                }
                #[cfg(not(feature = "backend:singlepass"))]
                unimplemented!("the singlepass backend is not enabled, try rebuilding with the \"backend:singlepass\" feature enabled")
            }
            Backend::Cranelift => {
                #[cfg(feature = "backend:cranelift")]
                {
                    use wasmer_clif_backend::CraneliftCompiler;
                    CraneliftCompiler::new()
                        .compile(
                            self.wasm,
                            CompileConfig {
                                metering: self.metering,
                                allowed: self.allowed,
                                symbol_map: self.symbol_mapper,
                            },
                        )
                        .map(|module| Module::new(module.into()))
                }
                #[cfg(not(feature = "backend:cranelift"))]
                unimplemented!("the cranelift backend is not enabled, try rebuilding with the \"backend:cranelift\" feature enabled")
            }
            Backend::LLVM => {
                #[cfg(feature = "backend:llvm")]
                {
                    use wasmer_llvm_backend::LLVMCompiler;
                    LLVMCompiler::new()
                        .compile(
                            self.wasm,
                            CompileConfig {
                                metering: self.metering,
                                allowed: self.allowed,
                                symbol_map: self.symbol_mapper,
                            },
                        )
                        .map(|module| Module::new(module.into()))
                }
                #[cfg(not(feature = "backend:llvm"))]
                unimplemented!("the llvm backend is not enabled, try rebuilding with the \"backend:llvm\" feature enabled")
            }
        }
    }
}

///
/// # Usage:
/// ```
/// # use wasmer_runtime::{Compiler, backends::SinglePass, Metering};
///
/// let compiler: Compiler<SinglePass> = Compiler::new()
///     .metering(Metering {
///         .. Metering::default(),
///     })
///     .build();
///
/// let module = compiler.module(&[])
///     .map_symbols(|resource_index| resource_map.get(resource_index))
///     .compile().unwrap();
///
///
/// ```
pub struct Compiler {
    metering: Metering,
    allowed: Allowed,
    backend: Backend,
}

impl Compiler {
    pub fn new() -> CompilerBuilder {
        CompilerBuilder {
            metering: Default::default(),
            allowed: Default::default(),
            backend: Backend::Cranelift,
        }
    }

    pub fn module<'a>(&'a self, wasm: &'a [u8]) -> ModuleBuilder<'a> {
        ModuleBuilder {
            wasm,
            metering: &self.metering,
            allowed: &self.allowed,
            backend: self.backend,
            symbol_mapper: None,
        }
    }
}
