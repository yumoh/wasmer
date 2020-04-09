//! Support for compiling with Cranelift.

use crate::config::CraneliftConfig;
use crate::func_environ::{get_func_name, FuncEnvironment};
use crate::trampoline::{make_wasm_trampoline, FunctionBuilderContext};
use crate::translator::{
    irlibcall_to_libcall, irreloc_to_relocationkind, signature_to_cranelift_ir, FuncTranslator,
};
use crate::unwind::compiled_function_unwind_info;
use cranelift_codegen::ir::{self, ExternalName};
use cranelift_codegen::print_errors::pretty_error;
use cranelift_codegen::{binemit, isa, Context};
use rayon::prelude::{IntoParallelRefIterator, ParallelIterator};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use wasm_common::entity::{EntityRef, PrimaryMap, SecondaryMap};
use wasm_common::Features;
use wasm_common::{DefinedFuncIndex, FuncIndex, FuncType, SignatureIndex};
use wasmer_compiler::FunctionBodyData;
use wasmer_compiler::{Compilation, CompiledFunction, Compiler, JumpTable};
use wasmer_compiler::{CompilerConfig, ModuleTranslationState, Target};
use wasmer_compiler::{Relocation, RelocationTarget};
use wasmer_compiler::CompileError;
use wasmer_compiler::SourceLoc;
use wasmer_compiler::{Module, ModuleLocal};
use wasmer_compiler::{TrapCode, TrapInformation};

/// Implementation of a relocation sink that just saves all the information for later
pub struct RelocSink {
    /// Current function index.
    func_index: FuncIndex,

    /// Relocations recorded for the function.
    pub func_relocs: Vec<Relocation>,
}

impl binemit::RelocSink for RelocSink {
    fn reloc_block(
        &mut self,
        _offset: binemit::CodeOffset,
        _reloc: binemit::Reloc,
        _block_offset: binemit::CodeOffset,
    ) {
        // This should use the `offsets` field of `ir::Function`.
        panic!("block headers not yet implemented");
    }
    fn reloc_external(
        &mut self,
        offset: binemit::CodeOffset,
        reloc: binemit::Reloc,
        name: &ExternalName,
        addend: binemit::Addend,
    ) {
        let reloc_target = if let ExternalName::User { namespace, index } = *name {
            debug_assert_eq!(namespace, 0);
            RelocationTarget::UserFunc(FuncIndex::from_u32(index))
        } else if let ExternalName::LibCall(libcall) = *name {
            RelocationTarget::LibCall(irlibcall_to_libcall(libcall))
        } else {
            panic!("unrecognized external name")
        };
        self.func_relocs.push(Relocation {
            kind: irreloc_to_relocationkind(reloc),
            reloc_target,
            offset,
            addend,
        });
    }

    fn reloc_constant(
        &mut self,
        _code_offset: binemit::CodeOffset,
        _reloc: binemit::Reloc,
        _constant_offset: ir::ConstantOffset,
    ) {
        // Do nothing for now: cranelift emits constant data after the function code and also emits
        // function code with correct relative offsets to the constant data.
    }

    fn reloc_jt(&mut self, offset: binemit::CodeOffset, reloc: binemit::Reloc, jt: ir::JumpTable) {
        self.func_relocs.push(Relocation {
            kind: irreloc_to_relocationkind(reloc),
            reloc_target: RelocationTarget::JumpTable(self.func_index, JumpTable::new(jt.index())),
            offset,
            addend: 0,
        });
    }
}

impl RelocSink {
    /// Return a new `RelocSink` instance.
    pub fn new(func_index: FuncIndex) -> Self {
        Self {
            func_index,
            func_relocs: Vec::new(),
        }
    }
}

struct TrapSink {
    pub traps: Vec<TrapInformation>,
}

impl TrapSink {
    fn new() -> Self {
        Self { traps: Vec::new() }
    }
}

impl binemit::TrapSink for TrapSink {
    fn trap(
        &mut self,
        code_offset: binemit::CodeOffset,
        source_loc: ir::SourceLoc,
        trap_code: ir::TrapCode,
    ) {
        self.traps.push(TrapInformation {
            code_offset,
            source_loc: SourceLoc::new(source_loc.bits()),
            // TODO: Translate properly environment Trapcode into cranelift IR
            trap_code: translate_ir_trapcode(trap_code),
        });
    }
}

/// Translates the Cranelift IR TrapCode into generic Trap Code
fn translate_ir_trapcode(trap: ir::TrapCode) -> TrapCode {
    match trap {
        ir::TrapCode::StackOverflow => TrapCode::StackOverflow,
        ir::TrapCode::HeapOutOfBounds => TrapCode::HeapOutOfBounds,
        ir::TrapCode::TableOutOfBounds => TrapCode::TableOutOfBounds,
        ir::TrapCode::OutOfBounds => TrapCode::OutOfBounds,
        ir::TrapCode::IndirectCallToNull => TrapCode::IndirectCallToNull,
        ir::TrapCode::BadSignature => TrapCode::BadSignature,
        ir::TrapCode::IntegerOverflow => TrapCode::IntegerOverflow,
        ir::TrapCode::IntegerDivisionByZero => TrapCode::IntegerDivisionByZero,
        ir::TrapCode::BadConversionToInteger => TrapCode::BadConversionToInteger,
        ir::TrapCode::UnreachableCodeReached => TrapCode::UnreachableCodeReached,
        ir::TrapCode::Interrupt => TrapCode::Interrupt,
        ir::TrapCode::User(user_code) => TrapCode::User(user_code),
    }
}

/// A compiler that compiles a WebAssembly module with Cranelift, translating the Wasm to Cranelift IR,
/// optimizing it and then translating to assembly.
pub struct CraneliftCompiler {
    isa: Box<dyn isa::TargetIsa>,
    config: CraneliftConfig,
}

impl CraneliftCompiler {
    /// Creates a new Cranelift compiler
    pub fn new(config: &CraneliftConfig) -> CraneliftCompiler {
        let isa = config.isa();
        CraneliftCompiler {
            isa,
            config: config.clone(),
        }
    }

    /// Retrieves the target ISA
    fn isa(&self) -> &dyn isa::TargetIsa {
        &*self.isa
    }

    /// Gets the WebAssembly features for this Compiler
    fn config(&self) -> &CraneliftConfig {
        &self.config
    }
}

impl Compiler for CraneliftCompiler {
    /// Gets the WebAssembly features for this Compiler
    fn features(&self) -> Features {
        self.config.features().clone()
    }

    /// Gets the target associated to the Cranelift ISA.
    fn target(&self) -> Target {
        self.config.target().clone()
    }

    /// Compile the module using Cranelift, producing a compilation result with
    /// associated relocations.
    fn compile_module(
        &self,
        module: &Module,
        module_translation: &ModuleTranslationState,
        function_body_inputs: PrimaryMap<DefinedFuncIndex, FunctionBodyData<'_>>,
        // generate_debug_info: bool,
    ) -> Result<Compilation, CompileError> {
        compile((
            &module.local,
            HashedModuleTranslationState(module_translation),
            function_body_inputs,
            Isa(&*self.isa),
            // generate_debug_info,
        ))
    }

    fn compile_wasm_trampolines(
        &self,
        signatures: &[FuncType],
    ) -> Result<Vec<CompiledFunction>, CompileError> {
        signatures
            .par_iter()
            .map_init(FunctionBuilderContext::new, |mut cx, sig| {
                make_wasm_trampoline(&*self.isa, &mut cx, sig, std::mem::size_of::<u128>())
            })
            .collect::<Result<Vec<_>, CompileError>>()
    }
}

/// Transforms Cranelift JumpTable's into runtime JumpTables
pub fn transform_jump_table(
    jt_offsets: SecondaryMap<ir::JumpTable, u32>,
) -> SecondaryMap<JumpTable, u32> {
    let mut func_jt_offsets = SecondaryMap::with_capacity(jt_offsets.capacity());

    for (key, value) in jt_offsets.iter() {
        let new_key = JumpTable::new(key.index());
        func_jt_offsets[new_key] = *value;
    }
    func_jt_offsets
}

fn compile(
    (module, HashedModuleTranslationState(module_translation), function_body_inputs, Isa(isa)): (
        &ModuleLocal,
        HashedModuleTranslationState<'_>,
        PrimaryMap<DefinedFuncIndex, FunctionBodyData<'_>>,
        Isa<'_, '_>,
    ),
) -> Result<Compilation, CompileError> {
    let frontend_config = isa.frontend_config();
    let signatures = module
        .signatures
        .iter()
        .map(|(_sig_index, func_type)| signature_to_cranelift_ir(func_type, &frontend_config))
        .collect::<PrimaryMap<SignatureIndex, ir::Signature>>();

    let functions = function_body_inputs
        .into_iter()
        .collect::<Vec<(DefinedFuncIndex, &FunctionBodyData<'_>)>>()
        .par_iter()
        .map_init(FuncTranslator::new, |func_translator, (i, input)| {
            let func_index = module.func_index(*i);
            let mut context = Context::new();
            let mut func_env = FuncEnvironment::new(isa.frontend_config(), module, &signatures);
            context.func.name = get_func_name(func_index);
            context.func.signature = signatures[module.functions[func_index]].clone();
            context.func.collect_frame_layout_info();
            // if generate_debug_info {
            //     context.func.collect_debug_info();
            // }

            func_translator.translate(
                module_translation,
                input.data,
                input.module_offset,
                &mut context.func,
                &mut func_env,
            )?;

            let mut code_buf: Vec<u8> = Vec::new();
            let mut reloc_sink = RelocSink::new(func_index);
            let mut trap_sink = TrapSink::new();
            let mut stackmap_sink = binemit::NullStackmapSink {};
            context
                .compile_and_emit(
                    isa,
                    &mut code_buf,
                    &mut reloc_sink,
                    &mut trap_sink,
                    &mut stackmap_sink,
                )
                .map_err(|error| {
                    CompileError::Codegen(pretty_error(&context.func, Some(isa), error))
                })?;

            let unwind_info = compiled_function_unwind_info(isa, &context);

            // We transform the Cranelift JumpTable's into compiler JumpTables
            let func_jt_offsets = transform_jump_table(context.func.jt_offsets);

            Ok(CompiledFunction {
                body: code_buf,
                jt_offsets: func_jt_offsets,
                unwind_info,
                relocations: reloc_sink.func_relocs,
                traps: trap_sink.traps,
            })
        })
        .collect::<Result<Vec<_>, CompileError>>()?
        .into_iter()
        .collect::<PrimaryMap<DefinedFuncIndex, _>>();

    Ok(Compilation::new(functions))
}

/// This is a wrapper struct to hash the specific bits of `TargetIsa` that
/// affect the output we care about. The trait itself can't implement `Hash`
/// (it's not object safe) so we have to implement our own hashing.
struct Isa<'a, 'b>(&'a (dyn isa::TargetIsa + 'b));

impl Hash for Isa<'_, '_> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.0.triple().hash(hasher);

        // TODO: if this `to_string()` is too expensive then we should upstream
        // a native hashing ability of flags into cranelift itself, but
        // compilation and/or cache loading is relatively expensive so seems
        // unlikely.
        self.0.flags().to_string().hash(hasher);

        // TODO: ... and should we hash anything else? There's a lot of stuff in
        // `TargetIsa`, like registers/encodings/etc. Should we be hashing that
        // too? It seems like wasmer doesn't configure it too too much, but
        // this may become an issue at some point.
    }
}

/// A wrapper struct around cranelift's `ModuleTranslationState` to implement
/// `Hash` since it's not `Hash` upstream yet.
///
/// TODO: we should upstream a `Hash` implementation, it would be very small! At
/// this moment though based on the definition it should be fine to not hash it
/// since we'll re-hash the signatures later.
struct HashedModuleTranslationState<'a>(&'a ModuleTranslationState);

impl Hash for HashedModuleTranslationState<'_> {
    fn hash<H: Hasher>(&self, _hasher: &mut H) {
        // nothing to hash right now
    }
}
