use cranelift_codegen::cursor::FuncCursor;
use cranelift_codegen::ir;
use cranelift_codegen::ir::InstBuilder;
use cranelift_codegen::ir::Type;
use wasmer_runtime_core::types::FuncSig;
use wasmer_runtime_core::types::Type as WasmType;

/// ReturnType represents the ABI class of a multi-value returning function. It
/// does not correctly represent the case of a single return value, the caller
/// should check for that case and handle it without involving ReturnType.
#[derive(Eq, PartialEq)]
pub enum ReturnType {
    None,
    FourByte(Type),
    EightByte(Type),
    TwelveByte(Type, Type),

    /// SixteenByte will only contain 12 bytes of data when the first element
    /// is four bytes, followed by four bytes of padding, followed by 8 bytes of
    /// data.
    SixteenByte(Type, Type),

    StructRet,
}

impl Default for ReturnType {
    fn default() -> Self {
        Self::None
    }
}

pub fn is_32(ty: &WasmType) -> bool {
    match ty {
        WasmType::I32 | WasmType::F32 => true,
        _ => false,
    }
}

pub fn is_64(ty: &WasmType) -> bool {
    match ty {
        WasmType::I64 | WasmType::F64 => true,
        _ => false,
    }
}

fn wasm_ty_to_clif_ty(ty: &WasmType) -> Type {
    match ty {
        WasmType::I32 => ir::types::I32,
        WasmType::I64 => ir::types::I64,
        WasmType::F32 => ir::types::F32,
        WasmType::F64 => ir::types::F64,
        WasmType::V128 => ir::types::I32X4,
    }
}

impl ReturnType {
    pub fn fold(self, ty: &WasmType) -> Self {
        match self {
            Self::None if is_32(ty) => Self::FourByte(wasm_ty_to_clif_ty(ty)),
            Self::None if is_64(ty) => Self::EightByte(wasm_ty_to_clif_ty(ty)),
            Self::FourByte(a) if is_32(ty) => match (a, wasm_ty_to_clif_ty(ty)) {
                (ir::types::F32, ir::types::F32) => Self::EightByte(ir::types::F64),
                _ => Self::EightByte(ir::types::I64),
            },
            Self::FourByte(a) if is_64(ty) => Self::SixteenByte(a, wasm_ty_to_clif_ty(ty)),
            Self::EightByte(a) if is_32(ty) => Self::TwelveByte(a, wasm_ty_to_clif_ty(ty)),
            Self::EightByte(a) if is_64(ty) => Self::SixteenByte(a, wasm_ty_to_clif_ty(ty)),
            Self::TwelveByte(a, b) if is_32(ty) => match (b, wasm_ty_to_clif_ty(ty)) {
                (ir::types::F32, ir::types::F32) => Self::SixteenByte(a, ir::types::F64),
                _ => Self::SixteenByte(a, ir::types::I64),
            },
            _ => Self::StructRet,
        }
    }

    pub fn _is_void(&self) -> bool {
        match self {
            Self::None | Self::StructRet => true,
            _ => false,
        }
    }

    pub fn is_sret(&self) -> bool {
        *self == Self::StructRet
    }

    pub fn rets(&self) -> Vec<Type> {
        match self {
            Self::FourByte(a) | Self::EightByte(a) => vec![*a],
            Self::TwelveByte(a, b) | Self::SixteenByte(a, b) => vec![*a, *b],
            _ => vec![],
        }
    }
}

/*
// Given a function definition, retrieve the parameter that is the vmctx pointer.
pub fn get_vmctx_ptr_param<'ctx>(func_value: &FunctionValue<'ctx>) -> PointerValue<'ctx> {
    unimplemented!();
}
*/

/*
// Given a wasm function type, produce an llvm function declaration.
pub fn func_sig_to_clif<'ctx>(
    context: &'ctx Context,
    intrinsics: &Intrinsics<'ctx>,
    sig: &FuncSig,
) -> (FunctionType<'ctx>, Vec<(Attribute, AttributeLoc)>) {
    unimplemented!();
}
*/

/*
// Marshall wasm stack values into function parameters.
pub fn args_to_call<'ctx>(
    alloca_builder: &Builder<'ctx>,
    func_sig: &FuncSig,
    ctx_ptr: PointerValue<'ctx>,
    llvm_fn_ty: &FunctionType<'ctx>,
    values: &[BasicValueEnum<'ctx>],
) -> Vec<BasicValueEnum<'ctx>> {
    unimplemented!();
}
*/

pub fn rets_from_call(
    mut pos: &mut FuncCursor,
    call: ir::Inst,
    func_sig: &FuncSig,
) -> Vec<ir::Value> {
    let return_values = pos.func.dfg.inst_results(call).to_vec();

    let is_32 = |wasm_ty| match wasm_ty {
        WasmType::I32 | WasmType::F32 => true,
        _ => false,
    };

    let is_64 = |wasm_ty| match wasm_ty {
        WasmType::I64 | WasmType::F64 => true,
        _ => false,
    };

    let split_i64 = |pos: &mut FuncCursor, v0| {
        let v1 = pos.ins().ireduce(ir::types::I32, v0);
        let v2 = pos.ins().ushr_imm(v0, 32);
        let v3 = pos.ins().ireduce(ir::types::I32, v2);
        (v1, v3)
    };

    let split_f64 = |pos: &mut FuncCursor, v0| {
        let v1 = pos.ins().bitcast(ir::types::I64, v0);
        let (v2, v3) = split_i64(pos, v1);
        let v4 = pos.ins().bitcast(ir::types::F32, v2);
        let v5 = pos.ins().bitcast(ir::types::F32, v3);
        (v4, v5)
    };

    match func_sig.returns() {
        [] => return_values,
        [_] => return_values,
        [a, b] if is_32(*a) && is_32(*b) => {
            let (mut r0, mut r1) = if (*a, *b) == (WasmType::F32, WasmType::F32) {
                split_f64(&mut pos, return_values[0])
            } else {
                split_i64(&mut pos, return_values[0])
            };
            if *a == WasmType::F32 {
                r0 = pos.ins().bitcast(ir::types::F32, r0);
            }
            if *b == WasmType::F32 {
                r1 = pos.ins().bitcast(ir::types::F32, r1);
            }
            vec![r0, r1]
        }
        [a, b, c] if is_64(*a) && is_32(*b) && is_32(*c) => {
            let mut r0 = return_values[0];
            let (mut r1, mut r2) = if (*b, *c) == (WasmType::F32, WasmType::F32) {
                split_f64(&mut pos, return_values[1])
            } else {
                split_i64(&mut pos, return_values[1])
            };
            if *a == WasmType::F32 {
                r0 = pos.ins().bitcast(ir::types::F32, r0);
            }
            if *b == WasmType::F32 {
                r1 = pos.ins().bitcast(ir::types::F32, r1);
            }
            if *c == WasmType::F32 {
                r2 = pos.ins().bitcast(ir::types::F32, r2);
            }
            vec![r0, r1, r2]
        }
        [a, b, c] if is_32(*a) && is_32(*b) && is_64(*c) => {
            let (mut r0, mut r1) = if (*a, *b) == (WasmType::F32, WasmType::F32) {
                split_f64(&mut pos, return_values[0])
            } else {
                split_i64(&mut pos, return_values[0])
            };
            let r2 = return_values[1];
            if *a == WasmType::F32 {
                r0 = pos.ins().bitcast(ir::types::F32, r0);
            }
            if *b == WasmType::F32 {
                r1 = pos.ins().bitcast(ir::types::F32, r1);
            }
            vec![r0, r1, r2]
        }
        [a, b, c] if is_32(*a) && is_32(*b) && is_32(*c) => {
            let (mut r0, mut r1) = if (*a, *b) == (WasmType::F32, WasmType::F32) {
                split_f64(&mut pos, return_values[0])
            } else {
                split_i64(&mut pos, return_values[0])
            };
            let mut r2 = return_values[1];
            if *a == WasmType::F32 {
                r0 = pos.ins().bitcast(ir::types::F32, r0);
            }
            if *b == WasmType::F32 {
                r1 = pos.ins().bitcast(ir::types::F32, r1);
            }
            if *c == WasmType::F32 {
                r2 = pos.ins().bitcast(ir::types::F32, r2);
            }
            vec![r0, r1, r2]
        }
        [a, b, c, d] if is_32(*a) && is_32(*b) && is_32(*c) && is_32(*d) => {
            let (mut r0, mut r1) = if (*a, *b) == (WasmType::F32, WasmType::F32) {
                split_f64(&mut pos, return_values[0])
            } else {
                split_i64(&mut pos, return_values[0])
            };
            let (mut r2, mut r3) = if (*c, *d) == (WasmType::F32, WasmType::F32) {
                split_f64(&mut pos, return_values[1])
            } else {
                split_i64(&mut pos, return_values[1])
            };
            if *a == WasmType::F32 {
                r0 = pos.ins().bitcast(ir::types::F32, r0);
            }
            if *b == WasmType::F32 {
                r1 = pos.ins().bitcast(ir::types::F32, r1);
            }
            if *c == WasmType::F32 {
                r2 = pos.ins().bitcast(ir::types::F32, r2);
            }
            if *d == WasmType::F32 {
                r3 = pos.ins().bitcast(ir::types::F32, r3);
            }
            vec![r0, r1, r2, r3]
        }
        _ => return_values,
    }
}
