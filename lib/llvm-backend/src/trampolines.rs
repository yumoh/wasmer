use crate::intrinsics::Intrinsics;
use inkwell::{
    attributes::{Attribute, AttributeLoc},
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::{BasicType, BasicTypeEnum, FunctionType},
    values::{BasicValueEnum, CallSiteValue, FloatValue, FunctionValue, IntValue, VectorValue},
    AddressSpace,
};
use wasmer_runtime_core::{
    module::ModuleInfo,
    structures::{SliceMap, TypedIndex},
    types::{FuncSig, SigIndex, Type},
};

pub fn generate_trampolines<'ctx>(
    info: &ModuleInfo,
    signatures: &SliceMap<SigIndex, FunctionType<'ctx>>,
    module: &Module<'ctx>,
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    intrinsics: &Intrinsics<'ctx>,
) -> Result<(), String> {
    for (sig_index, sig) in info.signatures.iter() {
        let func_type = signatures[sig_index];

        let trampoline_sig = intrinsics.void_ty.fn_type(
            &[
                intrinsics.ctx_ptr_ty.as_basic_type_enum(), // vmctx ptr
                func_type
                    .ptr_type(AddressSpace::Generic)
                    .as_basic_type_enum(), // func ptr
                intrinsics.i64_ptr_ty.as_basic_type_enum(), // args ptr
                intrinsics.i64_ptr_ty.as_basic_type_enum(), // returns ptr
            ],
            false,
        );

        let trampoline_func = module.add_function(
            &format!("trmp{}", sig_index.index()),
            trampoline_sig,
            Some(Linkage::External),
        );

        generate_trampoline(trampoline_func, sig, context, builder, intrinsics)?;
    }
    Ok(())
}

// Given a call in sysV AMD64 ABI, extract the return values and return those.
pub fn rets_from_call<'ctx>(
    _context: &'ctx Context,
    builder: &Builder<'ctx>,
    intrinsics: &Intrinsics<'ctx>,
    call_site: CallSiteValue<'ctx>,
    func_sig: &FuncSig,
) -> Vec<BasicValueEnum<'ctx>> {
    let split_i64 = |value: IntValue<'ctx>| -> (IntValue<'ctx>, IntValue<'ctx>) {
        assert!(value.get_type() == intrinsics.i64_ty);
        let low = builder.build_int_truncate(value, intrinsics.i32_ty, "");
        let lshr =
            builder.build_right_shift(value, intrinsics.i64_ty.const_int(32, false), false, "");
        let high = builder.build_int_truncate(lshr, intrinsics.i32_ty, "");
        (low, high)
    };

    let f32x2_ty = intrinsics.f32_ty.vec_type(2).as_basic_type_enum();
    let extract_f32x2 = |value: VectorValue<'ctx>| -> (FloatValue<'ctx>, FloatValue<'ctx>) {
        assert!(value.get_type() == f32x2_ty.into_vector_type());
        let ret0 = builder
            .build_extract_element(value, intrinsics.i32_ty.const_int(0, false), "")
            .into_float_value();
        let ret1 = builder
            .build_extract_element(value, intrinsics.i32_ty.const_int(1, false), "")
            .into_float_value();
        (ret0, ret1)
    };

    let casted = |value: BasicValueEnum<'ctx>, ty: Type| -> BasicValueEnum<'ctx> {
        match ty {
            Type::I32 => {
                assert!(
                    value.get_type() == intrinsics.i32_ty.as_basic_type_enum()
                        || value.get_type() == intrinsics.f32_ty.as_basic_type_enum()
                );
                builder.build_bitcast(value, intrinsics.i32_ty, "")
            }
            Type::F32 => {
                assert!(
                    value.get_type() == intrinsics.i32_ty.as_basic_type_enum()
                        || value.get_type() == intrinsics.f32_ty.as_basic_type_enum()
                );
                builder.build_bitcast(value, intrinsics.f32_ty, "")
            }
            Type::I64 => {
                assert!(
                    value.get_type() == intrinsics.i64_ty.as_basic_type_enum()
                        || value.get_type() == intrinsics.f64_ty.as_basic_type_enum()
                );
                builder.build_bitcast(value, intrinsics.i64_ty, "")
            }
            Type::F64 => {
                assert!(
                    value.get_type() == intrinsics.i64_ty.as_basic_type_enum()
                        || value.get_type() == intrinsics.f64_ty.as_basic_type_enum()
                );
                builder.build_bitcast(value, intrinsics.f64_ty, "")
            }
            Type::V128 => {
                assert!(value.get_type() == intrinsics.i128_ty.as_basic_type_enum());
                value
            }
        }
    };

    if let Some(basic_value) = call_site.try_as_basic_value().left() {
        if func_sig.returns().len() > 1 {
            if basic_value.get_type() == intrinsics.i64_ty.as_basic_type_enum() {
                assert!(func_sig.returns().len() == 2);
                let value = basic_value.into_int_value();
                let (low, high) = split_i64(value);
                let low = casted(low.into(), func_sig.returns()[0]);
                let high = casted(high.into(), func_sig.returns()[1]);
                return vec![low.into(), high.into()];
            }
            if basic_value.get_type() == f32x2_ty {
                assert!(func_sig.returns().len() == 2);
                let (ret0, ret1) = extract_f32x2(basic_value.into_vector_value());
                return vec![ret0.into(), ret1.into()];
            }
            let struct_value = basic_value.into_struct_value();
            let rets = (0..struct_value.get_type().count_fields())
                .map(|i| builder.build_extract_value(struct_value, i, "").unwrap())
                .collect::<Vec<_>>();
            let func_sig_returns_bitwidths = func_sig
                .returns()
                .iter()
                .map(|ty| match ty {
                    Type::I32 | Type::F32 => 32,
                    Type::I64 | Type::F64 => 64,
                    Type::V128 => 128,
                })
                .collect::<Vec<i32>>();

            match func_sig_returns_bitwidths.as_slice() {
                [32, 64] | [64, 32] | [64, 64] => {
                    assert!(func_sig.returns().len() == 2);
                    vec![rets[0].into(), rets[1].into()]
                }
                [32, 32, _]
                    if rets[0].get_type() == intrinsics.f32_ty.vec_type(2).as_basic_type_enum() =>
                {
                    assert!(func_sig.returns().len() == 3);
                    let (rets0, rets1) = extract_f32x2(rets[0].into_vector_value());
                    vec![rets0.into(), rets1.into(), rets[1].into()]
                }
                [32, 32, _] => {
                    assert!(func_sig.returns().len() == 3);
                    let (low, high) = split_i64(rets[0].into_int_value());
                    let low = casted(low.into(), func_sig.returns()[0]);
                    let high = casted(high.into(), func_sig.returns()[1]);
                    vec![low.into(), high.into(), rets[1].into()]
                }
                [64, 32, 32]
                    if rets[1].get_type() == intrinsics.f32_ty.vec_type(2).as_basic_type_enum() =>
                {
                    assert!(func_sig.returns().len() == 3);
                    let (rets1, rets2) = extract_f32x2(rets[1].into_vector_value());
                    vec![rets[0].into(), rets1.into(), rets2.into()]
                }
                [64, 32, 32] => {
                    assert!(func_sig.returns().len() == 3);
                    let (rets1, rets2) = split_i64(rets[1].into_int_value());
                    let rets1 = casted(rets1.into(), func_sig.returns()[1]);
                    let rets2 = casted(rets2.into(), func_sig.returns()[2]);
                    vec![rets[0].into(), rets1.into(), rets2.into()]
                }
                [32, 32, 32, 32] => {
                    assert!(func_sig.returns().len() == 4);
                    let (low0, high0) = if rets[0].get_type()
                        == intrinsics.f32_ty.vec_type(2).as_basic_type_enum()
                    {
                        let (x, y) = extract_f32x2(rets[0].into_vector_value());
                        (x.into(), y.into())
                    } else {
                        let (x, y) = split_i64(rets[0].into_int_value());
                        (x.into(), y.into())
                    };
                    let (low1, high1) = if rets[1].get_type()
                        == intrinsics.f32_ty.vec_type(2).as_basic_type_enum()
                    {
                        let (x, y) = extract_f32x2(rets[1].into_vector_value());
                        (x.into(), y.into())
                    } else {
                        let (x, y) = split_i64(rets[1].into_int_value());
                        (x.into(), y.into())
                    };
                    let low0 = casted(low0, func_sig.returns()[0]);
                    let high0 = casted(high0, func_sig.returns()[1]);
                    let low1 = casted(low1, func_sig.returns()[2]);
                    let high1 = casted(high1, func_sig.returns()[3]);
                    vec![low0.into(), high0.into(), low1.into(), high1.into()]
                }
                _ => unreachable!("expected an sret for this type"),
            }
        } else {
            assert!(func_sig.returns().len() == 1);
            vec![basic_value]
        }
    } else {
        assert!(call_site.count_arguments() > 0); // Either sret or vmctx.
        if call_site
            .get_enum_attribute(
                AttributeLoc::Param(0),
                Attribute::get_named_enum_kind_id("sret"),
            )
            .is_some()
        {
            let sret = call_site
                .try_as_basic_value()
                .right()
                .unwrap()
                .get_operand(0)
                .unwrap()
                .left()
                .unwrap()
                .into_pointer_value();
            let struct_value = builder.build_load(sret, "").into_struct_value();
            let mut rets: Vec<_> = Vec::new();
            for i in 0..struct_value.get_type().count_fields() {
                let value = builder.build_extract_value(struct_value, i, "").unwrap();
                rets.push(value);
            }
            assert!(func_sig.returns().len() == rets.len());
            rets
        } else {
            assert!(func_sig.returns().len() == 0);
            vec![]
        }
    }
}

pub fn type_to_llvm<'ctx>(intrinsics: &Intrinsics<'ctx>, ty: Type) -> BasicTypeEnum<'ctx> {
    match ty {
        Type::I32 => intrinsics.i32_ty.as_basic_type_enum(),
        Type::I64 => intrinsics.i64_ty.as_basic_type_enum(),
        Type::F32 => intrinsics.f32_ty.as_basic_type_enum(),
        Type::F64 => intrinsics.f64_ty.as_basic_type_enum(),
        Type::V128 => intrinsics.i128_ty.as_basic_type_enum(),
    }
}

fn generate_trampoline<'ctx>(
    trampoline_func: FunctionValue,
    func_sig: &FuncSig,
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    intrinsics: &Intrinsics<'ctx>,
) -> Result<(), String> {
    let entry_block = context.append_basic_block(trampoline_func, "entry");
    builder.position_at_end(entry_block);

    let (vmctx_ptr, func_ptr, args_ptr, returns_ptr) = match trampoline_func.get_params().as_slice()
    {
        &[vmctx_ptr, func_ptr, args_ptr, returns_ptr] => (
            vmctx_ptr,
            func_ptr.into_pointer_value(),
            args_ptr.into_pointer_value(),
            returns_ptr.into_pointer_value(),
        ),
        _ => return Err("trampoline function unimplemented".to_string()),
    };

    let cast_ptr_ty = |wasmer_ty| match wasmer_ty {
        Type::I32 => intrinsics.i32_ptr_ty,
        Type::F32 => intrinsics.f32_ptr_ty,
        Type::I64 => intrinsics.i64_ptr_ty,
        Type::F64 => intrinsics.f64_ptr_ty,
        Type::V128 => intrinsics.i128_ptr_ty,
    };

    let mut args_vec = Vec::with_capacity(func_sig.params().len() + 1);

    let func_sig_returns_bitwidths = func_sig
        .returns()
        .iter()
        .map(|ty| match ty {
            Type::I32 | Type::F32 => 32,
            Type::I64 | Type::F64 => 64,
            Type::V128 => 128,
        })
        .collect::<Vec<i32>>();

    let is_sret = match func_sig_returns_bitwidths.as_slice() {
        []
        | [_]
        | [32, 64]
        | [64, 32]
        | [64, 64]
        | [32, 32]
        | [32, 32, 32]
        | [32, 32, 64]
        | [64, 32, 32]
        | [32, 32, 32, 32] => false,
        _ => {
            let basic_types: Vec<_> = func_sig
                .returns()
                .iter()
                .map(|&ty| type_to_llvm(intrinsics, ty))
                .collect();

            let sret_ty = context.struct_type(&basic_types, false);
            args_vec.push(builder.build_alloca(sret_ty, "sret").into());

            true
        }
    };

    args_vec.push(vmctx_ptr);

    let mut i = 0;
    for param_ty in func_sig.params().iter() {
        let index = intrinsics.i32_ty.const_int(i as _, false);
        let item_pointer = unsafe { builder.build_in_bounds_gep(args_ptr, &[index], "arg_ptr") };

        let casted_pointer_type = cast_ptr_ty(*param_ty);

        let typed_item_pointer =
            builder.build_pointer_cast(item_pointer, casted_pointer_type, "typed_arg_pointer");

        let arg = builder.build_load(typed_item_pointer, "arg");
        args_vec.push(arg);
        i = i + 1;
        if *param_ty == Type::V128 {
            i = i + 1;
        }
    }

    let call_site = builder.build_call(func_ptr, &args_vec, "call");
    if is_sret {
        call_site.add_attribute(
            AttributeLoc::Param(0),
            context.create_enum_attribute(Attribute::get_named_enum_kind_id("sret"), 0),
        );
    }

    let rets = rets_from_call(context, builder, intrinsics, call_site, func_sig);
    let mut idx = 0;
    rets.iter().for_each(|v| {
        let ptr = unsafe {
            builder.build_gep(returns_ptr, &[intrinsics.i32_ty.const_int(idx, false)], "")
        };
        let ptr = builder.build_pointer_cast(ptr, v.get_type().ptr_type(AddressSpace::Generic), "");
        builder.build_store(ptr, *v);
        if v.get_type() == intrinsics.i128_ty.as_basic_type_enum() {
            idx = idx + 1;
        }
        idx = idx + 1;
    });

    builder.build_return(None);
    Ok(())
}
