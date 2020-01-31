use wasmer_runtime_core::{
    compile_with_config, error::RuntimeError, imports, memory::Memory, typed_func::Func, typed_func::{Host, S3},
    types::{MemoryDescriptor, Value}, units::Pages, vm, Instance,
    backend::{CompilerConfig, Features}
};
use std::sync::Arc;
use wasmer_runtime_core_tests::{get_compiler, wat2wasm};

macro_rules! call_and_assert {
    ($instance:ident, $function:ident, $expected_value:expr) => {
        let $function: Func<i32, i32> = $instance.func(stringify!($function)).unwrap();

        let result = $function.call(1);

        match (result, $expected_value) {
            (Ok(value), expected_value) => assert_eq!(
                Ok(value),
                expected_value,
                concat!("Expected right when calling `", stringify!($function), "`.")
            ),
            (
                Err(RuntimeError::Error { data }),
                Err(RuntimeError::Error {
                    data: expected_data,
                }),
            ) => {
                if let (Some(data), Some(expected_data)) = (
                    data.downcast_ref::<&str>(),
                    expected_data.downcast_ref::<&str>(),
                ) {
                    assert_eq!(
                        data, expected_data,
                        concat!("Expected right when calling `", stringify!($function), "`.")
                    )
                } else if let (Some(data), Some(expected_data)) = (
                    data.downcast_ref::<String>(),
                    expected_data.downcast_ref::<String>(),
                ) {
                    assert_eq!(
                        data, expected_data,
                        concat!("Expected right when calling `", stringify!($function), "`.")
                    )
                } else {
                    assert!(false, "Unexpected error, cannot compare it.")
                }
            }
            (result, expected_value) => assert!(
                false,
                format!(
                    "Unexpected assertion for `{}`: left = `{:?}`, right = `{:?}`.",
                    stringify!($function),
                    result,
                    expected_value
                )
            ),
        }
    };
}

/// The shift that is set in the instance memory. The value is part of
/// the result returned by the imported functions if the memory is
/// read properly.
const SHIFT: i32 = 10;

/// The shift that is captured in the environment of a closure. The
/// value is part of the result returned by the imported function if
/// the closure captures its environment properly.
#[allow(non_upper_case_globals)]
const shift: i32 = 100;

fn imported_functions_forms(test: &dyn Fn(&Instance)) {
    const MODULE: &str = r#"
(module
  (type $type (func (param i32) (result i32)))
  (type $type2 (func (param i32) (result i64 i64 i64)))
  (import "env" "memory" (memory 1 1))
  (import "env" "callback_fn" (func $callback_fn (type $type)))
  (import "env" "callback_closure" (func $callback_closure (type $type)))
  (import "env" "callback_closure_with_env" (func $callback_closure_with_env (type $type)))
  (import "env" "callback_fn_with_vmctx" (func $callback_fn_with_vmctx (type $type)))
  (import "env" "callback_closure_with_vmctx" (func $callback_closure_with_vmctx (type $type)))
  (import "env" "callback_closure_with_vmctx_and_env" (func $callback_closure_with_vmctx_and_env (type $type)))
  (import "env" "callback_fn_trap" (func $callback_fn_trap (type $type)))
  (import "env" "callback_closure_trap" (func $callback_closure_trap (type $type)))
  (import "env" "callback_fn_trap_with_vmctx" (func $callback_fn_trap_with_vmctx (type $type)))
  (import "env" "callback_closure_trap_with_vmctx" (func $callback_closure_trap_with_vmctx (type $type)))
  (import "env" "callback_closure_trap_with_vmctx_and_env" (func $callback_closure_trap_with_vmctx_and_env (type $type)))

  (import "env" "callback_polymorphic_closure" (func $callback_polymorphic_closure (type $type2)))

  
  (func (export "function_fn") (type $type)
    get_local 0
    call $callback_fn)

  (func (export "function_closure") (type $type)
    get_local 0
    call $callback_closure)

  (func (export "function_closure_with_env") (type $type)
    get_local 0
    call $callback_closure_with_env)

  (func (export "function_fn_with_vmctx") (type $type)
    get_local 0
    call $callback_fn_with_vmctx)

  (func (export "function_closure_with_vmctx") (type $type)
    get_local 0
    call $callback_closure_with_vmctx)

  (func (export "function_closure_with_vmctx_and_env") (type $type)
    get_local 0
    call $callback_closure_with_vmctx_and_env)

  (func (export "function_fn_trap") (type $type)
    get_local 0
    call $callback_fn_trap)

  (func (export "function_closure_trap") (type $type)
    get_local 0
    call $callback_closure_trap)

  (func (export "function_fn_trap_with_vmctx") (type $type)
    get_local 0
    call $callback_fn_trap_with_vmctx)

  (func (export "function_closure_trap_with_vmctx") (type $type)
    get_local 0
    call $callback_closure_trap_with_vmctx)

  (func (export "function_closure_trap_with_vmctx_and_env") (type $type)
    get_local 0
    call $callback_closure_trap_with_vmctx_and_env)

  (func (export "function_polymorphic_closure") (type $type2)
    get_local 0
    call $callback_polymorphic_closure)
)
"#;

    let mut features = wabt::Features::new();
    features.enable_all();
    let wasm_binary = wabt::wat2wasm_with_features(MODULE.as_bytes(), features).expect("WAST not valid or malformed");
    let module = compile_with_config(&wasm_binary, &get_compiler(), CompilerConfig { features: Features { simd: true, threads: true, multi_value: true }, ..Default::default() }).unwrap();
    let memory_descriptor = MemoryDescriptor::new(Pages(1), Some(Pages(1)), false).unwrap();
    let memory = Memory::new(memory_descriptor).unwrap();

    memory.view()[0].set(SHIFT);

    use wasmer_runtime_core::types::{FuncSig, Type};
    
    let sig = Arc::new(FuncSig::new(vec![Type::I32], vec![Type::I64, Type::I64, Type::I64]));
    let polymorphic: Func<(), (), Host> = Func::new_polymorphic(sig, |_ctx, params| {
        println!("Polymorphic closure");
        println!("Params: {:?}", params);
        vec![Value::I64(7), Value::I64(8), Value::I64(9)]
    });
    // fn poly_inner(_vmctx: &mut vm::Ctx, _args: &[Value]) -> Vec<Value> {
    //     println!("Polymorphic closure");
    //     vec![Value::I32(2)]
    // }
    // let polymorphic: Func<(), (), Host> = Func::new_polymorphic(sig, poly_inner);

    let import_object = imports! {
        "env" => {
            "memory" => memory.clone(),

            // Regular function.
            "callback_fn" => Func::new(callback_fn),

            // Closure without a captured environment.
            "callback_closure" => Func::new(|n: i32| -> Result<i32, ()> {
                Ok(n + 1)
            }),

            // Closure without a captured environment.
            "callback_polymorphic_closure" => polymorphic,

            // Closure with a captured environment (a single variable + an instance of `Memory`).
            "callback_closure_with_env" => Func::new(move |n: i32| -> Result<i32, ()> {
                let shift_ = shift + memory.view::<i32>()[0].get();

                Ok(shift_ + n + 1)
            }),

            // Regular function with an explicit `vmctx`.
            "callback_fn_with_vmctx" => Func::new(callback_fn_with_vmctx),

            // Closure without a captured environment but with an explicit `vmctx`.
            "callback_closure_with_vmctx" => Func::new(|vmctx: &mut vm::Ctx, n: i32| -> Result<i32, ()> {
                let memory = vmctx.memory(0);
                let shift_: i32 = memory.view()[0].get();

                Ok(shift_ + n + 1)
            }),

            // Closure with a captured environment (a single variable) and with an explicit `vmctx`.
            "callback_closure_with_vmctx_and_env" => Func::new(move |vmctx: &mut vm::Ctx, n: i32| -> Result<i32, ()> {
                let memory = vmctx.memory(0);
                let shift_ = shift + memory.view::<i32>()[0].get();

                Ok(shift_ + n + 1)
            }),

            // Trap a regular function.
            "callback_fn_trap" => Func::new(callback_fn_trap),

            // Trap a closure without a captured environment.
            "callback_closure_trap" => Func::new(|n: i32| -> Result<i32, String> {
                Err(format!("bar {}", n + 1))
            }),

            // Trap a regular function with an explicit `vmctx`.
            "callback_fn_trap_with_vmctx" => Func::new(callback_fn_trap_with_vmctx),

            // Trap a closure without a captured environment but with an explicit `vmctx`.
            "callback_closure_trap_with_vmctx" => Func::new(|vmctx: &mut vm::Ctx, n: i32| -> Result<i32, String> {
                let memory = vmctx.memory(0);
                let shift_: i32 = memory.view()[0].get();

                Err(format!("qux {}", shift_ + n + 1))
            }),

            // Trap a closure with a captured environment (a single variable) and with an explicit `vmctx`.
            "callback_closure_trap_with_vmctx_and_env" => Func::new(move |vmctx: &mut vm::Ctx, n: i32| -> Result<i32, String> {
                let memory = vmctx.memory(0);
                let shift_ = shift + memory.view::<i32>()[0].get();

                Err(format!("! {}", shift_ + n + 1))
            }),
        },
    };
    let instance = module.instantiate(&import_object).unwrap();

    test(&instance);
}

fn callback_fn(n: i32) -> Result<i32, ()> {
    Ok(n + 1)
}

fn callback_fn_with_vmctx(vmctx: &mut vm::Ctx, n: i32) -> Result<i32, ()> {
    let memory = vmctx.memory(0);
    let shift_: i32 = memory.view()[0].get();

    Ok(shift_ + n + 1)
}

fn callback_fn_trap(n: i32) -> Result<i32, String> {
    Err(format!("foo {}", n + 1))
}

fn callback_fn_trap_with_vmctx(vmctx: &mut vm::Ctx, n: i32) -> Result<i32, String> {
    let memory = vmctx.memory(0);
    let shift_: i32 = memory.view()[0].get();

    Err(format!("baz {}", shift_ + n + 1))
}

macro_rules! test {
    ($test_name:ident, $function:ident, $expected_value:expr) => {
        #[test]
        fn $test_name() {
            imported_functions_forms(&|instance| {
                call_and_assert!(instance, $function, $expected_value);
            });
        }
    };
}

test!(test_fn, function_fn, Ok(2));
test!(test_closure, function_closure, Ok(2));
test!(
    test_closure_with_env,
    function_closure_with_env,
    Ok(2 + shift + SHIFT)
);
test!(test_fn_with_vmctx, function_fn_with_vmctx, Ok(2 + SHIFT));
test!(
    test_closure_with_vmctx,
    function_closure_with_vmctx,
    Ok(2 + SHIFT)
);
test!(
    test_closure_with_vmctx_and_env,
    function_closure_with_vmctx_and_env,
    Ok(2 + shift + SHIFT)
);
test!(
    test_fn_trap,
    function_fn_trap,
    Err(RuntimeError::Error {
        data: Box::new(format!("foo {}", 2))
    })
);
test!(
    test_closure_trap,
    function_closure_trap,
    Err(RuntimeError::Error {
        data: Box::new(format!("bar {}", 2))
    })
);
test!(
    test_fn_trap_with_vmctx,
    function_fn_trap_with_vmctx,
    Err(RuntimeError::Error {
        data: Box::new(format!("baz {}", 2 + SHIFT))
    })
);
test!(
    test_closure_trap_with_vmctx,
    function_closure_trap_with_vmctx,
    Err(RuntimeError::Error {
        data: Box::new(format!("qux {}", 2 + SHIFT))
    })
);
test!(
    test_closure_trap_with_vmctx_and_env,
    function_closure_trap_with_vmctx_and_env,
    Err(RuntimeError::Error {
        data: Box::new(format!("! {}", 2 + shift + SHIFT))
    })
);

#[test]
fn test_polymorphic_closure() {
    imported_functions_forms(&|instance| {
        //let function = function_polymorphic_closure;
        //let expected_value = Ok((7u64, 8u64, 9u64));
//        call_and_assert!(instance, function, expected_value);
        let fnz: Func<i32, (i64, i64, i64)> = instance.func("function_polymorphic_closure").unwrap();

        let result = fnz.call(1);

        dbg!(result);
    });
}
