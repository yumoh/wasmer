use std::sync::Arc;
use wasmer_runtime_core::{
    backend::CompilerConfig,
    backend::Features,
    compile_with_config,
    error::RuntimeError,
    imports,
    memory::Memory,
    typed_func::{DynamicFunc, Func},
    types::{FuncSig, MemoryDescriptor, Type, Value},
    units::Pages,
    vm, Instance,
};
use wasmer_runtime_core_tests::{get_compiler, wat2wasm_with_features};

macro_rules! call_and_assert {
    ($instance:ident, $function:ident, $return_type:ty, $expected_value:expr) => {
        let $function: Func<i32, $return_type> = $instance.func(stringify!($function)).unwrap();

        let result = $function.call(1);

        match (result, $expected_value) {
            (Ok(value), expected_value) => assert_eq!(
                Ok(value),
                expected_value,
                concat!("Expected right when calling `", stringify!($function), "`.")
            ),
            (Err(RuntimeError(data)), Err(RuntimeError(expected_data))) => {
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
  (type $multi_value_return_type_3xi32 (func (param i32) (result i32 i32 i32)))
  (type $multi_value_return_type_2xf32 (func (param i32) (result f32 f32)))
  (import "env" "memory" (memory 1 1))
  (import "env" "callback_fn" (func $callback_fn (type $type)))
  (import "env" "callback_closure" (func $callback_closure (type $type)))
  (import "env" "callback_closure_dynamic" (func $callback_closure_dynamic (type $type)))
  (import "env" "callback_closure_with_env" (func $callback_closure_with_env (type $type)))
  (import "env" "callback_fn_with_vmctx" (func $callback_fn_with_vmctx (type $type)))
  (import "env" "callback_closure_with_vmctx" (func $callback_closure_with_vmctx (type $type)))
  (import "env" "callback_closure_with_vmctx_and_env" (func $callback_closure_with_vmctx_and_env (type $type)))
  (import "env" "callback_fn_trap" (func $callback_fn_trap (type $type)))
  (import "env" "callback_closure_trap" (func $callback_closure_trap (type $type)))
  (import "env" "callback_fn_trap_with_vmctx" (func $callback_fn_trap_with_vmctx (type $type)))
  (import "env" "callback_closure_trap_with_vmctx" (func $callback_closure_trap_with_vmctx (type $type)))
  (import "env" "callback_closure_trap_with_vmctx_and_env" (func $callback_closure_trap_with_vmctx_and_env (type $type)))

  (func (export "function_fn") (type $type)
    get_local 0
    call $callback_fn)

  (func (export "function_closure") (type $type)
    get_local 0
    call $callback_closure)

  (func (export "function_closure_dynamic") (type $type)
    get_local 0
    call $callback_closure_dynamic)

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
    call $callback_closure_trap_with_vmctx_and_env))
"#;

    let mut features = wabt::Features::new();
    features.enable_multi_value();
    let wasm_binary =
        wat2wasm_with_features(MODULE.as_bytes(), features).expect("WAST not valid or malformed");
    let module = compile_with_config(
        &wasm_binary,
        &get_compiler(),
        CompilerConfig {
            features: Features {
                multi_value: true,
                ..Default::default()
            },
            ..Default::default()
        },
    )
    .unwrap();
    let memory_descriptor = MemoryDescriptor::new(Pages(1), Some(Pages(1)), false).unwrap();
    let memory = Memory::new(memory_descriptor).unwrap();

    memory.view()[0].set(SHIFT);

    let import_object = imports! {
        "env" => {
            "memory" => memory.clone(),

            // Regular function.
            "callback_fn" => Func::new(callback_fn),

            // Closure without a captured environment.
            "callback_closure" => Func::new(|n: i32| -> Result<i32, ()> {
                Ok(n + 1)
            }),

            "callback_closure_dynamic" => DynamicFunc::new(
                Arc::new(FuncSig::new(vec![Type::I32], vec![Type::I32])),
                |_, params| -> Vec<Value> {
                    match params[0] {
                        Value::I32(x) => vec![Value::I32(x + 1)],
                        _ => unreachable!()
                    }
                }
            ),

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
    ($test_name:ident, $function:ident, $return_type:ty, $expected_value:expr) => {
        #[test]
        fn $test_name() {
            imported_functions_forms(&|instance| {
                call_and_assert!(instance, $function, $return_type, $expected_value);
            });
        }
    };
}


test!(test_fn, function_fn, i32, Ok(2));
test!(test_closure, function_closure, i32, Ok(2));
test!(test_closure_dynamic, function_closure_dynamic, i32, Ok(2));
test!(
    test_closure_with_env,
    function_closure_with_env,
    i32,
    Ok(2 + shift + SHIFT)
);
test!(
    test_fn_with_vmctx,
    function_fn_with_vmctx,
    i32,
    Ok(2 + SHIFT)
);
test!(
    test_fn_trap,
    function_fn_trap,
    i32,
    Err(RuntimeError(Box::new(format!("foo {}", 2))))
);
test!(
    test_closure_trap,
    function_closure_trap,
    i32,
    Err(RuntimeError(Box::new(format!("bar {}", 2))))
);
test!(
    test_fn_trap_with_vmctx,
    function_fn_trap_with_vmctx,
    i32,
    Err(RuntimeError(Box::new(format!("baz {}", 2 + SHIFT))))
);
test!(
    test_closure_trap_with_vmctx,
    function_closure_trap_with_vmctx,
    i32,
    Err(RuntimeError(Box::new(format!("qux {}", 2 + SHIFT))))
);
test!(
    test_closure_trap_with_vmctx_and_env,
    function_closure_trap_with_vmctx_and_env,
    i32,
    Err(RuntimeError(Box::new(format!("! {}", 2 + shift + SHIFT))))
);

trait ExpectedExpr {
    fn expected_value(n: i32) -> Self;
}
impl ExpectedExpr for i32 {
    fn expected_value(n: i32) -> i32 {
        n + 1
    }
}
impl ExpectedExpr for i64 {
    fn expected_value(n: i32) -> i64 {
        n as i64 + 2i64
    }
}
impl ExpectedExpr for f32 {
    fn expected_value(n: i32) -> f32 {
        n as f32 * 0.1
    }
}
impl ExpectedExpr for f64 {
    fn expected_value(n: i32) -> f64 {
        n as f64 * 0.12
    }
}

macro_rules! mvr_test {
    ($test_name:ident, $( $return_type:ty ),* ) => {
        #[test]
        fn $test_name() {
            let wat: String = r#"
(module
  (type $type (func (param i32) (result
"#.to_string() +
            &stringify!( $( $return_type ),* ).replace(",", "").replace("(", "").replace(")", "") + &r#")))
  (import "env" "callback_fn" (func $callback_fn (type $type)))
  (func (export "test_call") (type $type)
    get_local 0
    call $callback_fn)

  (func (export "test_call_indirect") (type $type)
    (i32.const 1)
    (call_indirect (type $type) (i32.const 0))
  )

  (table funcref
    (elem
      $callback_fn
    )
  )
)
"#.to_string();
            let mut features = wabt::Features::new();
            features.enable_multi_value();
            let wasm_binary =
                wat2wasm_with_features(wat.as_bytes(), features).expect("WAST not valid or malformed");
            let module = compile_with_config(
                &wasm_binary,
                &get_compiler(),
                CompilerConfig {
                    features: Features {
                        multi_value: true,
                        ..Default::default()
                    },
                    ..Default::default()
                },
            )
                .unwrap();
            fn callback_fn(n: i32) -> Result<( $( $return_type ),* ), ()> {
                Ok( ( $( <$return_type>::expected_value(n) ),* ) )
            }
            let import_object = imports! {
                "env" => {
                    "callback_fn" => Func::new(callback_fn),
                },
            };
            let instance = module.instantiate(&import_object).unwrap();

            call_and_assert!(instance, test_call, ( $( $return_type ),* ), Ok( ( $( <$return_type>::expected_value(1) ),* ) ) );
            call_and_assert!(instance, test_call_indirect, ( $( $return_type ),* ), Ok( ( $( <$return_type>::expected_value(1) ),* ) ) );
        }
    }
}

mvr_test!(test_mvr_i32_i32, i32, i32);
mvr_test!(test_mvr_i32_f32, i32, f32);
mvr_test!(test_mvr_f32_i32, f32, i32);
mvr_test!(test_mvr_f32_f32, f32, f32);

mvr_test!(test_mvr_i64_i32, i64, i32);
mvr_test!(test_mvr_i64_f32, i64, f32);
mvr_test!(test_mvr_f64_i32, f64, i32);
mvr_test!(test_mvr_f64_f32, f64, f32);

mvr_test!(test_mvr_i32_i64, i32, i64);
mvr_test!(test_mvr_f32_i64, f32, i64);
mvr_test!(test_mvr_i32_f64, i32, f64);
mvr_test!(test_mvr_f32_f64, f32, f64);

mvr_test!(test_mvr_i32_i32_i32, i32, i32, i32);
mvr_test!(test_mvr_i32_i32_f32, i32, i32, f32);
mvr_test!(test_mvr_i32_f32_i32, i32, f32, i32);
mvr_test!(test_mvr_i32_f32_f32, i32, f32, f32);
mvr_test!(test_mvr_f32_i32_i32, f32, i32, i32);
mvr_test!(test_mvr_f32_i32_f32, f32, i32, f32);
mvr_test!(test_mvr_f32_f32_i32, f32, f32, i32);
mvr_test!(test_mvr_f32_f32_f32, f32, f32, f32);

mvr_test!(test_mvr_i32_i32_i64, i32, i32, i64);
mvr_test!(test_mvr_i32_f32_i64, i32, f32, i64);
mvr_test!(test_mvr_f32_i32_i64, f32, i32, i64);
mvr_test!(test_mvr_f32_f32_i64, f32, f32, i64);
mvr_test!(test_mvr_i32_i32_f64, i32, i32, f64);
mvr_test!(test_mvr_i32_f32_f64, i32, f32, f64);
mvr_test!(test_mvr_f32_i32_f64, f32, i32, f64);
mvr_test!(test_mvr_f32_f32_f64, f32, f32, f64);

mvr_test!(test_mvr_i32_i64_i32, i32, i64, i32);
mvr_test!(test_mvr_i32_i64_f32, i32, i64, f32);
mvr_test!(test_mvr_f32_i64_i32, f32, i64, i32);
mvr_test!(test_mvr_f32_i64_f32, f32, i64, f32);
mvr_test!(test_mvr_i32_f64_i32, i32, f64, i32);
mvr_test!(test_mvr_i32_f64_f32, i32, f64, f32);
mvr_test!(test_mvr_f32_f64_i32, f32, f64, i32);
mvr_test!(test_mvr_f32_f64_f32, f32, f64, f32);

mvr_test!(test_mvr_i64_i32_i32, i64, i32, i32);
mvr_test!(test_mvr_i64_i32_f32, i64, i32, f32);
mvr_test!(test_mvr_i64_f32_i32, i64, f32, i32);
mvr_test!(test_mvr_i64_f32_f32, i64, f32, f32);
mvr_test!(test_mvr_f64_i32_i32, f64, i32, i32);
mvr_test!(test_mvr_f64_i32_f32, f64, i32, f32);
mvr_test!(test_mvr_f64_f32_i32, f64, f32, i32);
mvr_test!(test_mvr_f64_f32_f32, f64, f32, f32);

mvr_test!(test_mvr_i32_i32_i32_i32, i32, i32, i32, i32);
mvr_test!(test_mvr_i32_i32_i32_f32, i32, i32, i32, f32);
mvr_test!(test_mvr_i32_i32_f32_i32, i32, i32, f32, i32);
mvr_test!(test_mvr_i32_i32_f32_f32, i32, i32, f32, f32);
mvr_test!(test_mvr_i32_f32_i32_i32, i32, f32, i32, i32);
mvr_test!(test_mvr_i32_f32_i32_f32, i32, f32, i32, f32);
mvr_test!(test_mvr_i32_f32_f32_i32, i32, f32, f32, i32);
mvr_test!(test_mvr_i32_f32_f32_f32, i32, f32, f32, f32);
mvr_test!(test_mvr_f32_i32_i32_i32, f32, i32, i32, i32);
mvr_test!(test_mvr_f32_i32_i32_f32, f32, i32, i32, f32);
mvr_test!(test_mvr_f32_i32_f32_i32, f32, i32, f32, i32);
mvr_test!(test_mvr_f32_i32_f32_f32, f32, i32, f32, f32);
mvr_test!(test_mvr_f32_f32_i32_i32, f32, f32, i32, i32);
mvr_test!(test_mvr_f32_f32_i32_f32, f32, f32, i32, f32);
mvr_test!(test_mvr_f32_f32_f32_i32, f32, f32, f32, i32);
mvr_test!(test_mvr_f32_f32_f32_f32, f32, f32, f32, f32);

mvr_test!(test_mvr_i32_i32_i32_i32_i32, i32, i32, i32, i32, i32);
