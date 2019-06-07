#[test]
fn test_lseek() {
    assert_emscripten_output!(
        "../../emtests/lseek.wasm",
        "lseek",
        vec![],
        "../../emtests/lseek.out"
    );
}
