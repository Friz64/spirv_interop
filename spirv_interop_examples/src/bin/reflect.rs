spirv_interop::reflect!(mod reflect_test: "src/bin/reflect_test.vert.spv");

fn main() {
    dbg!(reflect_test::ENTRY_POINTS);
    dbg!(reflect_test::UNIFORM_VARS);
    dbg!(reflect_test::input_attrs());
}
