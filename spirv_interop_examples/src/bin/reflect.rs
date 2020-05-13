spirv_interop::reflect!(mod reflect_test: "src/bin/reflect_test.vert.spv");

fn main() {
    dbg!(reflect_test::ENTRY_POINTS);
    dbg!(reflect_test::UNIFORM_VARIABLES);
    //dbg!(reflect_test::UniformTest {});
}
