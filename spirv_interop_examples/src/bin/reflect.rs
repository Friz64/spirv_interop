spirv_interop::reflect!(mod uniform: "src/bin/reflect.vert.spv");

fn main() {
    dbg!(uniform::ENTRY_POINTS);
    dbg!(uniform::Uniform {
        model: (),
        view: (),
        proj: (),
        integer: (),
        vector: (),
        test: (),
    });
}
