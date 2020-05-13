#version 450

layout(set = 0, binding = 0) uniform Test {
    mat4 matrix1;
    layout(row_major) mat4 matrix2;
    mat2x4 matrix3;
    float[2] array1;
    float[2][3] array2;
    bool boolean;
    int integer;
    uint uinteger;
    float float32;
    double float64;
    bvec2 boolvec;
    ivec3 intvec;
    uvec4 uintvec;
    vec2 floatvec;
    dvec3 doublevec;
    float[] unsized_array;
}
test[3];
//layout(binding = 1) uniform sampler2D texSampler;

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inColor;
layout(location = 2) in vec2 inTexCoord;

layout(location = 0) out vec3 fragColor;
layout(location = 1) out vec2 fragTexCoord;

void main() {
    // ensures that the compiler does not optimize out structs
    gl_Position = test[0].matrix1[0];
    //gl_Position = texture(texSampler, test[0].matrix1[0].xy);
}
