#version 440 core
layout(location = 0) in vec3 vpos;
layout(location = 1) in vec2 texcoord;

uniform mat4 mvp;

out vec3 normal;
out vec2 vtexcoord;

void main() {
    normal = vpos;
    vtexcoord = texcoord;
    gl_Position = mvp * vec4(vpos, 1);
}
