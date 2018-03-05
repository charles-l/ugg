#version 440 core
layout(location = 0) in vec3 vpos;

uniform mat4 mvp;

out vec3 normal;

void main() {
    normal = vpos;
    gl_Position = mvp * vec4(vpos, 1);
}
