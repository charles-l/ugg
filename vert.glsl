#version 400 core
layout(location = 0) in vec3 vpos;

void main() {
    gl_Position.xyz = vpos;
    gl_Position.w = 1.0;
}
