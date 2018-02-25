#version 440 core
layout(location = 0) in vec3 vpos;

uniform mat4 mvp;

void main() {
    gl_Position = mvp * vec4(vpos, 1);
}
