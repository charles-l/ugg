#version 400 core
out vec3 ocolor;
in vec3 normal; // bit of a cheat - we're using the vertex shader, and storing colors in the normal channel

void main(){
  ocolor = normal;
}
