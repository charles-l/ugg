#version 400 core
out vec3 ocolor;
in vec3 normal;
uniform vec3 color;
void main(){
  ocolor = color * abs(normal);
}
