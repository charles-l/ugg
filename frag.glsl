#version 400 core
out vec3 color;
in vec3 normal;
void main(){
  color = vec3(1,0,0) * abs(normal);
}
