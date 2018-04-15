#version 400 core
out vec3 ocolor;

in vec3 normal;
in vec2 vtexcoord;

uniform sampler2D tex;
uniform vec3 color;
void main(){
  ocolor = texture2D(tex, vtexcoord).xyz * color;
}
