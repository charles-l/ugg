#version 400 core
out vec3 ocolor;

in vec3 pos;
in vec3 normal;
in vec2 vtexcoord;

uniform sampler2D tex;
uniform vec3 color;
uniform vec3 lightPos;

#define MAX_LIGHTS 8

void main(){
  vec3 norm = normalize(-normal);
  vec3 lightDir = normalize(lightPos - pos);
  float diff = max(dot(norm, lightDir), 0.0);
  ocolor = diff * texture2D(tex, vtexcoord).xyz;
}
