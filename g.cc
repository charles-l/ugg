#include <cstdio>
#include <cstdlib>

#define HANDMADE_MATH_IMPLEMENTATION
#include "HandmadeMath.h"
#include <SDL2/SDL.h>

#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"

#include <GL/glew.h>
#include "GL/gl.h"
#include "GL/glut.h"
#include <math.h>

static const int SCREEN_FULLSCREEN = 0;
static const int SCREEN_WIDTH  = 1280;
static const int SCREEN_HEIGHT = 720;
static SDL_Window *window = NULL;
static SDL_GLContext maincontext;

typedef hmm_vec3 v3;
typedef hmm_vec4 v4;
typedef hmm_mat4 m44;
typedef float f32;
typedef unsigned int u32;
typedef unsigned char u8;

extern "C" {

static void sdl_die(const char * message) {
  fprintf(stderr, "%s: %s\n", message, SDL_GetError());
  exit(2);
}

u32 gen_vao() {
  u32 id;
  glGenVertexArrays(1, &id);
  glBindVertexArray(id);
  return id;
}

void dump_mat4(m44 m) {
  printf("(");
  for(int i = 0; i < 4; i++) {
    for(int j = 0; j < 4; j++) {
      printf("%f ", m.Elements[j][i]);
    }
    if(i == 3) printf(")");
    printf("\n");
  }
}

v3 mat4_relative_move(m44 m, v3 cur_pos, f32 dx, f32 dz, f32 speed) {
  v3 f = HMM_Vec3(m.Elements[0][2], m.Elements[1][2], m.Elements[2][2]);
  v3 s = HMM_Vec3(m.Elements[0][0], m.Elements[1][0], m.Elements[2][0]);
  return cur_pos + speed * (-dz * f + dx * s);
}
m44 make_id_mat() {
  return HMM_Mat4d(1.0);
}

m44 calculate_view(v3 pos, f32 yaw, f32 pitch) {
  v3 up = HMM_Vec3(0, 1, 0);
  v3 right = HMM_Vec3(1, 0, 0);
  m44 yrot = HMM_Rotate(yaw, up);
  m44 prot = HMM_Rotate(pitch, right);
  m44 trans = HMM_Translate(pos);
  m44 view = prot * yrot * trans;
  return view;
}

m44 make_translate_matrix(v3 v) {
  m44 m = HMM_Translate(v);
  return m;
}

v3 transform_v(m44 m, v3 v) {
  v4 r;
  r.XYZ = v;
  r.W = 1;
  r = (m * r);
  return r.XYZ;
}

m44 make_projection(){
  m44 projection = HMM_Perspective(45.0f, ((f32) SCREEN_WIDTH / (f32) SCREEN_HEIGHT), 0.1f, 100.f);
  return projection;
}

void init_screen(const char *caption) {
    if (SDL_Init(SDL_INIT_VIDEO) < 0)
        sdl_die("Couldn't initialize SDL");
    atexit (SDL_Quit);

    SDL_GL_LoadLibrary(NULL);
    // Request an OpenGL 4.5 context (should be core)
    SDL_GL_SetAttribute(SDL_GL_ACCELERATED_VISUAL, 1);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 4);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 5);
    // Also request a depth buffer
    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
    SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 24);

    if (SCREEN_FULLSCREEN) {
        window = SDL_CreateWindow(
                caption,
                SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
                0, 0, SDL_WINDOW_FULLSCREEN_DESKTOP | SDL_WINDOW_OPENGL
                );
    } else {
        window = SDL_CreateWindow(
                caption,
                SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
                SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_OPENGL
                );
    }
    if (window == NULL) sdl_die("Couldn't set video mode");

    maincontext = SDL_GL_CreateContext(window);
    SDL_SetHint( SDL_HINT_RENDER_SCALE_QUALITY, "1" );
    if (maincontext == NULL)
        sdl_die("Failed to create OpenGL context");

    int w,h;
    SDL_GetWindowSize(window, &w, &h);
    SDL_GL_MakeCurrent(window, maincontext);

    glewInit();

    glEnable(GL_MULTISAMPLE);
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);
    glDepthFunc(GL_LESS);

    glPointSize(4);

    glViewport(0, 0, w, h);
}

SDL_Window* get_win() {
  return window;
}

u32 compile_shader(const char *source, u32 type) {
  u32 id = glCreateShader(type);
  glShaderSource(id, 1, &source, NULL);
  glCompileShader(id);
  int result;
  int loglen;
  glGetShaderiv(id, GL_COMPILE_STATUS, &result);
  glGetShaderiv(id, GL_INFO_LOG_LENGTH, &loglen);
  if(loglen > 0) {
    char *e = (char *) malloc(loglen + 1);
    glGetShaderInfoLog(id, loglen, NULL, e);
    printf("%s\n", e);
    free(e);
  }
  return id;
}

u32 link_program(u32 vshader, u32 fshader) {
  u32 p = glCreateProgram();
  glAttachShader(p, vshader);
  glAttachShader(p, fshader);
  glLinkProgram(p);

  int result, loglen;
  glGetProgramiv(p, GL_LINK_STATUS, &result);
  glGetProgramiv(p, GL_INFO_LOG_LENGTH, &loglen);
  if(loglen > 0) {
    char *e = (char *) malloc(loglen + 1);
    glGetProgramInfoLog(p, loglen, NULL, e);
    printf("%s\n", e);
  }


  glDeleteShader(vshader);
  glDeleteShader(fshader);

  return p;
}

void clear_frame(f32 r, f32 g, f32 b) {
  glClearColor(r, g, b, 0.0f);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}

// TODO write a proper texture manager
u32 load_texture(char *fname) {
  static int ntexs = 0;
  assert(ntexs < 32);

  u32 id;
  glGenTextures(1, &id);

  int w, h;
  u8 *img;
  glActiveTexture(GL_TEXTURE0 + ntexs++);
  glBindTexture(GL_TEXTURE_2D, id);
  img = stbi_load(fname, &w, &h, NULL, STBI_rgb);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, w, h, 0, GL_RGB, GL_UNSIGNED_BYTE, img);
  stbi_image_free(img);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  // XXX not tracking texture id
  return ntexs - 1;
}

void draw_elements(u32 vao, size_t n) {
  glBindVertexArray(vao);
  glDrawElements(GL_TRIANGLES, n, GL_UNSIGNED_INT, NULL);
  glBindVertexArray(0);
}

void draw_lines(u32 vao, size_t n, bool connected) {
  glBindVertexArray(vao);
  glDrawArrays(connected ? GL_LINE_STRIP : GL_LINES, 0, n);
  glBindVertexArray(0);
}

void draw_points(u32 vao, size_t n) {
  glBindVertexArray(vao);
  glDrawArrays(GL_POINTS, 0, n);
  glBindVertexArray(0);
}

u32 gen_element_vbo(u32 *buf, size_t n) {
  u32 id;
  glGenBuffers(1, &id);

  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, id);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(u32) * n, buf, GL_STATIC_DRAW);

  return id;
}

u32 gen_vbo_3f(uint i, f32 *buf, size_t n) {
  u32 id;
  glGenBuffers(1, &id);

  // !!! assumes tris
  glBindBuffer(GL_ARRAY_BUFFER, id);
  glBufferData(GL_ARRAY_BUFFER, sizeof(f32) * n, buf, GL_STATIC_DRAW);
  glVertexAttribPointer(i, 3, GL_FLOAT, GL_FALSE, 0, NULL);
  glEnableVertexAttribArray(i);

  return id;
}

u32 gen_vbo_2f(uint i, f32 *buf, size_t n) {
  u32 id;
  glGenBuffers(1, &id);

  glBindBuffer(GL_ARRAY_BUFFER, id);
  glBufferData(GL_ARRAY_BUFFER, sizeof(f32) * n, buf, GL_STATIC_DRAW);
  glVertexAttribPointer(i, 2, GL_FLOAT, GL_FALSE, 0, NULL);
  glEnableVertexAttribArray(i);

  return id;
}

char is_key_down(int k) {
  const Uint8 *state = SDL_GetKeyboardState(NULL);
  return state[k];
}

SDL_bool should_lock_mouse =
#ifdef NDEBUG
SDL_TRUE;
#else
SDL_FALSE;
#endif

void main_loop(void (*f)(f32), void (*g)(SDL_Event, f32)) {
  SDL_Event event;
  SDL_SetRelativeMouseMode(should_lock_mouse);
  Uint64 last = 0;
  Uint64 now = SDL_GetPerformanceCounter();
  while (true) {
    last = now;
    now = SDL_GetPerformanceCounter();
    f32 dt = ((f32)(now - last)) / SDL_GetPerformanceFrequency();

    SDL_GL_SwapWindow(window);
    while (SDL_PollEvent(&event)) {
      if (event.type == SDL_QUIT) {
        printf("quit\n");
        return;
      }
#ifndef NDEBUG // DEBUG BINDINGS
      if (event.type == SDL_KEYDOWN && event.key.keysym.sym == SDLK_ESCAPE) {
        should_lock_mouse = (SDL_bool) !should_lock_mouse;
        SDL_SetRelativeMouseMode(should_lock_mouse);
      }
#endif
      g(event, dt);
    }
    SDL_PumpEvents();
    f(dt);
  }
}

}
