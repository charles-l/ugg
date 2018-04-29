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

extern "C" {

static void sdl_die(const char * message) {
  fprintf(stderr, "%s: %s\n", message, SDL_GetError());
  exit(2);
}

unsigned int gen_vao() {
  unsigned int id;
  glGenVertexArrays(1, &id);
  glBindVertexArray(id);
  return id;
}

void dump_mat4(hmm_mat4 m) {
  printf("(");
  for(int i = 0; i < 4; i++) {
    for(int j = 0; j < 4; j++) {
      printf("%f ", m.Elements[j][i]);
    }
    if(i == 3) printf(")");
    printf("\n");
  }
}

hmm_vec3 mat4_relative_move(hmm_mat4 m, hmm_vec3 cur_pos, float dx, float dz, float speed) {
  hmm_vec3 f = HMM_Vec3(m.Elements[0][2], m.Elements[1][2], m.Elements[2][2]);
  hmm_vec3 s = HMM_Vec3(m.Elements[0][0], m.Elements[1][0], m.Elements[2][0]);
  return cur_pos + speed * (-dz * f + dx * s);
}

hmm_mat4 make_id_mat() {
  return HMM_Mat4d(1.0);
}

hmm_mat4 calculate_view(hmm_vec3 pos, float yaw, float pitch) {
  hmm_vec3 up = HMM_Vec3(0, 1, 0);
  hmm_vec3 right = HMM_Vec3(1, 0, 0);
  hmm_mat4 yrot = HMM_Rotate(yaw, up);
  hmm_mat4 prot = HMM_Rotate(pitch, right);
  hmm_mat4 trans = HMM_Translate(pos);
  hmm_mat4 view = prot * yrot * trans;
  return view;
}

hmm_mat4 make_translate_matrix(hmm_vec3 v) {
  hmm_mat4 m = HMM_Translate(v);
  return m;
}

hmm_mat4 make_projection(){
    hmm_mat4 projection = HMM_Perspective(45.0f, ((float) SCREEN_WIDTH / (float) SCREEN_HEIGHT), 0.1f, 100.f);
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


    glViewport(0, 0, w, h);
}

SDL_Window* get_win() {
  return window;
}

unsigned int compile_shader(const char *source, unsigned int type) {
  GLuint id = glCreateShader(type);
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

unsigned int link_program(unsigned int vshader, unsigned int fshader) {
  unsigned int p = glCreateProgram();
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

void clear_frame(float r, float g, float b) {
  glClearColor(r, g, b, 0.0f);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}

static inline void bind_vao_and_vbo(unsigned int vao, unsigned int array_id, size_t n) {
  glBindVertexArray(vao);

  { // attribute 0: vertices
    glEnableVertexAttribArray(0);
    glBindBuffer(GL_ARRAY_BUFFER, array_id);
  }
}

static inline void unbind_vao_and_vbo() {
  glDisableVertexAttribArray(0);
  glBindVertexArray(0);
}

void draw_lines(unsigned int vao, unsigned int array_id, size_t n, bool connected) {
  bind_vao_and_vbo(vao, array_id, n);
  {
    glDrawArrays(connected ? GL_LINE_STRIP : GL_LINES, 0, n);
  }
  unbind_vao_and_vbo();
}

// TODO write a proper texture manager
unsigned int load_texture(char *fname) {
  static int ntexs = 0;
  assert(ntexs < 32);

  unsigned int id;
  glGenTextures(1, &id);

  int w, h;
  unsigned char *img;
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

void draw_elements(unsigned int vao, unsigned int array_id, unsigned int element_array_id, int uv_array_id, size_t n) {
  bind_vao_and_vbo(vao, array_id, n);
  if(uv_array_id != -1) {
    glEnableVertexAttribArray(1);
    glBindBuffer(GL_ARRAY_BUFFER, uv_array_id);
  }

  {
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, element_array_id);
    glDrawElements(GL_TRIANGLES, n, GL_UNSIGNED_INT, NULL);
  }

  if(uv_array_id != -1) {
    glDisableVertexAttribArray(1);
  }
  unbind_vao_and_vbo();
}

unsigned int gen_element_vbo(unsigned int *buf, size_t n) {
  unsigned int id;
  glGenBuffers(1, &id);

  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, id);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(unsigned int) * n, buf, GL_STATIC_DRAW);

  return id;
}

unsigned int gen_vert_vbo(float *buf, size_t n) {
  unsigned int id;
  glGenBuffers(1, &id);

  // !!! assumes tris
  glBindBuffer(GL_ARRAY_BUFFER, id);
  glBufferData(GL_ARRAY_BUFFER, sizeof(float) * n, buf, GL_STATIC_DRAW);
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, NULL);

  return id;
}

unsigned int gen_uv_vbo(float *buf, size_t n) {
  unsigned int id;
  glGenBuffers(1, &id);

  glBindBuffer(GL_ARRAY_BUFFER, id);
  glBufferData(GL_ARRAY_BUFFER, sizeof(float) * n, buf, GL_STATIC_DRAW);
  glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 0, NULL);

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

void main_loop(void (*f)(float), void (*g)(SDL_Event, float)) {
  SDL_Event event;
  SDL_SetRelativeMouseMode(should_lock_mouse);
  Uint64 last = 0;
  Uint64 now = SDL_GetPerformanceCounter();
  while (true) {
    last = now;
    now = SDL_GetPerformanceCounter();
    float dt = ((float)(now - last)) / SDL_GetPerformanceFrequency();

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
