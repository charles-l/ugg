#include <cstdio>
#include <cstdlib>

#define GLM_FORCE_RADIANS 1
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <SDL2/SDL.h>

#include <GL/glew.h>
#include "GL/gl.h"
#include "GL/glut.h"

static const int SCREEN_FULLSCREEN = 0;
static const int SCREEN_WIDTH  = 960;
static const int SCREEN_HEIGHT = 540;
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
    if (maincontext == NULL)
        sdl_die("Failed to create OpenGL context");

    int w,h;
    SDL_GetWindowSize(window, &w, &h);
    SDL_GL_MakeCurrent(window, maincontext);
    glewInit();

    glViewport(0, 0, w, h);
    glClearColor(0.0f, 0.5f, 1.0f, 0.0f);
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

void clear_frame() {
  glClear(GL_COLOR_BUFFER_BIT);
}

void draw_array(unsigned int vao, unsigned int vbo) {
  glBindVertexArray(vao);
  glEnableVertexAttribArray(0);
  glBindBuffer(GL_ARRAY_BUFFER, vbo);
  glVertexAttribPointer(
      0, 3, GL_FLOAT, GL_FALSE, 0, NULL
    );
  glDrawArrays(GL_TRIANGLES, 0, 3);
  glDisableVertexAttribArray(0);
  glBindVertexArray(0);
}

unsigned int gen_vbo(float *buf, size_t n) {
  unsigned int id;
  glGenBuffers(1, &id);
  glBindBuffer(GL_ARRAY_BUFFER, id);
  glBufferData(GL_ARRAY_BUFFER, sizeof(float) * n, buf, GL_STATIC_DRAW);

  return id;
}

void main_loop(void (*f)(void)) {
  SDL_Event event;
  bool quit = false;
  while (!quit) {
    SDL_GL_SwapWindow(window);
    while (SDL_PollEvent(&event)) {
      if (event.type == SDL_QUIT) {
        quit = true;
      }
      f();
    }
  }
}

}
