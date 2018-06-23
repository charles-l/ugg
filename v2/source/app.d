import opengl.gl4;
import opengl.loader;
import glfw3d;
import gfm.math;
import gfm.freeimage;
import std.stdio;

void clearFrame(float r, float g, float b) {
    glClearColor(r, g, b, 0.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}

struct VaoID {
    uint id;
    alias id this;
}

struct Mesh {
    VaoID vao;
    size_t verts;
    // TODO: maybe track vbo ids?
}

VaoID genVAO() {
    uint id;
    glGenVertexArrays(1, &id);
    glBindVertexArray(id);
    return VaoID(id);
}

// T - type of individual compoennt
// N - number of components
template genVBO(T, uint N) {
    uint genVBO(uint i, T[] buf) {
        uint id;
        glGenBuffers(1, &id);

        // !!! assumes tris
        glBindBuffer(GL_ARRAY_BUFFER, id);
        glBufferData(GL_ARRAY_BUFFER, T.sizeof * buf.length, buf.ptr, GL_STATIC_DRAW);
        glVertexAttribPointer(i, N, GL_FLOAT, GL_FALSE, 0, null);
        glEnableVertexAttribArray(i);

        return id;
    }
}

void drawPoints(Mesh m) {
    glBindVertexArray(m.vao);
    glDrawArrays(GL_POINTS, 0, cast(uint) m.verts);
    glBindVertexArray(0);
}

template drawLines(bool connected = false) {
    void drawLines(Mesh m) {
        glBindVertexArray(m.vao);
        static if(connected) {
            glDrawArrays(GL_LINE_STRIP, 0, cast(uint) m.verts);
        } else {
            glDrawArrays(GL_LINES, 0, cast(uint) m.verts);
        }
        glBindVertexArray(0);
    }
}

void drawElements(Mesh m) {
  glBindVertexArray(m.vao);
  glDrawElements(GL_TRIANGLES, cast(uint) m.verts, GL_UNSIGNED_INT, null);
  glBindVertexArray(0);
}

// TODO rewrite this sucker
uint loadTexture(string fname) {
    static int ntexs = 0;
    assert(ntexs < 32);

    uint id;
    glGenTextures(1, &id);

    int w, h;
    glActiveTexture(GL_TEXTURE0 + ntexs++);
    glBindTexture(GL_TEXTURE_2D, id);
    auto img = new FIBitmap(new FreeImage(), fname);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, img.width(), img.height(), 0, GL_RGB, GL_UNSIGNED_BYTE, img.data());

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    // XXX not tracking texture id
    return ntexs - 1;
}

void main() {
    glfw3dInit();

    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 5);

    Window w = new Window(640, 480, "TEST");
    w.makeContextCurrent();

    loadGL!(opengl.gl4);

    glEnable(GL_MULTISAMPLE);
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);
    glDepthFunc(GL_LESS);
    glPointSize(4);


    Mesh m;
    m.vao = genVAO();
    m.verts = 2;
    genVBO!(float, 3)(0, [0,0,0,0.2,0.3,0]);
    glBindVertexArray(0);

    while(!w.shouldClose()) {
        glfwPollEvents();
        clearFrame(0, 0.1, 0);

        drawLines(m);

        w.swapBuffers();
    }
}
