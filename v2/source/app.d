import opengl.gl4;
import opengl.loader;
//import opengl.d;
import glfw3d;
import std.stdio;

void clear_frame(float r, float g, float b) {
    glClearColor(r, g, b, 0.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}

uint gen_vao() {
    uint id;
    glGenVertexArrays(1, &id);
    glBindVertexArray(id);
    return id;
}

// T - type of individual compoennt
// N - number of components
template gen_vbo(T, uint N) {
    uint gen_vbo(uint i, T[] buf) {
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

void draw_points(uint vao, uint n) {
    glBindVertexArray(vao);
    glDrawArrays(GL_POINTS, 0, n);
    glBindVertexArray(0);
}

void main() {
    glfw3dInit();

    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 5);

    Window w = new Window(640, 480, "TEST");
    w.makeContextCurrent();

    loadGL!(opengl.gl4);

    uint vao = gen_vao();
    gen_vbo!(float, 3)(0, [0,0,0,0.2,0.3,0]);
    glPointSize(10.0);
    glBindVertexArray(0);

    while(!w.shouldClose()) {
        glfwPollEvents();
        clear_frame(0, 0.1, 0);

        draw_points(vao, 2);

        w.swapBuffers();
    }
}
