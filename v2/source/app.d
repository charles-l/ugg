import opengl.gl4;
import glfw3d;
import gfm.math;
import std.stdio;
import std.string;
import std.file;
import std.math;

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

uint compileShader(string source, uint type) {
    uint id = glCreateShader(type);
    char *s = cast(char *) toStringz(source);
    glShaderSource(id, 1u, cast(const(char**)) &s, cast(const(int *)) null);
    glCompileShader(id);
    int result;
    int loglen;
    glGetShaderiv(id, GL_COMPILE_STATUS, &result);
    glGetShaderiv(id, GL_INFO_LOG_LENGTH, &loglen);
    if(loglen > 0) {
        auto log = new char[loglen + 1];
        glGetShaderInfoLog(id, cast(uint) log.length, cast(uint *) null, log.ptr);
        writeln("%s\n", fromStringz(log.ptr));
        log.destroy;
    }
    return id;
}

uint linkProgram(uint vshader, uint fshader) {
    uint p = glCreateProgram();
    glAttachShader(p, vshader);
    glAttachShader(p, fshader);
    glLinkProgram(p);

    int result, loglen;
    glGetShaderiv(p, GL_COMPILE_STATUS, &result);
    glGetShaderiv(p, GL_INFO_LOG_LENGTH, &loglen);
    if(loglen > 0) {
        auto log = new char[loglen + 1];
        glGetProgramInfoLog(p, cast(uint) log.length, cast(uint *) null, log.ptr);
        writeln("%s\n", fromStringz(log.ptr));
        log.destroy;
    }

    glDeleteShader(vshader);
    glDeleteShader(fshader);

    return p;
}

// TODO rewrite this sucker
uint loadTexture(string fname) {
    import gfm.freeimage;

    static int ntexs = 0;
    assert(ntexs < 32);

    uint id;
    glGenTextures(1, &id);

    int w, h;
    glActiveTexture(GL_TEXTURE0 + ntexs++);
    glBindTexture(GL_TEXTURE_2D, id);
    // XXX - I don't think I should be creating a new FreeImage every time
    auto img = new FIBitmap(new FreeImage(), fname);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, img.width(), img.height(), 0, GL_RGB, GL_UNSIGNED_BYTE, img.data());

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    // XXX not tracking texture id
    return ntexs - 1;
}

auto projection = mat4f.perspective(PI / 4.0, 640.0 / 480.0, 0.1, 100.0);

vec3f pos = [0, 0, 0];

vec3f direction = [0, 0, -1];

vec3f up = [0, 1, 0];
vec3f right = [1, 0, 0];

extern(C) nothrow void keyCallback(GLFWwindow *, int key, int scancode, int action, int mods) {
    switch(key) {
    case GLFW_KEY_W:
        pos += direction;
        break;
    case GLFW_KEY_S:
        pos -= direction;
        break;
    case GLFW_KEY_A:
        pos -= direction.cross(up).normalized();
        break;
    case GLFW_KEY_D:
        pos += direction.cross(up).normalized();
        break;
    default:
        printf("huh?");
    }
}

extern(C) nothrow void cursorPositionCallback(GLFWwindow *w, double xpos, double ypos) {
    vec3f cameraRight = direction.cross(up).normalized();
    direction = vec3f((mat4f.rotation(xpos / 1000, up) * vec4f(direction, 1)).xyz);

    // TODO: clamp the pitch of the camera - othewise weird stuff will happen
    direction = vec3f((mat4f.rotation(ypos / 1000, cameraRight) * vec4f(direction, 1)).xyz);
    glfwSetCursorPos(w, 0, 0);
}

void main() {
    glfw3dInit();

    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 5);

    Window w = new Window(640, 480, "TEST");
    w.makeContextCurrent();
    w.setKeyCallback(&keyCallback);

    glfwSetInputMode(w.ptr, GLFW_CURSOR, GLFW_CURSOR_DISABLED);
    glfwSetCursorPosCallback(w.ptr, &cursorPositionCallback);
    glfwSetCursorPos(w.ptr, 0, 0);


    { // load opengl
        import opengl.loader;
        loadGL!(opengl.gl4);
    }

    glEnable(GL_MULTISAMPLE);
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);
    glDepthFunc(GL_LESS);
    glPointSize(4);

    Mesh m;
    m.vao = genVAO();
    m.verts = 4;
    genVBO!(float, 3)(0, [
            0,0,0.2,
            0.1,0.1,0.2,
            1, -1, 1,
            -1, -1, -1,
            ]);
    glBindVertexArray(0);

    string vertSrc = cast(string) read("./vert.glsl", 512);
    string fragSrc = cast(string) read("./mono.glsl", 512);
    uint prog = linkProgram(
            compileShader(vertSrc, GL_VERTEX_SHADER),
            compileShader(fragSrc, GL_FRAGMENT_SHADER));

    glUseProgram(prog);
    while(!w.shouldClose()) {
        glfwPollEvents();

        uint mvpid = glGetUniformLocation(prog, "mvp");
        mat4f view = mat4f.lookAt(pos, pos + direction, up);
        mat4f mvp = projection * view * mat4f.identity;

        glUniformMatrix4fv(mvpid, 1, true /* matrix is in row major */, mvp.ptr);

        { // draw stuff
            clearFrame(0, 0.1, 0);
            drawLines(m);
            w.swapBuffers();
        }
    }
}
