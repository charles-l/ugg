import opengl.gl4;
import glfw3d;
import gfm.math;
import std.stdio;
import std.string;
import std.file;
import std.math;
import mesh;
import debug_draw;

void clearFrame(float r, float g, float b) {
    glClearColor(r, g, b, 0.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}

void drawElements(Mesh m) {
    glBindVertexArray(m.vao);
    glDrawElements(GL_TRIANGLES, cast(uint) m.nelems * 3, GL_UNSIGNED_INT, null);
    glBindVertexArray(0);
}

// TODO rewrite this sucker
uint loadTexture(string fname) {
    import imageformats;

    static int ntexs = 0;
    assert(ntexs < 32);

    uint id;
    glGenTextures(1, &id);

    int w, h;
    glActiveTexture(GL_TEXTURE0 + ntexs++);
    glBindTexture(GL_TEXTURE_2D, id);
    IFImage img = read_image(fname, ColFmt.RGB);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, img.w, img.h, 0, GL_RGB, GL_UNSIGNED_BYTE,
            img.pixels.ptr);

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

debug bool mouseCaptured = true;

extern(C) nothrow void cursorPositionCallback(GLFWwindow *w, double xpos, double ypos) {
    debug {
        if(!mouseCaptured)
            return;
    }

    vec3f cameraRight = direction.cross(up).normalized();
    direction = vec3f((mat4f.rotation(-xpos / 1000, up) * vec4f(direction, 1)).xyz);

    // TODO: clamp the pitch of the camera - othewise weird stuff will happen
    direction = vec3f((mat4f.rotation(-ypos / 1000, cameraRight) * vec4f(direction, 1)).xyz);
    glfwSetCursorPos(w, 0, 0);
}


extern(C) nothrow void keyCallback(GLFWwindow *win, int key, int scancode, int action, int mods) {
    debug {
        if(key == GLFW_KEY_ESCAPE && action == GLFW_PRESS) {
            mouseCaptured = !mouseCaptured;
            if(!mouseCaptured) {
                glfwSetInputMode(win, GLFW_CURSOR, GLFW_CURSOR_NORMAL);
            } else {
                glfwSetInputMode(win, GLFW_CURSOR, GLFW_CURSOR_DISABLED);
                glfwSetCursorPos(win, 0, 0);
            }
        }
    }
}

void handleInput(GLFWwindow *win) {
    const float speed = 0.1;

    if(glfwGetKey(win, GLFW_KEY_W)) {
        pos += direction * speed;
    }

    if(glfwGetKey(win, GLFW_KEY_S)) {
        pos -= direction * speed;
    }

    if(glfwGetKey(win, GLFW_KEY_A)) {
        pos -= direction.cross(up).normalized() * speed;
    }

    if(glfwGetKey(win, GLFW_KEY_D)) {
        pos += direction.cross(up).normalized() * speed;
    }
}

void setUniform(T)(Program p, string u, T v) {
    import std.traits;
    import std.conv;
    uint id = glGetUniformLocation(p.id, toStringz(u));
    static if(isFloatingPoint!T) {
        glUniform1f(id, v);
    } else static if (isIntegral!T) {
        glUniform1i(id, v);
    } else static if (is(T == vec3f)) {
        glUniform3fv(id, 1, v.ptr);
    } else static if(is(T == mat4f)) {
        glUniformMatrix4fv(id, 1, true, v.ptr);
    } else {
        static assert(false, "type" ~ T.stringof ~ "not handled in setUniform");
    }

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

    glEnable(GL_MULTISAMPLE);
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);
    glDepthFunc(GL_LESS);
    glPointSize(4);

    // TODO: do this behind the scenes and use a hashtable to prevent shader duplication
    // TODO: do lazy updates of uniforms
    auto vertexShader = _compileShader(cast(string) read("./vert.glsl", 512), GL_VERTEX_SHADER);
    auto fragShader = _compileShader(cast(string) read("./frag.glsl", 512), GL_FRAGMENT_SHADER);
    auto monoShader = _compileShader(cast(string) read("./mono.glsl", 512), GL_FRAGMENT_SHADER);
    Program prog = makeProgram(
            vertexShader,
            fragShader);

    Program debugProg = makeProgram(vertexShader, monoShader);

    Mesh m = loadMesh("x.sdl");
    Mesh p = makePlane(4);
    Mesh s = makeSphere(1);

    setUniform(prog, "tex", loadTexture("../img.png"));

    DebugContext dbg = initDebug();

    while(!w.shouldClose()) {
        glfwPollEvents();
        handleInput(w.ptr);

        mat4f view = mat4f.lookAt(pos, pos + direction, up);
        mat4f mvp = projection * view * mat4f.identity;

        { // draw stuff
            use(prog);
            setUniform(prog, "mvp", mvp);

            clearFrame(0, 0.1, 0);
            //drawElements(m);
            //drawElements(p);

            drawElements(s);
            use(debugProg);
            setUniform(debugProg, "mvp", mvp);
            setUniform(debugProg, "color", vec3f([1,1,0]));
            drawPoints(s);

            debugLine(&dbg, vec3f([0, 0, 0]), vec3f([1, 1, 1]));
            debugArrow(&dbg, vec3f([0, 0, 0]), vec3f([sin(glfwGetTime()) * 5, 1, 1]));
            drawDebug(&dbg);
            w.swapBuffers();
        }
    }
}
