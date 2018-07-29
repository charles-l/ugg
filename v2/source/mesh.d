import opengl.gl4;
import gfm.math;
import std.algorithm.iteration : map, reduce;
import std.string;
import std.stdio;

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

Mesh makePlane(float r) {
    float[] verts = [
        -r, 0, r,
        -r, 0, -r,
        r, 0, -r,
        r, 0, r
    ];
    uint[] faces = [
        2, 1, 0,
        3, 2, 0
    ];
    float[] uvs = [
        0, 1,
        0, 0,
        1, 0,
        1, 1
    ];
    float[] norms = [
        0, -1, 0,
        0, -1, 0,
        0, -1, 0,
        0, -1, 0
    ];
    Mesh m;
    m.vao = genVAO();
    m.verts = verts.length / 3;

    genElementVBO(faces);
    genVBO!(float, 3)(0, verts);
    genVBO!(float, 3)(1, norms);
    genVBO!(float, 2)(2, uvs);
    glBindVertexArray(0);
    return m;
}

Mesh loadMesh(string path) {
    import sdlang;
    import std.range;

    Tag root = parseFile(path);

    float[] verts;
    float[] normals;
    float[] uvs;
    uint[] faces;

    // XXX only parsing first mesh for now
    foreach(mesh; root.tags["mesh"].take(1))
    {
        foreach(vert; mesh.getTag("vertices").tags) {
            verts ~= vert.values.map!(x => cast(float) x.get!double).array;
        }

        foreach(face; mesh.getTag("faces").tags) {
            faces ~= face.values.map!(x => cast(uint) x.get!int).array;
        }

        foreach(norm; mesh.getTag("normals").tags) {
            normals ~= norm.values.map!(x => cast(float) -x.get!double).array;
        }

        foreach(uv; mesh.getTag("uvs").tags) {
            uvs ~= uv.values.map!(x => cast(float) x.get!double).array;
        }
    }

    Mesh m;
    m.vao = genVAO();
    m.verts = verts.length / 3;
    genElementVBO(faces);
    genVBO!(float, 3)(0, verts);
    genVBO!(float, 3)(1, normals);
    genVBO!(float, 2)(2, uvs);
    glBindVertexArray(0);

    return m;
}

uint genElementVBO(uint[] buf) {
    uint id;
    glGenBuffers(1, &id);

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, id);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, uint.sizeof * buf.length, buf.ptr, GL_STATIC_DRAW);

    return id;
}

// T - type of individual compoennt
// N - number of components (i.e. vertex has 3, (x, y, z))
template genVBO(T, uint N) {
    // allocate array manually
    uint genVBO(uint i, uint n, T *data = null, int draw_type = GL_DYNAMIC_DRAW) {
        uint id;
        glGenBuffers(1, &id);
        glBindBuffer(GL_ARRAY_BUFFER, id);

        static if (!is(T == float))
            assert(false, "unsupported vbo type " ~ T.stringof);

        glBufferData(GL_ARRAY_BUFFER, n * T.sizeof * N, data, draw_type);

        glVertexAttribPointer(i, N, GL_FLOAT, GL_FALSE, 0, null);
        glEnableVertexAttribArray(i);

        return id;
    }

    // allocate static array, when we know the mesh data AOT
    uint genVBO(uint i, T[] buf) {
        return genVBO!(T, N)(i, cast(uint) buf.length, buf.ptr, GL_STATIC_DRAW);
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

struct Program {
    uint id;
}

void use(Program p) {
    glUseProgram(p.id);
}

uint _compileShader(string source, uint type) {
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

Program makeProgram(uint vid, uint fid) {
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

    Program p = {linkProgram(vid, fid)};
    return p;
}

Program makeProgram(string vsource, string fsource) {
    return makeProgram(_compileShader(vsource, GL_VERTEX_SHADER), _compileShader(fsource, GL_FRAGMENT_SHADER));
}

