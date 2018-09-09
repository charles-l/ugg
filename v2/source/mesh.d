import opengl.gl4;
import gfm.math;
import std.algorithm.iteration : map, reduce;
import std.string;
import std.stdio;
import std.math;
import std.typecons;

struct VaoID {
    uint id;
    alias id this;
}

struct Mesh {
    VaoID vao;
    Nullable!size_t nelems;
    size_t nverts;
    // TODO: maybe track vbo ids?
}

VaoID genVAO() {
    uint id;
    glGenVertexArrays(1, &id);
    glBindVertexArray(id);
    return VaoID(id);
}

Mesh makeSphere(float r, uint np = 8, uint nm = 8) {
    Mesh mesh;
    float[] verts;
    float[] normals;
    uint[] faces;
    void addTriangle(uint a, uint b, uint c) { faces ~= [a, c, b]; }
    void addQuad(uint a, uint b, uint c, uint d) { faces ~= [a, b, c, a, c, d]; }

    { // gen verts (with normals)
        verts ~= [0, r, 0];
        normals ~= [0, 1, 0];
        for(int j = 0; j < np - 1; j++) {
            float p = PI * (j+1) / float(np);
            float sp = sin(p);
            float cp = cos(p);
            for(int i = 0; i < nm; i++) {
                float m = 2.0 * PI * i / float(nm);
                float sm = sin(m);
                float cm = cos(m);
                float[3] v = [sp * cm,
                              cp,
                              sp * sm];
                verts ~= [v[0] * r, v[1] * r, v[2] * r];
                normals ~= v;
            }
        }
        verts ~= [0, -r, 0];
        normals ~= [0, -1, 0];
    }

    { // connect faces
        uint lastverti = cast(uint) (verts.length/3) - 1;
        for(int i = 0; i < nm; i++) {
            // add to top cap
            addTriangle(0, i + 1, (i + 1) % nm + 1);
            // add to bottom cap
            addTriangle(lastverti,
                        nm * (np-2) + ((i+1) % nm) + 1,
                        nm * (np-2) + i+1,
            );
        }
        for(int j = 0; j < np - 2; j++) {
            uint aLoopStart = j * nm + 1;
            uint bLoopStart = (j + 1) * nm + 1;
            for(int i = 0; i < nm; i++) {
                addQuad(
                        aLoopStart + i,
                        aLoopStart + (i+1) % nm,
                        bLoopStart + (i+1) % nm,
                        bLoopStart + i
                );
            }
        }
    }

    auto uvs = new float[(verts.length / 3) * 2];

    mesh.vao = genVAO();
    mesh.nverts = verts.length / 3;
    mesh.nelems = faces.length / 3;

    genElementVBO(faces);
    debug writeln(faces.length / 3);
    debug writeln(verts.length / 3);
    genVBO!(float, 3)(0, verts);
    genVBO!(float, 3)(1, normals);
    genVBO!(float, 2)(2, uvs);
    glBindVertexArray(0);
    return mesh;
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
    m.nverts = verts.length / 3;
    m.nelems = faces.length / 3;

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
    m.nverts = verts.length / 3;
    m.nelems = faces.length / 3;
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
    glDrawArrays(GL_POINTS, 0, cast(uint) m.nverts);
    glBindVertexArray(0);
}

template drawLines(bool connected = false) {
    void drawLines(Mesh m) {
        glBindVertexArray(m.vao);
        static if(connected) {
            glDrawArrays(GL_LINE_STRIP, 0, cast(uint) m.nverts);
        } else {
            glDrawArrays(GL_LINES, 0, cast(uint) m.nverts);
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

