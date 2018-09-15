import opengl.gl4;
import gfm.math : vec3f, mat4f;
import std.algorithm.iteration: map;
import std.range: zip, drop;
import std.file : read;
import mesh;

alias DebugPoint = vec3f;
alias Color = vec3f;

enum MAX_DEBUG_LINES = 256;

struct DebugContext {
    vec3f[MAX_DEBUG_LINES * 2] lineVertices;
    vec3f[MAX_DEBUG_LINES * 2] lineColors;
    int nlines = 0;
    Mesh lines;
    uint linesVBO;
    uint colorsVBO;
    Program debugProgram;
}

DebugContext initDebug() {
    DebugContext c;

    c.lines.vao = genVAO();
    c.linesVBO = genVBO!(float, 3)(0, MAX_DEBUG_LINES * 2);
    c.colorsVBO = genVBO!(float, 3)(1, MAX_DEBUG_LINES * 2);

    // currently duplicated -- TODO use hashtable to prevent duplicate program generation
    auto vertexShader = _compileShader(cast(string) read("./vert.glsl", 512), GL_VERTEX_SHADER);
    auto monoShader = _compileShader(cast(string) read("./mono.glsl", 512), GL_FRAGMENT_SHADER);

    c.debugProgram = makeProgram(vertexShader, monoShader);

    return c;
}

void debugLine(DebugContext *ctx, vec3f start, vec3f end, Color color) {
    ctx.lineVertices[ctx.nlines * 2] = start;
    ctx.lineVertices[ctx.nlines * 2 + 1] = end;
    ctx.lineColors[ctx.nlines * 2] = color;
    ctx.lineColors[ctx.nlines * 2 + 1] = color;
    ctx.nlines++;
    ctx.lines.nverts = ctx.nlines * 2;
}

void debugArrow(DebugContext *ctx, vec3f start, vec3f end, Color color = vec3f(1)) {
    debugLine(ctx, start, end, color);
    vec3f e = (end - start).normalized() * 0.2;
    e.z += 0.2;
    debugLine(ctx, end - e, end, color);
    e.z -= 0.4;
    debugLine(ctx, end - e, end, color);
}

void debugSphere(DebugContext *ctx, vec3f pos, float radius, Color color = vec3f(1)) {
    auto verts = makeUnitSphereVerts().map!(x => (x * radius) + pos);
    foreach(a, b; zip(verts, verts.drop(1))) {
        debugLine(ctx, a, b, color);
    }
}

void drawDebug(DebugContext *ctx, mat4f mvp) {
    use(ctx.debugProgram);
    setUniform(ctx.debugProgram, "mvp", mvp);

    glBindBuffer(GL_ARRAY_BUFFER, ctx.linesVBO);
    glBufferSubData(GL_ARRAY_BUFFER, 0, (2 * ctx.nlines) * 3 * float.sizeof, cast(void *) ctx.lineVertices.ptr);
    glBindBuffer(GL_ARRAY_BUFFER, ctx.colorsVBO);
    glBufferSubData(GL_ARRAY_BUFFER, 0, (2 * ctx.nlines) * 3 * float.sizeof, cast(void *) ctx.lineColors.ptr);
    drawLines(ctx.lines);
    drawPoints(ctx.lines);

    // reset
    ctx.nlines = 0;
}
