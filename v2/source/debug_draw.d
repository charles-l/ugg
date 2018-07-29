import opengl.gl4;
import gfm.math : vec3f;
import mesh;

alias DebugPoint = vec3f;
alias Color = vec3f;

enum MAX_DEBUG_LINES = 256;

struct DebugContext {
    vec3f[MAX_DEBUG_LINES * 2] lineVertices;
    int nlines = 0;
    Mesh lines;
    uint linesVBO;
}

DebugContext initDebug() {
    DebugContext c;

    c.lines.vao = genVAO();
    c.linesVBO = genVBO!(float, 3)(0, MAX_DEBUG_LINES * 2);

    return c;
}

void debugLine(DebugContext *ctx, vec3f start, vec3f end) {
    ctx.lineVertices[ctx.nlines * 2] = start;
    ctx.lineVertices[ctx.nlines * 2 + 1] = end;
    ctx.nlines++;
    ctx.lines.verts += 2;
}

void debugArrow(DebugContext *ctx, vec3f start, vec3f end) {
    debugLine(ctx, start, end);
    vec3f e = (end - start).normalized() * 0.2;
    e.z += 0.2;
    debugLine(ctx, end - e, end);
    e.z -= 0.4;
    debugLine(ctx, end - e, end);
}

void drawDebug(DebugContext *ctx) {
    glBindBuffer(GL_ARRAY_BUFFER, ctx.linesVBO);
    glBufferSubData(GL_ARRAY_BUFFER, 0, (2 * ctx.nlines) * 3 * float.sizeof, cast(void *) ctx.lineVertices.ptr);
    drawLines(ctx.lines);
    ctx.nlines = 0;
}
