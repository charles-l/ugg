import gfm.math;
import std.typecons;
import std.math;
import std.conv;

struct Ray {
    vec3f pos;
    vec3f dir;
};

struct Plane {
    vec3f pos;
    vec3f normal;
};

struct PhysicsSphere {
    vec3f pos;
    float radius;
};

struct RigidBody {
    float mass;
    float drag;
    vec3f velocity;

    PhysicsSphere shape;

    //TODO: angular velocity;
    // float angular_drag;
}

// TODO: cylinder collision

template hasPosition(T) {
    enum bool hasPosition() {
        return __traits(hasMember, T, "pos") && is(typeof(T.pos) == vec3f);
    }
}

unittest {
    assert(hasPosition!(Plane));
    assert(hasPosition!(PhysicsSphere));
    assert(hasPosition!(Ray));

    struct X { float pos; }
    assert(!hasPosition!X);
}

float sqrdist(T1, T2)(T1 a, T2 b) if (hasPosition!T1 && hasPosition!T2) {
    vec3f r = (a.pos - b.pos) * (a.pos - b.pos);
    return r.x + r.y + r.z;
}

float dist(T1, T2)(T1 a, T2 b) if (hasPosition!T1 && hasPosition!T2) {
    return sqrt(sqrdist(a, b));
}

unittest {
    float d = dist(
            Plane(vec3f(0), vec3f(1)),
            Ray(vec3f(2), vec3f(1))
    );
    assert(d.approxEqual(sqrt(4. + 4. + 4.)), to!string(d));
}

vec3f pointOnRay(Ray r, float t) {
    return r.pos + t * r.dir;
}

unittest {
    {
        Ray r = Ray(vec3f(0), vec3f(1, 0, 0));
        assert(r.pointOnRay(0.5) == vec3f(0.5, 0, 0));
    }
}

float distanceFromCenter(Plane p) {
    return p.normal.dot(p.pos);
}

// based on http://www.gamasutra.com/view/feature/131424/pool_hall_lessons_fast_accurate_.php?page=2
vec3f moveWithCollision(PhysicsSphere a, PhysicsSphere b, vec3f v) {
    vec3f goal = a.pos + v;
    // is a even close enough?
    if(sqrdist(a, b) > pow(v.magnitude, 2) + pow(a.radius, 2) + pow(b.radius, 2))
        return goal;
    // is a even going towards b?
    /*if(v.dot(b.pos - a.pos) <= 0)
        return false;*/
    vec3f c = b.pos - a.pos;
    float d = c.dot(v);
    float f = pow(c.magnitude, 2) - pow(d, 2);
    // are we even close enough? (when a is moved along v)
    if(f > pow(a.radius + b.radius, 2))
        return goal;

    float t = pow(a.radius + b.radius, 2) - f;
    if(t < 0)
        return goal;
    float distance = d - sqrt(t);
    if(v.magnitude < distance)
        return goal;
    import std.stdio;
    debug stderr.writeln("HAHH");
    return Ray(a.pos, v).pointOnRay(distance);
}

version(unittest) {
    // visual debug
    void assertPhysicsResult(PhysicsSphere a, vec3f v, PhysicsSphere b, vec3f expectedPos, vec3f actualPos) {
        if(expectedPos == actualPos) {
            return; // success
        }
        // we failed - display graphic demonstrating issue
        import glfw3d;
        import mesh;
        import debug_draw;

        vec3f up = vec3f(0, 1, 0);
        Window w = initGL();
        DebugContext dbg = initDebug();
        auto projection = mat4f.perspective(PI / 4.0, 640.0 / 480.0, 0.1, 100.0);
        executeGraphicsLoop(&w, () {
            vec3f pos = vec3f(cos(glfwGetTime()) * 16, 3, sin(glfwGetTime()) * 16);
            mat4f view = mat4f.lookAt(pos, a.pos, up);
            mat4f mvp = projection * view * mat4f.identity;
            {
                //debugSphere(&dbg, a.pos, a.radius, Color(0.5, 0.5, 0.5));
                debugSphere(&dbg, b.pos, b.radius, Color(0.0, 0.0, 0.5));
                debugSphere(&dbg, expectedPos, a.radius, Color(0, 1, 0));
                debugSphere(&dbg, actualPos, a.radius, Color(1, 0, 0));
                debugArrow(&dbg, a.pos, a.pos + v);
            }
            drawDebug(&dbg, mvp);
        }, (GLFWwindow *w) {});

        assert(0); // mark as fail
    }
}

unittest {
    {
        PhysicsSphere a = PhysicsSphere(vec3f(0, 0, 0), 1);
        PhysicsSphere b = PhysicsSphere(vec3f(3, 0, 0), 1);
        vec3f v = vec3f(2, 0, 0);
        vec3f e = vec3f(1, 0, 0);
        vec3f r = a.moveWithCollision(b, v);
        assertPhysicsResult(a, v, b, e, r);
    }
}
