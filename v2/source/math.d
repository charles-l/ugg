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

float distanceFromCenter(Plane p) {
    return p.normal.dot(p.pos);
}

// based on http://www.gamasutra.com/view/feature/131424/pool_hall_lessons_fast_accurate_.php?page=2
bool willCollide(PhysicsSphere a, PhysicsSphere b, vec3f v) {
    // is a even close enough?
    if(sqrdist(a, b) > pow(v.magnitude, 2) + pow(a.radius, 2) + pow(b.radius, 2))
        return false;
    // is a even going towards b?
    if(v.dot(b.pos - a.pos) <= 0)
        return false;
    vec3f c = b.pos - a.pos;
    float d = c.dot(v);
    float f = pow(c.magnitude, 2) - pow(d, 2);
    // are we even close enough? (when a is moved along v)
    if(f > pow(a.radius + b.radius, 2))
        return false;

    float t = pow(a.radius + b.radius, 2) - f;
    if(t < 0)
        return false;
    float distance = d - sqrt(t);
    if(v.magnitude < distance)
        return false;
    return true;
}

unittest {
    {
        Ray r = Ray(vec3f(0), vec3f(1, 0, 0));
        assert(r.pointOnRay(0.5) == vec3f(0.5, 0, 0));
    }
}
