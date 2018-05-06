#!BPY

import bpy

# generate an assoc array using relevant data in the object
def obj_assoc_array(ob):
    m = ob.data
    
    # keys have structure (x, y, z, u, v)
    # value is index (for order)
    vs = {}
    fs = []
    
    # grabbed from obj exporter
    import bmesh
    bm = bmesh.new()
    bm.from_mesh(m)
    bmesh.ops.triangulate(bm, faces=bm.faces)
    bm.to_mesh(m)
    
    uv_layer = bm.loops.layers.uv.verify()
    bm.faces.layers.tex.verify()
    for f in bm.faces:
        vis = []
        for l in f.loops:
            vu = tuple(l.vert.co) + tuple(l[uv_layer].uv)
            if vu not in vs:
                vs[vu] = len(vs)
            vis.append(vs[vu])
        fs.append(vis)
            
    bm.free()
    vus = [vu[0] for vu in sorted(vs.items(), key=lambda x: x[1])]
    verts = [(v[0], v[1], v[2]) for v in vus]
    uvs = [(v[3], v[4]) for v in vus]
    return [ob.name, ['vertices', verts], ['faces', fs], ['uvs', uvs]]

# print an array as an sexpression (recursively)
def print_array_as_sexp(a, o):
    o.write('(')
    for i, e in enumerate(a):
        if isinstance(e, (list, tuple)):
            print_array_as_sexp(e, o)
        else:
            o.write(str(e))
        if i != len(a) - 1:
            o.write(' ')
    o.write(')')

def write(filename):
    with open(filename, 'w') as out:
        s = bpy.context.scene
        for ob in s.objects:
            if ob.type == 'MESH':
                print_array_as_sexp(obj_assoc_array(ob), out)
    return {'FINISHED'}

###

from bpy_extras.io_utils import ExportHelper

bl_info = {
    "name": "Export Sexpression",
    "author": "ncharlie",
    "version": (0, 1),
    "blender": (2, 79, 0),
    "location": "File > Export > Export Sexpression",
    "warning": "",
    "description": "Export an s-expression of the scene",
    "category": "Import-Export"
}

class ExportSexpression(bpy.types.Operator, ExportHelper):
    bl_idname = "export_scene.sexp"
    bl_label = "Export Sexpression"
    bl_options = {"PRESET"}
    filename_ext = ".sexp"
    def execute(self, context):
        if not self.filepath:
            raise Exception("filepath not set")
        return write(self.filepath)
    
def menu_func(self, context):
    self.layout.operator(ExportSexpression.bl_idname, text="Export Sexpression (.sexp)")
        
def register():
    bpy.utils.register_module(__name__)
    bpy.types.INFO_MT_file_export.append(menu_func)

def unregister():
    bpy.utils.unregister_module(__name__)
    bpy.types.INFO_MT_file_export.remove(menu_func)

if __name__ == "__main__":
    register()