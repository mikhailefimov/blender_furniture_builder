###############################################################################
# This is a library for programmatically designing cabinet furniture
# in Blender 2.80 (or later). Due to breaking API changes in Blender 2.80,
# this script won't work in older versions.
#
# Copy-paste it to blender file as text module, see 'example.blend'.
#
# Author: Mikhail Efimov (mikhail.m.efimov@gmail.com)
#
# This is unusable without Blender, so it probably falls under
# GNU GPL Version 2 or later as Blender itself (if anyone cares).
# See https://www.blender.org/about/license/ for licensing details.
###############################################################################

import functools
from abc import ABCMeta, abstractmethod
import bpy
from bpy import context as ctx
from math import radians
import csv
from collections import OrderedDict, defaultdict
from contextlib import contextmanager


###############################################################################
# Blender object wrappers
###############################################################################
class ObjectWrapper:
    """
    Wrapper for blender object with some useful functions.
    """

    def __init__(self, obj, parent, layer):
        """
        :param obj: blender object (mesh, etc)
        :param parent: parent wrapper
        :param layer: blender layer_collection
        """
        self.object = obj
        self.name = obj.name
        self.parent = parent
        self.layer = layer

    def add_child(self, obj):
        """
        Add obj as child to self.object.

        :param obj: blender object (mesh, etc)
        """
        obj.parent = self.object

    def group(self, name, offset=None, collection=None):
        """
        Create new group root (empty mesh, may be used as a common parent for many objects).

        :param name: - group name
        :param offset: - (x,y,z) tuple, location relative to the parent (defaults to (0,0,0))
        :param collection: - name of blender collection (if None - add to the same collection as self)
        :return: new ObjectWrapper
        """
        _, layer = self.collection_by_name(collection)
        ctx.view_layer.active_layer_collection = layer
        bpy.ops.object.empty_add(location=offset or (0, 0, 0))
        obj = ctx.active_object
        obj.name = name
        self.add_child(obj)
        result = ObjectWrapper(obj, self, layer)
        return result

    def activate(self):
        ctx.view_layer.objects.active = self.object

    def rotate_euler(self, rx=0, ry=0, rz=0):
        self.object.rotation_euler = [r + radians(dr) for r, dr in zip(self.object.rotation_euler, (rx, ry, rz))]

    def move(self, dx=0, dy=0, dz=0):
        self.object.location = [a + da for a, da in zip(self.object.location, (dx, dy, dz))]

    def global_location(self):
        if self.parent is None:
            return self.object.location
        return [a + da for a, da in zip(self.object.location, self.parent.global_location())]

    @contextmanager
    def make_children(self, name=None, material=None, collection=None, link_to_first=False):
        """
        All blender objects, created in the context of this manager will be made children of self.object.

        :param name: base name of new objects
        :param material: material name for new objects (if None - keep as is)
        :param collection: blender collection for new objects (if None - the same collection with self.object)
        :param link_to_first: if True - only first created object will be linked to self.object,
                              others will be linked to first
        :return: yields a list of created objects wrapped in ObjectWrapper
        """
        temp_collection, layer = self.collection_by_name("%s_collection_temp" % self.name)
        ctx.view_layer.active_layer_collection = layer
        new_objects = []
        try:
            yield new_objects
        finally:
            material = self.material_by_name(material)
            collection, layer = self.collection_by_name(collection)
            parent = None
            for obj in temp_collection.objects:
                if obj.mode != 'OBJECT':
                    ctx.view_layer.objects.active = obj
                    bpy.ops.object.mode_set(mode='OBJECT')
                if material is not None:
                    obj.data.materials.append(material)
                obj.name = name or self.name
                collection.objects.link(obj)
                (parent or self).add_child(obj)
                item = ObjectWrapper(obj, self, layer)
                new_objects.append(item)
                if link_to_first and parent is None:
                    parent = item
            bpy.data.collections.remove(temp_collection)

    def collection_by_name(self, name):
        """
        Find or create blender collection by name. If name is None - return wrapped object's collection and layer.

        :param name: name of collection
        :return: tuple (collection, layer)
        """
        if name is None:
            return self.layer.collection, self.layer
        collection = bpy.data.collections.get(name)
        if collection is None:
            collection = bpy.data.collections.new(name=name)
        if name not in ctx.scene.collection.children.keys():
            ctx.scene.collection.children.link(collection)
        layer = ObjectWrapper.find_layer(collection)
        layer.hide_viewport = False
        return collection, layer

    @staticmethod
    def find_layer(collection, parent=None):
        """
        Find blender layer_collection, linked to the given collection.

        :param collection: blender collection
        :param parent: parent layer_collection to search (if None - look in ctx.view_layer.layer_collection)
        :return: found layer_collection or None if nothing found
        """
        parent = parent or ctx.view_layer.layer_collection
        if parent.collection == collection:
            return parent
        for p in parent.children:
            result = ObjectWrapper.find_layer(collection, parent=p)
            if result is not None:
                return result
        return None

    @staticmethod
    def material_by_name(name):
        """
        Find material by name, or create new if not found.

        :param name: name of material
        :return: blender material
        """
        if name is None:
            return None
        material = bpy.data.materials.get(name)
        if material is None:
            material = bpy.data.materials.new(name=name)
        return material


class Root(ObjectWrapper):
    """
    Root of ObjectWrappers hierarchy.
    """

    def __init__(self, name, location=None, collection='Builder_root'):
        """
        Create new root.
        If blender object with the same name exists - it will be deleted with all its children.

        :param name: name of this root
        :param location: (x,y,z) tuple, location of this root
        :param collection: name of blender collection
        """
        if bpy.data.objects.find(name) >= 0:
            Root.__delete_hierarchy(name)
        _, layer = self.collection_by_name(collection)  # it's OK here, collection is not None
        ctx.view_layer.active_layer_collection = layer
        bpy.ops.object.empty_add(location=location or [0, 0, 0])
        ctx.active_object.name = name
        super().__init__(ctx.active_object, None, layer)

    @staticmethod
    def __add_recursive(lst, obj):
        lst.append(obj)
        for child in obj.children:
            Root.__add_recursive(lst, child)
        return lst

    @staticmethod
    def __delete_hierarchy(root_name):
        for obj in bpy.context.scene.objects:
            if obj.mode != 'OBJECT':
                ctx.view_layer.objects.active = obj
                bpy.ops.object.mode_set(mode='OBJECT')

        objects = Root.__add_recursive([], bpy.data.objects[root_name])
        for obj in objects:
            obj.animation_data_clear()
            for collection in obj.users_collection:
                layer = ObjectWrapper.find_layer(collection)
                if layer is not None:
                    layer.hide_viewport = False

        override_ctx = ctx.copy()
        override_ctx['selected_objects'] = objects
        bpy.ops.object.delete(override_ctx)


###############################################################################
# Box builder
###############################################################################
class Box:
    """
    Mutable rectangular box with negative y axis (from far end to near).
    Supports slicing/cutting operations.
    """

    def __init__(self, x=0.0, y=0.0, z=0.0, w=0.0, d=0.0, h=0.0):
        self.x = x
        self.y = y
        self.z = z
        self.w = w
        self.d = d
        self.h = h

    def copy(self):
        return Box(self.x, self.y, self.z, self.w, self.d, self.h)

    def slice_z(self, z, size):
        """
        Create new Box as a slice of this box by plane  XY. This box is not changed.

        :param z: position, where to place new box
        :param size: size of new box
        :return: new Box
        """
        return Box(self.x, self.y, z, self.w, self.d, size)

    def slice_x(self, x, size):
        """
        Create new Box as a slice of this box by plane YZ. This box is not changed.

        :param x: position, where to place new box
        :param size: size of new box
        :return: new Box
        """
        return Box(x, self.y, self.z, size, self.d, self.h)

    def slice_y(self, y, size):
        """
        Create new Box as a slice of this box by plane XZ. This box is not changed.

        :param y: position, where to place new box
        :param size: size of new box
        :return: new Box
        """
        return Box(self.x, y, self.z, self.w, size, self.h)

    def cut_top(self, size):
        """
        Cut this box by plane XY. Keep bottom part as this box, return top part.

        :param size: size of separated part
        :return: top part
        """
        self.h -= size
        return self.slice_z(self.z + self.h, size)

    def cut_top_at(self, z):
        """
        Cut this box by plane XY. Keep bottom part as this box, return top part.

        :param z: cutting plane location
        :return: top part
        """
        return self.cut_top(self.h + self.z - z)

    def cut_bottom(self, size):
        """
        Cut this box by plane XY. Keep top part as this box, return bottom part.

        :param size: size of separated part
        :return: bottom part
        """
        self.h -= size
        self.z += size
        return self.slice_z(self.z - size, size)

    def cut_bottom_at(self, z):
        """
        Cut this box by plane XY. Keep top part as this box, return bottom part.

        :param z: cutting plane location
        :return: bottom part
        """
        return self.cut_bottom(z - self.z)

    def cut_right(self, size):
        """
        Cut this box by plane YZ. Keep left part as this box, return right part.

        :param size: size of separated part
        :return: right part
        """
        self.w -= size
        return self.slice_x(self.x + self.w, size)

    def cut_right_at(self, x):
        """
        Cut this box by plane YZ. Keep left part as this box, return right part.

        :param x: cutting plane location
        :return: right part
        """
        return self.cut_right(self.w + self.x - x)

    def cut_left(self, size):
        """
        Cut this box by plane YZ. Keep right part as this box, return left part.

        :param size: size of separated part
        :return: left part
        """
        self.w -= size
        self.x += size
        return self.slice_x(self.x - size, size)

    def cut_left_at(self, x):
        """
        Cut this box by plane YZ. Keep right part as this box, return left part.

        :param x: cutting plane location
        :return: left part
        """
        return self.cut_left(x - self.x)

    def cut_front(self, size):
        """
        Cut this box by plane XZ. Keep back (far) part as this box, return front (near) part.

        :param size: size of separated part
        :return: front (near) part
        """
        self.d -= size
        return self.slice_y(self.y - self.d, size)

    def cut_front_at(self, y):
        """
        Cut this box by plane XZ. Keep back (far) part as this box, return front (near) part.

        :param y: cutting plane location
        :return: front (near) part
        """
        return self.cut_front(y - (self.y - self.d))

    def cut_back(self, size):
        """
        Cut this box by plane XZ. Keep front (near) part as this box, return back (far) part.

        :param size: size of separated part
        :return: back (far) part
        """
        self.d -= size
        self.y -= size
        return self.slice_y(self.y + size, size)

    def cut_back_at(self, y):
        """
        Cut this box by plane XZ. Keep front (near) part as this box, return back (far) part.

        :param y: cutting plane location
        :return: back (far) part
        """
        return self.cut_back(self.y - y)

    def shrink(self, left=0, right=0, near=0, far=0, top=0, bottom=0):
        self.x += left
        self.w -= left + right
        self.y -= far
        self.d -= near + far
        self.z += bottom
        self.h -= bottom + top

    def shift(self, dx=0, dy=0, dz=0):
        self.x += dx
        self.y += dy
        self.z += dz


###############################################################################
# Furniture components
###############################################################################
class Sheet:
    """
    Represents sheet material like wood, MDF, particleboard, etc.
    """

    bom = []  # Bill of materials (list of parts descriptions)

    def __init__(self, thickness, material=None, thick_edge=0.0, thin_edge=0.0, name=None):
        """
        :param thickness: sheet thickness
        :param material: blender material name (for render)
        :param thick_edge: edge banding thickness (thick decorative material - ABS etc)
        :param thin_edge: edge banding thickness (thin protective material - melamine etc)
        :param name: BOM name of this material
        """
        self.name = name or "%s%.1f" % (material or "", thickness * 1000)
        self.material = material or self.name
        self.thickness = thickness
        self.thick_edge = thick_edge
        self.thin_edge = thin_edge

    @staticmethod
    def process_bom(func):
        Sheet.bom = [func(part) for part in Sheet.bom]

    @staticmethod
    def save_bom(filename):
        """
        Save BOM to file in CSV format.
        """
        fieldnames = OrderedDict()
        for part in Sheet.bom:
            for key, value in part.items():
                fieldnames[key] = '%s' if type(value) is str else '%i' if type(value) is int else '%.3f'
        with open(filename, 'w', newline='') as f:
            writer = csv.DictWriter(f, fieldnames=list(fieldnames.keys()))
            writer.writeheader()
            for part in Sheet.bom:
                writer.writerow({k: fieldnames[k] % v for k, v in part.items()})

    def make(self, parent, name=None,
             w=1.0, h=1.0, dx=0.0, dy=0.0, dz=0.0, rx=0.0, ry=0.0, rz=0.0,
             edges='LRTB', no_edges=''):
        """
        Create a part from this material, place it at the given location, rotate, and attach to the parent.

        :param parent: parent ObjectWrapper
        :param name: name of a part (defaults to self.name)
        :param w: part "width"
        :param h: part "height"
        :param dx: X location
        :param dy: Y location
        :param dz: Z location
        :param rx: X rotation
        :param ry: Y rotation
        :param rz: Z rotation
        :param edges: string of letters 'LRTB'. Use thick edge on those sides, and thin on others.
        :param no_edges: string of letters 'LRTB'.  Don't use any edge banding on those sides.
        :return: new part in ObjectWrapper
        """
        with parent.make_children(name=name or self.name, material=self.material) as created:
            bpy.ops.mesh.primitive_cube_add(location=(dx, dy, dz), rotation=[radians(r) for r in (rx, ry, rz)])
            obj = ctx.active_object
            for idx, v in enumerate(obj.data.vertices):
                v.co = [0 if (idx & mask) == 0 else round(c, 3) for c, mask in ((w, 4), (self.thickness, 2), (h, 1))]

        module = parent
        while module.parent is not None and module.parent.parent is not None:
            parent = module
            module = module.parent

        bom_data = dict(module=module.name, submodule=parent.name, name=name, material=self.name,
                        w=w * 1000, h=h * 1000)

        e_thick, e_thin = 0, 0
        edge_t = []
        for side, length in (('T', w), ('B', w), ('L', h), ('R', h)):
            t = 0
            if side in edges:
                e_thick += length
                t = self.thick_edge
            elif side not in no_edges:
                e_thin += length
                t = self.thin_edge
            edge_t.append(t)
            bom_data['e' + side] = t
        if self.thick_edge:
            bom_data["f%.1f" % self.thick_edge] = e_thick
        if self.thin_edge:
            bom_data["f%.1f" % self.thin_edge] = e_thin
        hh = h * 1000 - edge_t[0] - edge_t[1]
        ww = w * 1000 - edge_t[2] - edge_t[3]
        bom_data.update(ww=ww, hh=hh, S=w * h, P=2 * (w + h))
        Sheet.bom.append(bom_data)
        return created[-1]

    def make_box(self, parent, name, box, side, edges=None, no_edges=''):
        """
        Create a part from this material, with coordinates given by the projection of the box.
        Sides for 'edges' and 'no_edges'  parameters are given as the box sides, Left, Right, Top, Bottom, Far, Near.

        :param parent: parent ObjectWrapper
        :param name: name of a part (defaults to self.name)
        :param box: Box to take coordinates from
        :param side: projection: 'T' - top, 'F' - front, 'L' - left
        :param edges: string of letters 'LRTBFN'. Use thick edge on those sides, and thin on others.
        :param no_edges: string of letters 'LRTBFN'.  Don't use any edge banding on those sides.
        :return: new part in ObjectWrapper
        """

        if side == 'F':
            return self.make(parent, name=name, w=box.w, h=box.h, dx=box.x, dy=box.y - box.d, dz=box.z,
                             edges='LRTB' if edges is None else edges, no_edges=no_edges)
        elif side == 'T':
            mapping = dict(N='L', F='R', R='T', L='B')
            return self.make(parent, name=name, w=box.d, h=box.w, dx=box.x, dy=box.y - box.d, dz=box.z, rx=90, rz=90,
                             edges='LRTB' if edges is None else ''.join(map(mapping.get, edges)),
                             no_edges=''.join(map(mapping.get, no_edges)))
        elif side == 'L':
            mapping = dict(N='R', F='L', T='T', B='B')
            return self.make(parent, name=name, w=box.d, h=box.h, dx=box.x, dy=box.y, dz=box.z, rz=-90,
                             edges='LRTB' if edges is None else ''.join(map(mapping.get, edges)),
                             no_edges=''.join(map(mapping.get, no_edges)))


class Hardware:
    """
    Base class for handles, hinges and other countable parts with complex geometry.
    """

    bom = defaultdict(int)  # Bill of materials (dict part->count)
    render_internal = False  # If False - don't render hidden components like hinges (to speedup render).

    def __init__(self, name, title=None, material=None, internal=False):
        """
        :param name: full name of hardware type in BOM
        :param title: short name for rendered objects
        :param material: blender material name for render
        :param internal: if True - don't render this part when Hardware.render_internal==False
        """
        self.name = name
        self.title = title or name
        self.material = material
        self.internal = internal

    @staticmethod
    def save_bom(filename):
        with open(filename, 'w', newline='') as csvfile:
            bom_writer = csv.writer(csvfile)
            bom_writer.writerow(['Name', 'Count'])
            for key in sorted(Hardware.bom):
                bom_writer.writerow([key, Hardware.bom[key]])

    @staticmethod
    def add(name, count):
        """
        Add any named hardware part to the BOM (maybe not rendered).
        """
        Hardware.bom[name] += count

    def attach_to(self, parent, dx=0.0, dy=0.0, dz=0.0, rx=0.0, ry=0.0, rz=0.0):
        """
        Add one item of this type to the BOM and render it as attached to the parent at the given location.

        :param parent: parent ObjectWrapper
        :param dx: X location
        :param dy: Y location
        :param dz: Z location
        :param rx: X rotation
        :param ry: Y rotation
        :param rz: Z rotation
        :return: Wrapped blender object created in render() method
        """
        Hardware.add(self.name, 1)
        if self.internal and not Hardware.render_internal:
            return None
        with parent.make_children(self.title, self.material, link_to_first=True) as created:
            self.render()
        result = created[0]
        result.activate()
        result.move(dx, dy, dz)
        result.rotate_euler(rx, ry, rz)
        return result

    def render(self):
        pass


class Handle(Hardware):
    """
    Door/drawer handle with a shape like _I__I_.
    """

    def __init__(self, name, length, offset, ext=0, material=None):
        """
        :param name: name for the BOM
        :param length: length of the handle
        :param offset: length of stands (I parts in _I__I_)
        :param ext: length of extensions to the left/right sides of the stands
        :param material: material name for render
        """
        super().__init__("%s(%i)" % (name, int(length * 1000)), name, material)
        self.length = length
        self.offset = offset
        self.ext = ext

    def render(self):
        bpy.ops.mesh.primitive_cylinder_add(
            radius=0.005, depth=self.length, location=(0, - self.offset, 0))
        dz = self.length * 0.5 - self.ext - 0.003
        for ddz in (dz, -dz):
            bpy.ops.mesh.primitive_cylinder_add(
                radius=0.003, depth=self.offset, location=(0, self.offset / 2, ddz), rotation=[radians(90), 0, 0])


class Hinge(Hardware):
    """
    Door hinge.
    """

    @staticmethod
    def perpendicular(name, dx, material=None):
        """
        Overlay type hinge mounted on a perpendicular panel.

        :param name: name for the BOM
        :param dx: side panel overlay
        :param material: material name for render
        :return:
        """
        if dx > 0:
            return Hinge(name + "(overlay %.1f)" % (dx * 1000), name, dx=dx, material=material)
        else:
            return Hinge(name + "(inset %.1f)" % (-dx * 1000), name, dx=dx, material=material)

    @staticmethod
    def coplanar(name, material=None):
        """
        Hinge mounted on coplanar panel (fixed facade in the same plane with a door)

        :param name: name for the BOM
        :param material: material name for render
        :return:
        """
        return Hinge(name + "(facade mount)", name, dx=-0.040, w=0.06, d=0.01, material=material)

    def __init__(self, name, title, dx, w=0.01, d=0.04, material=None):
        super().__init__(name, title, material, internal=True)
        self.dx = dx
        self.w = w
        self.d = d

    def render(self):
        x, y = 0.0175 + 0.004, - 0.0115 / 2
        bpy.ops.mesh.primitive_cylinder_add(
            radius=0.0175, depth=0.012, location=[x, y, 0], rotation=[radians(90), 0, 0])
        bpy.ops.mesh.primitive_cube_add(location=[self.dx - x, y, - 0.005], rotation=[radians(-90), 0, 0])
        result = ctx.active_object
        for idx, v in enumerate(result.data.vertices):
            v.co = [0 if (idx & mask) == 0 else c for c, mask in ((self.w, 4), (self.d, 2), (0.01, 1))]
        result.data.vertices[6].co = [self.w - 0.002, self.d - 0.002, 0]
        result.data.vertices[7].co = [self.w - 0.002, self.d - 0.002, 0.01]


###############################################################################
# Furniture builders
###############################################################################
class AbstractStyle(metaclass=ABCMeta):
    """
    Abstract base class for furniture customization.
    See DefaultStyle as an example.
    """

    @abstractmethod
    def box_material(self):
        """
        Primary construction material - cabinet sides etc.

        :return: Sheet
        """
        pass

    @abstractmethod
    def backdrop_material(self):
        """
        Cabinets backdrop material

        :return: Sheet
        """
        pass

    @abstractmethod
    def facade_material(self):
        """
        Doors and drawers facade material

        :return: Sheet
        """
        pass

    @abstractmethod
    def ext_side_material(self):
        """
        External sides material

        :return:  Sheet
        """
        pass

    @abstractmethod
    def work_surface_material(self):
        """
        Top working surface material (for kitchen etc.)

        :return: Sheet
        """
        pass

    @abstractmethod
    def drawer_material(self):
        """
        Drawer sides construction material

        :return: Sheet
        """
        pass

    @abstractmethod
    def drawer_bottom_material(self, width):
        """
        Drawer bottom material. May depend on drawer width.

        :param width: drawer facade width
        :return: Sheet
        """
        pass

    @abstractmethod
    def door_handle_orientation(self, width, h_bottom, h_top):
        """
        Doors handles orientation - 'V' - vertical, 'H' - horizontal.
        May depend on door width and top/bottom position.

        :param width: door width
        :param h_bottom: z position of door bottom
        :param h_top: z position of door top
        :return: 'V' - vertical, 'H' - horizontal.
        """
        pass

    @abstractmethod
    def handle_type(self, width, orientation):
        """
        Door handle type.
        May depend on door width and handle orientation.

        :param width:  door width
        :param orientation: handle orientation ('V' - vertical, 'H' - horizontal)
        :return: instance of Hardware
        """
        pass

    @abstractmethod
    def door_handle_position(self, width, height, h_bottom, handle, orientation):
        """
        Door handle position.
        May depend on door size, position, handle type and orientation.

        :param width: door width
        :param height: door height
        :param h_bottom: z position of door bottom
        :param handle: handle type (instance of Hardware)
        :param orientation: handle orientation ('V' - vertical, 'H' - horizontal)
        :return: tuple (dx,dz) - position of handle relative to BL corner of the left door
        """
        pass

    @abstractmethod
    def drawer_handle_position(self, width, height, handle):
        """
        Drawer handle position.
        May depend on drawer facade size and handle type

        :param width: drawer facade width
        :param height: drawer facade height
        :param handle: handle type (instance of Hardware)
        :return: tuple (dx,dz) - position of handle relative to BL corner of the drawer facade
        """
        pass

    @abstractmethod
    def door_hinge(self, overlay):
        """
        Door hinge type

        :param overlay: "F" - coplanar mounting, number - side panel overlay
        :return: hinge type (instance of Hardware)
        """
        pass

    @abstractmethod
    def door_hinges_count(self, hinge, width, height, material):
        """
        Number of hinges for given door size and material.
        Depends on hinge type and door weight (or door size and material)

        :param hinge: hinge type (instance of Hardware)
        :param width: door width
        :param height: door height
        :param material: door material (Sheet)
        :return: required number of hinges
        """
        pass

    @abstractmethod
    def gap(self):
        """
        Gap between and around doors
        """
        pass

    @abstractmethod
    def drawer_space_left_right(self):
        """
        Space between side of drawer and cabinet side panel.
        Usually sliders are placed here, so it depends on selected slider
        """
        pass

    @abstractmethod
    def drawer_space_back(self):
        """
        Space between drawer back (far) side and cabinet backdrop
        """
        pass

    @abstractmethod
    def drawer_space_top(self):
        """
        Space between the top of drawer side panels and the closest obstacle on top
        (other drawer facade or top panel of the cabinet)
        """
        pass

    @abstractmethod
    def drawer_space_bottom(self, bottom_material_thickness):
        """
        Space between the bottom of the drawer the closest obstacle beneath
        (other drawer facade or bottom panel of the cabinet)
        May depend on drawer bottom material thickness.
        """
        pass


class DefaultStyle(AbstractStyle):
    """
    Example/default style class for furniture customization.
    """

    handleS = Handle('Handle', 0.096, 0.02, 0, 'Metal')
    handleL = Handle('Handle', 0.128, 0.02, 0, 'Metal')
    hinges = {}

    MDF = Sheet(0.016, 'MDF', 2.0, 0.4)
    HDF = Sheet(0.004, 'HDF')

    def box_material(self):
        """
        Primary construction material - cabinet sides etc.

        :return: Sheet
        """
        return self.MDF

    def backdrop_material(self):
        """
        Cabinets backdrop material

        :return: Sheet
        """
        return self.HDF

    def facade_material(self):
        """
        Doors and drawers facade material

        :return: Sheet
        """
        return self.box_material()

    def ext_side_material(self):
        """
        External sides material

        :return:  Sheet
        """
        return self.box_material()

    def work_surface_material(self):
        """
        Top working surface material (for kitchen etc.)

        :return: Sheet
        """
        return self.box_material()

    def drawer_material(self):
        """
        Drawer sides construction material

        :return: Sheet
        """
        return self.box_material()

    def drawer_bottom_material(self, width):
        """
        Drawer bottom material. May depend on drawer width.

        :param width: drawer facade width
        :return: Sheet
        """
        return self.HDF

    def door_handle_orientation(self, width, h_bottom, h_top):
        """
        Doors handles orientation - 'V' - vertical, 'H' - horizontal.
        May depend on door width and top/bottom position.

        :param width: door width
        :param h_bottom: z position of door bottom
        :param h_top: z position of door top
        :return: 'V' - vertical, 'H' - horizontal.
        """
        return 'V' if h_top > 1.5 else 'H'

    def handle_type(self, width, orientation):
        """
        Door handle type.
        May depend on door width and handle orientation.

        :param width:  door width
        :param orientation: handle orientation ('V' - vertical, 'H' - horizontal)
        :return: instance of Hardware
        """
        return DefaultStyle.handleL if width > 0.3 and orientation == "H" else DefaultStyle.handleS

    def door_handle_position(self, width, height, h_bottom, handle, orientation):
        """
        Door handle position.
        May depend on door size, position, handle type and orientation.

        :param width: door width
        :param height: door height
        :param h_bottom: z position of door bottom
        :param handle: handle type (instance of Hardware)
        :param orientation: handle orientation ('V' - vertical, 'H' - horizontal)
        :return: tuple (dx,dz) - position of handle relative to BL corner of the left door
        """
        dx = width - 0.05 - (handle.length / 2 if (orientation == "H") else 0)
        dz = 0.05 + (handle.length / 2 if (orientation == "V") else 0)
        if h_bottom + height < 1.5:
            dz = height - dz
        return dx, dz

    def drawer_handle_position(self, width, height, handle):
        """
        Drawer handle position.
        May depend on drawer facade size and handle type

        :param width: drawer facade width
        :param height: drawer facade height
        :param handle: handle type (instance of Hardware)
        :return: tuple (dx,dz) - position of handle relative to BL corner of the drawer facade
        """
        return width / 2, height / 2

    def door_hinge(self, overlay):
        """
        Door hinge type

        :param overlay: "F" - coplanar mounting, number - side panel overlay
        :return: hinge type (instance of Hardware)
        """
        if overlay in DefaultStyle.hinges:
            hinge = DefaultStyle.hinges[overlay]
        else:
            if overlay == 'F':
                hinge = Hinge.coplanar('Hinge', material='Metal')
            else:
                hinge = Hinge.perpendicular('Hinge', material='Metal', dx=overlay)
            DefaultStyle.hinges[overlay] = hinge
        return hinge

    def door_hinges_count(self, hinge, width, height, material):
        """
        Number of hinges for given door size and material.
        Depends on hinge type and door weight (or door size and material)

        :param hinge: hinge type (instance of Hardware)
        :param width: door width
        :param height: door height
        :param material: door material (Sheet)
        :return: required number of hinges
        """
        if height <= 0.8:
            return 2
        elif height < 1.5:
            return 3
        elif height < 2:
            return 4
        else:
            return 5

    def gap(self):
        """
        Gap between and around doors
        """
        return 0.002

    def drawer_space_left_right(self):
        """
        Space between side of drawer and cabinet side panel.
        Usually sliders are placed here, so it depends on selected slider
        """
        return 0.013

    def drawer_space_back(self):
        """
        Space between drawer back (far) side and cabinet backdrop
        """
        return 0.015

    def drawer_space_top(self):
        """
        Space between the top of drawer side panels and the closest obstacle on top
        (other drawer facade or top panel of the cabinet)
        """
        return 0.01

    def drawer_space_bottom(self, bottom_material_thickness):
        """
        Space between the bottom of the drawer the closest obstacle beneath
        (other drawer facade or bottom panel of the cabinet)
        May depend on drawer bottom material thickness.
        """
        if bottom_material_thickness < 0.005:
            return 0.005 + 0.002  # reserve extra space for screw caps if it is thin HDF etc
        else:
            return 0.005


def door_(parent, name, width, height, x, y, z, right, overlay, style):
    """
    Create a door with a handle and hinges.

    :param parent: parent ObjectWrapper
    :param name: name of a door for render
    :param width: door width
    :param height: door height
    :param x: door mounting side X location (left or right)
    :param y: door front (near) side Y location
    :param z: door bottom side Z location
    :param right: if True - this is "right door" (hinges are on the right side), otherwise - left
    :param overlay: cabinet side panel overlay, or "F" for coplanar mounting
    :param style: furniture style description (see DefaultStyle for example)
    :return: created door as ObjectWrapper
    """
    result = style.facade_material().make(parent, name, width, height,
                                          dx=x, dy=y - style.facade_material().thickness, dz=z)
    h_bottom = result.global_location()[-1]
    h_top = h_bottom + height
    handle_orientation = style.door_handle_orientation(width, h_bottom, h_top)
    handle = style.handle_type(width, handle_orientation)
    handle_dx, handle_dz = style.door_handle_position(width, height, h_bottom, handle, handle_orientation)
    handle.attach_to(result, dx=handle_dx, dz=handle_dz, ry=90 if (handle_orientation == 'H') else 0)

    hinges = style.door_hinge(overlay)
    hinges_count = style.door_hinges_count(hinges, width, height, style.facade_material())
    dh = (height - 0.2) / (hinges_count - 1)
    for i in range(hinges_count):
        hinges.attach_to(result, dy=style.facade_material().thickness, dz=0.1 + dh * i)
    if right:
        bpy.ops.object.select_all(action='DESELECT')
        result.object.select_set(True)
        bpy.ops.transform.mirror(constraint_axis=(True, False, False))
    return result


def doors(parent, name, width, height, x, y, z, ndoors, overlay_left, overlay_right, style):
    """
    Create one or two (left/right) cabinet doors.

    :param parent: parent ObjectWrapper
    :param name: name of a door for render
    :param width: doors area total width
    :param height: doors height
    :param x: doors left side X location (always left side, even for right doors)
    :param y: doors front (near) side Y location
    :param z: doors bottom side Z location
    :param ndoors: 'L' - left door (hinges on the left side), 'R' - right door, 2 - two doors.
    :param overlay_left: cabinet side panel overlay on the left, or "F" for coplanar mounting
    :param overlay_right: cabinet side panel overlay on the right, or "F" for coplanar mounting
    :param style: furniture style description (see DefaultStyle for example)
    """
    if ndoors == 2:
        door_(parent, name, width / 2 - style.gap() / 2, height, x, y, z, False, overlay_left, style)
        door_(parent, name, width / 2 - style.gap() / 2, height, x + width, y, z, True, overlay_right, style)
    else:
        if ndoors == "R":
            door_(parent, name, width, height, x + width, y, z, True, overlay_right, style)
        else:
            door_(parent, name, width, height, x, y, z, False, overlay_left, style)


class Defaults:
    """
    Helper class.
    Provides decorators to dynamically change method defaults.
    """
    defaults = {}

    @staticmethod
    def use_defaults(defaults_name):
        """
        Links decorated function to the set of default values.
        Later, when the decorated function is invoked with some parameters,
        this decorator will set unspecified parameters to values from the given defaults set.
        """

        def decorator(function):
            @functools.wraps(function)
            def inner(*args, **kwargs):
                a = Defaults.defaults.get(defaults_name, {}).copy()
                a.update(kwargs)
                return function(*args, **a)

            return inner

        return decorator

    @staticmethod
    def set_defaults(defaults_name, **kwargs):
        """
        Set new default values for the given set.

        :param defaults_name: name of defaults set
        :param kwargs: new default parameters values (may be empty to drop all defaults)
        """
        Defaults.defaults[defaults_name] = kwargs

    @staticmethod
    def update_defaults(defaults_name, **kwargs):
        """
        Update the default values set (change only given parameters, keep others as is).

        :param defaults_name: name of defaults set
        :param kwargs: changed parameters values
        """
        Defaults.defaults[defaults_name].update(kwargs)


class Cabinet:
    """
    Cabinet builder.

    Cabinet consists of one or more sections with common frame.
    Each section may contain a set of drawers and compartment with shelves behind 0-1-2 doors.
    More complex cabinets may be created by putting cabinets on top of each other.
    """
    part_names_map = {}

    @staticmethod
    def set_defaults(**kwargs):
        """
        Set default parameters for constructor.
        May be useful when creating many cabinets with partially similar parameters, like style, height, etc.

        :param kwargs: any subset of Cabinet constructor parameters
        """
        Defaults.set_defaults('Cabinet', **kwargs)

    @staticmethod
    def update_defaults(**kwargs):
        """
        Update default parameters for constructor (add/replace new defaults, keep old as is).
        May be useful when creating many cabinets with partially similar parameters, like style, height, etc.

        :param kwargs: any subset of Cabinet constructor parameters
        """
        Defaults.update_defaults('Cabinet', **kwargs)

    @Defaults.use_defaults('Cabinet')
    def __init__(self, name, parent=None, style=None, height=None, depth=None, back=0.0,
                 origin="L", dx=0, dy=0, dz=0, rx=0, ry=0, rz=0,
                 l_side="D", r_side="D", t_side="D", b_side="D",
                 l_ext_side=None, r_ext_side=None, t_ext_side=None, b_ext_side=None,
                 backdrop='D', back_bottom_link=0, back_top_link=0, back_links_outside=0,
                 top_link_width=0.07,
                 work_surface=None, work_surface_ext_l=0, work_surface_ext_r=0,
                 base=0, base_ext_l=0, base_ext_r=0,
                 l_gap=None, r_gap=None, t_gap=None, b_gap=None,
                 bottom_space=0, top_space=0, sections=None):
        """
        Create a new cabinet.

        ``l_ext_side, r_ext_side, t_ext_side, b_ext_side`` - side panels outside facade doors (visible frame).
        Values for all sides: None - no external side panel,
        'F' - full depth, covering space behind frame (==depth+back),
        'N' - as a frame (==depth)

        ``l_side, r_side, t_side, b_side`` - side panels behind facade doors (invisible frame)
        Values for top/bottom: 'F' - between left/right sides, 'E' - cover left/right sides;
        for top: 'N' - two narrow links between L/R sides(front/back);
        for all sides: None - no side panel, 'F' - normal side panel, 'D' - auto.

        :param name: name of this cabinet
        :param parent: parent ObjectWrapper
        :param style: style definition (see DefaultStyle for example)
        :param height: full height of the cabinet (external outline)
        :param depth: cabinet frame depth, including facade (not the working surface depth!)
        :param back: free space behind the cabinet frame
        :param origin: 'L' - dx points to the left side of cabinet, 'R' - to the right side
        :param dx: X position of the left/right side of cabinet outline
        :param dy: Y position of the far side of cabinet outline (back side of frame is at y-back, Y axis is negative)
        :param dz: Z position of the bottom side of of cabinet outline
        :param rx: rotation around X axis
        :param ry: rotation around Y axis
        :param rz: rotation around Z axis
        :param l_side: type of the left side panel behind facade doors
        :param r_side: type of the right side panel behind facade doors
        :param t_side: type of the top side panel behind facade doors
        :param b_side: type of the bottom side panel behind facade doors
        :param l_ext_side: type of the left side panel outside facade doors (visible frame)
        :param r_ext_side: type of the right side panel outside facade doors (visible frame)
        :param t_ext_side: type of the top side panel outside facade doors (visible frame)
        :param b_ext_side: type of the bottom side panel outside facade doors (visible frame)
        :param backdrop: backdrop material, 'D' - take from the style, None - no backdrop
        :param back_bottom_link: if >0 - add link between l/r sides to back bottom
        :param back_top_link: if >0 - add link between l/r sides to back top
        :param back_links_outside: if True - back links will we outside (not between sides, but covering them)
        :param top_link_width: if t_side='N' - width of the top front/back links
        :param work_surface: width (==depth) of the top working surface, or None for no working surface
        :param work_surface_ext_l: extend working surface across the left side of outline
        :param work_surface_ext_r: extend working surface across the right side of outline
        :param base: height of the basement
        :param base_ext_l: extend the basement panel across the left side of outline
        :param base_ext_r: extend the basement panel across the left side of outline
        :param l_gap: gap around the facade (left side), None for default value
        :param r_gap: gap around the facade (right side), None for default value
        :param t_gap: gap around the facade (top side), None for default value
        :param b_gap: gap around the facade (bottom side), None for default value
        :param bottom_space: leave space below the bottom side but between the left/right sides
        :param top_space: leave space on top of the top side but between the left/right sides
        :param sections: list[Section] of sections, also see .section method
        """
        assert parent is not None
        self.parent = parent
        self.style = style or DefaultStyle()
        self.name = name
        self.height = height
        self.depth = depth
        self.back = back

        self.origin = origin
        self.offset = [dx, dy, dz]
        self.rotation_euler = [rx, ry, rz]

        self.backdrop = style.backdrop_material() if backdrop == 'D' else backdrop
        self.back_bottom_link = back_bottom_link
        self.back_top_link = back_top_link
        self.back_links_outside = back_links_outside

        self.work_surface = work_surface
        self.work_surface_ext_l = work_surface_ext_l
        self.work_surface_ext_r = work_surface_ext_r

        self.ext_sides = dict(L=l_ext_side, R=r_ext_side, T=t_ext_side, B=b_ext_side)
        self.sides = dict(L=l_side, R=r_side, T=t_side, B=b_side)
        self.top_link_width = top_link_width

        self.facade_gaps = dict(L=l_gap, R=r_gap, T=t_gap, B=b_gap)

        self.base = base
        self.base_ext_r = base_ext_r
        self.base_ext_l = base_ext_l

        self.bottom_space = bottom_space
        self.top_space = top_space
        self.sections = sections or []

    def section(self, width, doors=0, shelves=0, shelves_space=0.04, drawers=None, drawers_below=False,
                fixed_facade_left=0.0, fixed_facade_right=0.0):
        """
        Add a new section to the cabinet.

        :param width: section facade width
        :param doors: None - no doors, 'L' - left door, 'R' - right door, 2 - two doors (L/R)
        :param shelves: number of shelves, 0 - no shelves
        :param shelves_space: space between the door and the front edge of the shelves
        :param drawers: list[[float] - drawers heights
        :param drawers_below: True - put drawers below doors, False - put doors below drawers
        :param fixed_facade_left: width of a fixed part of the facade on the left side
        :param fixed_facade_right: width of a fixed part of the facade on the right side
        """
        self.sections.append(Section(width, doors=doors, shelves=shelves, shelves_space=shelves_space, drawers=drawers,
                                     drawers_below=drawers_below, fixed_facade_left=fixed_facade_left,
                                     fixed_facade_right=fixed_facade_right))
        return self

    @staticmethod
    def n(name):
        return Cabinet.part_names_map.get(name, name)

    def __preprocess_params(self):
        for side in 'LRTB':
            if self.facade_gaps[side] is None:
                if self.ext_sides[side] is not None or side == 'T' and self.work_surface is not None:
                    self.facade_gaps[side] = self.style.gap()
                elif side == 'L' or side == 'R':
                    self.facade_gaps[side] = self.style.gap() / 2
                else:
                    self.facade_gaps[side] = 0

        if self.offset[-1] == 0:
            side_defaults = 'FFEN' if (self.work_surface is not None) else 'FFEF'  # LRBT
        else:
            side_defaults = 'FFFN' if (self.work_surface is not None) else 'FFFE'

        ext_side_defaults = [None, None, None, None]
        if len(self.sections) > 0 and len(self.sections[0].drawers) > 0:
            ext_side_defaults[0] = 'F'
        if len(self.sections) > 0 and len(self.sections[-1].drawers) > 0:
            ext_side_defaults[1] = 'F'

        for side, default, ext_default in zip('LRBT', side_defaults, ext_side_defaults):
            if self.sides[side] == 'D':
                self.sides[side] = default if self.ext_sides[side] is None else ext_default

    def __make_work_surface(self, root, outline):
        if self.work_surface is None:
            return
        material = self.style.work_surface_material()
        box = outline.cut_top(material.thickness)
        box.shrink(left=-self.work_surface_ext_l, right=-self.work_surface_ext_r)
        box.d = self.work_surface
        material.make_box(root, self.n('work_surface'), box, 'T')

    def __make_backdrop(self, root, outline):
        if self.backdrop is None:
            return
        box = outline.cut_back(self.backdrop.thickness)
        if self.backdrop.thickness < 0.005:
            box.shrink(left=0.001, right=0.001)
            box.shrink(bottom=0.001 + self.bottom_space, top=0.001 + self.top_space)

        self.backdrop.make_box(root, self.n('backdrop'), box, 'F', no_edges='LRTB')

    def __make_ext_box(self, root, outline):
        backdrop_over_ext_side = {s: False for s in 'LRBT'}
        backdrop_thickness = self.backdrop.thickness if self.backdrop else 0
        backdrop_box = outline.copy().cut_back(backdrop_thickness)
        backdrop_y = backdrop_box.y
        if self.backdrop and backdrop_thickness < 0.005:
            for side in 'LRBT':
                backdrop_over_ext_side[side] = (
                        self.ext_sides[side] and not self.sides[side]
                        and not (side == 'T' and self.back_top_link > 0)
                        and not (side == 'B' and self.back_bottom_link > 0)
                )
            has_n_type_ext_sides = any(backdrop_over_ext_side[side] and self.ext_sides[side] == 'N' for side in 'LRBT')
            if has_n_type_ext_sides:
                for side in 'LRBT':
                    if self.ext_sides[side] == 'F':
                        backdrop_over_ext_side[side] = False

        for side, name, cut_box, cut_backdrop, view, edges in (
                ('T', self.n('top_ext_side'), outline.cut_top, backdrop_box.cut_top, 'T', 'LRN'),
                ('L', self.n('left_ext_side'), outline.cut_left, backdrop_box.cut_left, 'L', 'TBN'),
                ('R', self.n('right_ext_side'), outline.cut_right, backdrop_box.cut_right, 'L', 'TBN'),
                ('B', self.n('bottom_ext_side'), outline.cut_bottom, backdrop_box.cut_bottom, 'T', 'LRN')
        ):
            if side == 'B' and self.base != 0:
                self.__make_base(root, outline)
                cut_backdrop(self.base)
            if self.ext_sides[side] is not None:
                material = self.style.ext_side_material()
                box = cut_box(material.thickness)
                if self.ext_sides[side] == 'N':
                    box.cut_back(self.back)
                if backdrop_over_ext_side[side]:
                    backdrop_y = min(backdrop_y, box.y)
                    box.cut_back(backdrop_thickness)
                else:
                    cut_backdrop(material.thickness)
                material.make_box(root, name, box, view, edges=edges)

        outline.cut_back(self.back)

        if self.back_links_outside:
            self.__make_back_links(root, outline)

        if self.backdrop:
            if any(backdrop_over_ext_side.values()):
                backdrop_box.shift(dy=backdrop_y - backdrop_box.y)
                if outline.y >= backdrop_box.y - backdrop_box.d:
                    outline.cut_back_at(backdrop_box.y - backdrop_box.d)
            else:
                backdrop_box = outline.cut_back(backdrop_thickness)

            if backdrop_thickness < 0.005:
                backdrop_box.shrink(left=0.001, right=0.001,
                                    bottom=0.001 + self.bottom_space, top=0.001 + self.top_space)

            self.backdrop.make_box(root, self.n('backdrop'), backdrop_box, 'F', no_edges='LRTB')

    def __make_base(self, root, outline):
        material = self.style.box_material()
        box = outline.cut_bottom(self.base)
        box.cut_front(self.style.facade_material().thickness)
        box = box.cut_front(material.thickness)
        box.shrink(left=-self.base_ext_l, right=-self.base_ext_r)
        material.make_box(root, self.n('base'), box, 'F', edges='')
        Hardware.add('%s(%i)' % (self.n('legs'), int(self.base * 1000)), len(self.sections) * 2 + 2)

    def __make_back_links(self, root, compartment):
        if (self.back_bottom_link == 0) and (self.back_top_link == 0):
            return
        material = self.style.box_material()
        box = compartment.cut_back(material.thickness)
        if self.back_bottom_link != 0:
            material.make_box(root, self.n('back_link_b'), box.cut_bottom(self.back_bottom_link), 'F', edges='')
        if self.back_top_link != 0:
            material.make_box(root, self.n('back_link_t'), box.cut_top(self.back_top_link), 'F', edges='')

    def __make_box(self, root, outline):
        material = self.style.box_material()
        facade_box = outline.cut_front(self.style.facade_material().thickness)
        for side, name, cut_func, view, edges in (
                ('T', self.n('top_side'), outline.cut_top, 'T', 'LRN'),
                ('B', self.n('bottom_side'), outline.cut_bottom, 'T', 'LRN')):
            if self.sides[side] == "E":
                material.make_box(root, name, cut_func(material.thickness), view, edges=edges)

        for side, name, cut_func, view, edges in (
                ('L', self.n('left_side'), outline.cut_left, 'L', 'TBN'),
                ('R', self.n('right_side'), outline.cut_right, 'L', 'TBN')):
            if self.sides[side] == 'F':
                material.make_box(root, name, cut_func(material.thickness), view, edges=edges)

        if self.bottom_space != 0:
            outline.cut_bottom(self.bottom_space)
        if self.top_space != 0:
            outline.cut_top(self.top_space)

        if self.sides['B'] == 'F':
            material.make_box(root, self.n('bottom_side'), outline.cut_bottom(material.thickness), 'T', edges='LRN')

        return facade_box

    def __make_section_box(self, root, compartment):
        material = self.style.box_material()
        if self.sides['T'] == "F":
            material.make_box(root, self.n('top_side'), compartment.cut_top(material.thickness), 'T', edges='N')
        elif self.sides['T'] == "N":
            box = compartment.cut_top(material.thickness)
            material.make_box(root, self.n('top_link'), box.cut_front(self.top_link_width), 'T', edges='N')
            material.make_box(root, self.n('top_link'), box.cut_back(self.top_link_width), 'T', edges='')

        if not self.back_links_outside:
            self.__make_back_links(root, compartment)

    def make(self):
        """
        Make a cabinet, described by this object.

        :return: ObjectWrapper for the root of created cabinet
        """
        self.__preprocess_params()

        total_width = 0
        for section in self.sections:
            total_width += section.width
        for side in 'LR':
            if self.ext_sides[side] is not None:
                total_width += self.style.ext_side_material().thickness
        outline = Box(w=total_width, h=self.height, d=self.depth + self.back)

        root = self.parent.group(self.name, offset=self.offset)

        self.__make_work_surface(root, outline)
        self.__make_ext_box(root, outline)
        facade_box = self.__make_box(root, outline)
        facade_box.shrink(bottom=self.facade_gaps['B'] - self.style.gap() / 2,
                          top=self.facade_gaps['T'] - self.style.gap() / 2)

        box_material = self.style.box_material()
        for i, section in enumerate(self.sections):
            if i == len(self.sections) - 1:
                facade, box = facade_box, outline
            else:
                facade = facade_box.cut_left(section.width)
                box = outline.cut_left_at(facade_box.x - box_material.thickness / 2)
                box_material.make_box(root, self.n('separator'),
                                      outline.cut_left(box_material.thickness), 'L', edges='N')
            if i == len(self.sections) - 1:
                facade.shrink(right=self.facade_gaps['R'] - self.style.gap() / 2)
            if i == 0:
                facade.shrink(left=self.facade_gaps['L'] - self.style.gap() / 2)
            self.__make_section_box(root, box)
            section.make(root, self.style, box, facade)

        if self.origin == "R":
            root.move(dx=-total_width)
        root.rotate_euler(*self.rotation_euler)
        return root, total_width


class Section:
    """
    Cabinet section builder.
    """

    def __init__(self, width, doors=0, shelves=0, shelves_space=0.04, drawers=None, drawers_below=False,
                 fixed_facade_left=0.0, fixed_facade_right=0.0):
        """
        :param width: section facade width
        :param doors: None - no doors, 'L' - left door, 'R' - right door, 2 - two doors (L/R)
        :param shelves: number of shelves, 0 - no shelves
        :param shelves_space: space between the door and the front edge of the shelves
        :param drawers: list[[float] - drawers heights
        :param drawers_below: True - put drawers below doors, False - put doors below drawers
        :param fixed_facade_left: width of a fixed part of the facade on the left side
        :param fixed_facade_right: width of a fixed part of the facade on the right side
        """
        self.width = width
        self.doors = doors
        self.shelves = shelves
        self.shelves_space = shelves_space
        self.drawers = drawers or []
        self.drawers_below = drawers_below
        self.fixed_facade_left = fixed_facade_left
        self.fixed_facade_right = fixed_facade_right

    @staticmethod
    def n(name):
        return Cabinet.n(name)

    def __make_drawer(self, root, style, compartment, facade):
        facade_mat = style.facade_material()
        facade.shrink(left=style.gap() / 2, right=style.gap() / 2, bottom=style.gap() / 2, top=style.gap() / 2)
        facade = facade.cut_back(facade_mat.thickness)
        drawer_door = facade_mat.make_box(root, self.n('drawer_facade'), facade, 'F')

        handle = style.handle_type(facade.w, 'H')
        handle_dx, handle_dz = style.drawer_handle_position(facade.w, facade.h, handle)
        handle.attach_to(drawer_door, dx=handle_dx, dz=handle_dz, ry=90)

        compartment.cut_back(style.drawer_space_back())

        space = style.drawer_space_left_right()
        compartment.shrink(left=space, right=space)

        bottom_material = style.drawer_bottom_material(facade.w)
        compartment.shrink(bottom=style.drawer_space_bottom(bottom_material.thickness), top=style.drawer_space_top())
        compartment.shift(-facade.x, -facade.y + facade.d, -facade.z)

        Hardware.add("%s(%i)" % (self.n('drawer_sliders'), int(compartment.d * 1000)), 1)
        bottom_material.make_box(drawer_door, self.n('drawer_bottom'),
                                 compartment.cut_bottom(bottom_material.thickness), 'T', edges='')

        box_mat = style.drawer_material()
        box_mat.make_box(drawer_door, self.n('drawer_side'), compartment.cut_left(box_mat.thickness), 'L', edges='T')
        box_mat.make_box(drawer_door, self.n('drawer_side'), compartment.cut_right(box_mat.thickness), 'L', edges='T')
        box_mat.make_box(drawer_door, self.n('drawer_front'), compartment.cut_front(box_mat.thickness), 'F', edges='T')
        box_mat.make_box(drawer_door, self.n('drawer_back'), compartment.cut_back(box_mat.thickness), 'F', edges='T')

    def __make_drawers(self, root, style, box, facade_box):
        if len(self.drawers) == 0:
            return

        if self.drawers_below:
            drawers_facade = facade_box.cut_bottom(sum(self.drawers))
        else:
            drawers_facade = facade_box.cut_top(sum(self.drawers))

        box_mat = style.box_material()
        if facade_box.h <= 0:
            drawers_compartment = box
        elif self.drawers_below:
            drawers_compartment = box.cut_bottom_at(facade_box.z - box_mat.thickness / 2)
            box_mat.make_box(root, self.n('separator'), box.cut_bottom(box_mat.thickness), 'T', edges='N')
        else:
            drawers_compartment = box.cut_top_at(drawers_facade.z + box_mat.thickness / 2)
            box_mat.make_box(root, self.n('separator'), box.cut_top(box_mat.thickness), 'T', edges='N')

        for i, h in enumerate(self.drawers):
            if i == len(self.drawers) - 1:
                drawer_facade = drawers_facade
                drawer_compartment = drawers_compartment
            else:
                drawer_facade = drawers_facade.cut_top(h)
                drawer_compartment = drawers_compartment.cut_top_at(drawer_facade.z)
            self.__make_drawer(root, style, drawer_compartment, drawer_facade)

    def __make_shelves(self, root, style, compartment):
        if not self.shelves:
            return
        box_mat = style.box_material()
        compartment.cut_front(self.shelves_space)
        step = compartment.h / (self.shelves + 1)
        for i in range(self.shelves):
            box = compartment.slice_z(compartment.z - box_mat.thickness / 2 +
                                      round(1000 * step * (i + 1)) * 0.001, box_mat.thickness)
            box_mat.make_box(root, self.n('shelf'), box, 'T', edges='N')
        Hardware.add(self.n('shelf_holder'), 4 * self.shelves)

    def __make_doors(self, root, style, compartment, facade_box):
        if self.doors == 0 or facade_box.h <= 0:
            return

        facade_mat = style.facade_material()
        if self.fixed_facade_left > 0:
            box = facade_box.cut_left(self.fixed_facade_left)
            facade_mat.make_box(root, self.n('fixed_facade'), box.cut_back(facade_mat.thickness), 'F')

        if self.fixed_facade_right > 0:
            box = facade_box.cut_right(self.fixed_facade_right)
            facade_mat.make_box(root, self.n('fixed_facade'), box.cut_back(facade_mat.thickness), 'F')

        facade_box.shrink(left=style.gap() / 2, right=style.gap() / 2, bottom=style.gap() / 2, top=style.gap() / 2)

        doors(root, self.n('door'), facade_box.w, facade_box.h, facade_box.x, facade_box.y, facade_box.z, self.doors,
              "F" if (self.fixed_facade_left > 0) else compartment.x - facade_box.x,
              "F" if (self.fixed_facade_right > 0) else facade_box.x + facade_box.w - (compartment.x + compartment.w),
              style)

    def make(self, root, style, box, facade_box):
        """Invoked by Cabinet.make  method to make the section, don't use this directly."""
        if self.doors == 0 and len(self.drawers) > 0:
            last_box = facade_box.h - sum(self.drawers)
            if last_box > 0.05:
                self.drawers.append(last_box)
            else:
                self.drawers[-1] += last_box

        self.__make_drawers(root, style, box, facade_box)
        self.__make_shelves(root, style, box)
        self.__make_doors(root, style, box, facade_box)
