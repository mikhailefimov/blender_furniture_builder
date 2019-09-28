import bpy
import os

try:
    import furniture_lib
except:
    furniture_lib = bpy.data.texts["furniture_lib.py"].as_module()


class KitchenStyle(furniture_lib.DefaultStyle):
    MDF = furniture_lib.Sheet(0.016, 'MDF', 2.0, 0.4)
    MDF_10 = furniture_lib.Sheet(0.01, 'MDF', 2.0, 0.4)
    HDF = furniture_lib.Sheet(0.004, 'HDF')
    MDF_25 = furniture_lib.Sheet(0.025, 'MDF', 2.0, 0.4)
    WorkSurface = furniture_lib.Sheet(0.028, 'WorkSurface', 2.0, 0.4)
    FACADE = furniture_lib.Sheet(0.016, 'FACADE', 2.0, 0.4)

    def box_material(self):
        return self.MDF

    def backdrop_material(self):
        return self.HDF

    def facade_material(self):
        return self.FACADE

    def ext_side_material(self):
        return self.MDF_25

    def work_surface_material(self):
        return self.WorkSurface

    def drawer_material(self):
        return self.MDF

    def drawer_bottom_material(self, width):
        if width <= 0.45:
            return self.HDF
        else:
            return self.MDF_10

    def door_handle_orientation(self, width, h_bottom, h_top):
        return 'V'


kitchen_style = KitchenStyle()

furniture_root = furniture_lib.Root('Kitchen', collection='Kitchen')

furniture_lib.Cabinet.set_defaults(parent=furniture_root, style=kitchen_style, height=0.8, base=0.1, depth=0.5,
                                   back=0.05, work_surface=0.6)

furniture_lib.Cabinet("SimpleCabinet"). \
    section(0.8, doors=2, shelves=1). \
    make()

furniture_lib.Cabinet("CabinetWithExtFrame", dx=1.0, l_ext_side="N", r_ext_side="F"). \
    section(0.8, doors=2). \
    section(0.4, drawers=[0.1, 0.15, 0.15]). \
    make()

furniture_lib.Cabinet.set_defaults(parent=furniture_root, style=kitchen_style, dz=1.5, height=0.8, depth=0.4)
furniture_lib.Cabinet("TopCabinet"). \
    section(0.8, doors=2, shelves=1). \
    make()

furniture_lib.Cabinet("TopCabinet", dx=1.0, l_ext_side="N", r_ext_side="N", t_ext_side="N", b_ext_side="N"). \
    section(0.8, doors=2, shelves=2). \
    section(0.4, doors='R', shelves=1). \
    make()

furniture_lib.Cabinet("Hood cabinet", dx=3.0, bottom_space=0.135). \
    section(0.6, doors=2, shelves=1). \
    make()

room_root = furniture_lib.Root('Room', collection='Room')


class RoomStyle(furniture_lib.DefaultStyle):
    MDF = furniture_lib.Sheet(0.016, 'MDF2', 2.0, 0.4)
    MDF_10 = furniture_lib.Sheet(0.01, 'MDF2', 2.0, 0.4)
    HDF = furniture_lib.Sheet(0.004, 'HDF')
    MDF_25 = furniture_lib.Sheet(0.025, 'MDF2', 2.0, 0.4)
    FACADE = furniture_lib.Sheet(0.016, 'FACADE', 2.0, 0.4)

    def box_material(self):
        return self.MDF

    def backdrop_material(self):
        return self.HDF

    def facade_material(self):
        return self.FACADE

    def ext_side_material(self):
        return self.MDF_25

    def work_surface_material(self):
        return self.MDF

    def drawer_material(self):
        return self.MDF

    def drawer_bottom_material(self, width):
        if width <= 0.45:
            return self.HDF
        else:
            return self.MDF_10

    def door_handle_orientation(self, width, h_bottom, h_top):
        return 'V'

    def door_handle_position(self, width, height, h_bottom, handle, orientation):
        dx = width - 0.05 - (handle.length / 2 if (orientation == "H") else 0)
        if h_bottom < 1.0:
            dz = height / 2
        else:
            dz = 0.05 + (handle.length / 2 if (orientation == "V") else 0)
        return dx, dz


room_style = RoomStyle()

furniture_lib.Cabinet.set_defaults(parent=room_root, style=room_style, origin='R', base=0.1, depth=0.6)

X = -0.1
X -= furniture_lib.Cabinet("RoomCabinet1", dx=X, height=2.0, l_ext_side="F", r_ext_side="F", t_ext_side="F"). \
    section(0.8, doors=2, shelves=4). \
    make()[1]

X -= furniture_lib.Cabinet("RoomCabinet2", dx=X, height=0.7, l_ext_side="F", r_ext_side="F", t_ext_side="F"). \
    section(0.8, drawers=[0.2, 0.2, 0.2]). \
    section(0.8, drawers=[0.2, 0.2, 0.2]). \
    section(0.8, drawers=[0.2, 0.2, 0.2]). \
    make()[1]

furniture_lib.Cabinet.set_defaults(parent=room_root, style=room_style, origin='L', depth=0.4)

X += furniture_lib.Cabinet("RoomCabinet3", dx=X, dz=0.7, height=1.3,
                           l_ext_side="F", r_ext_side="F", t_ext_side="F", b_ext_side="F"). \
    section(0.4, doors='L', shelves=4). \
    make()[1]

furniture_lib.Cabinet("RoomCabinet4", dx=X, dz=1.5, height=0.5, t_ext_side="F", b_ext_side="F"). \
    section(0.4, doors='R'). \
    section(0.8, doors=2). \
    section(0.8, doors=2). \
    make()

furniture_lib.Sheet.save_bom(os.path.join(os.path.dirname(bpy.data.filepath), 'bom_sheet.csv'))
furniture_lib.Hardware.save_bom(os.path.join(os.path.dirname(bpy.data.filepath), 'bom_hardware.csv'))
