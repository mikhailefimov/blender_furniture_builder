# Furniture making script for Blender

This is a library for programmatically designing cabinet furniture in Blender.
Generated models contain all important parts resembling actual construction of cabinet furniture.
This script can even generate a bill of materials with parts sizes.

It's is not 'end-user product', you need to know some Python to use it. 

![example image](https://github.com/mikhailefimov/blender_furniture_builder/raw/master/example.jpg)

# Compatibility 
This version works in Blender 2.80 (or, maybe, newer).
It will not work in older versions due to breaking API changes in Blender 2.80.

# Usage
- Copy-paste `furniture_lib.py` to blender file in scripting mode as a text module.
- In another text module write your script, using this library.
- Press "Run Script" button `(Alt-P)` to execute.
- Wait...

# Examples

Simple script to generate one cabinet with 2 doors and working surface:
```python
import bpy
gen_lib = bpy.data.texts["furniture_lib.py"].as_module()

furniture_root = gen_lib.Root('Furniture', collection='Furniture')
style=gen_lib.DefaultStyle()

gen_lib.Cabinet("SimpleCabinet", parent=furniture_root, style=style, height=0.8, base=0.1, depth=0.5, back=0.05, work_surface=0.6).\
    section(0.8, doors=2, shelves=1).\
    make()
```

See also `example.blend` and `example.py` files for more scripting examples.

# License

This script is unusable without Blender, so it is licensed under GNU GPL Version 2 or later as Blender itself.
See https://www.blender.org/about/license/ for licensing details.

Of course, there may be bugs and corner cases, so use this at your own risk. 
