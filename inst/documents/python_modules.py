import json
import sys
import types

def get_loaded_packages():
    """Return all the packages that are currently loaded in the Python environment"""
    loaded = set(m.split('.')[0] for m in sys.modules if m and not m.startswith('_'))
    return loaded

def get_namespaced_packages():
    """Get all the stuff that exposed via namespacing (i.e. attached)"""
    namespaced = set()
    
    # Use __main__.__dict__ to find all imported modules in the current script
    # needed for things like reticulate
    import __main__
    if hasattr(__main__, '__dict__'):
        globals_dict = __main__.__dict__
    else:
        frame = sys._getframe(1)
        globals_dict = frame.f_globals
    
    for name, obj in globals_dict.items():
        if isinstance(obj, types.ModuleType) and not name.startswith('_'):
            module_name = getattr(obj, '__name__', name)
            root_module = module_name.split('.')[0]
            if root_module not in ['sys', 'json', 'types', 'builtins']:
                namespaced.add(root_module)
    
    return namespaced

def get_package_status(tmpfile=''):
    """Get both loaded and namespaced packages as a named list"""
    loaded = get_loaded_packages()
    namespaced = get_namespaced_packages()
    
    status = {
        'loaded': list(loaded),
        'namespaced': list(namespaced)
    }
    
    if tmpfile != '':
        with open(tmpfile, 'w') as f:
            json.dump(status, f, indent=2)
    
    return status
