import json
import sys
import subprocess

temp_dir = r.params['tmpdir']

# Get a list of installed packages using pip list
installed_packages = subprocess.check_output([sys.executable, '-m', 'pip', 'list', '-v']).decode('utf-8').strip().split('\n')[2:]
installed_packages = {package.split()[0]: [package.split()[1], package.split()[2]] for package in installed_packages}

imported_modules = {}
for module_name, module in sys.modules.items():
    if not (module_name.startswith('_') or module_name.startswith('.') or '.' in module_name):
        if module_name in installed_packages:
            imported_modules[module_name] = {
                "version": installed_packages[module_name][0],
                "installation_path": installed_packages[module_name][1]
            }

with open(f'{temp_dir}/python_imports.json', 'w') as json_file:
    json.dump(imported_modules, json_file)