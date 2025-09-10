import sys
import subprocess
import os

print("=== PYTHON SCRIPT DEBUG ===")
print(f"Python executable: {sys.executable}")
print(f"Python version: {sys.version}")
print(f"Python path: {sys.path}")

# Try to import the packages
try:
    import pandas
    print(f"pandas imported successfully from: {pandas.__file__}")
    print(f"pandas version: {pandas.__version__}")
except ImportError as e:
    print(f"pandas import failed: {e}")

try:
    import numpy
    print(f"numpy imported successfully from: {numpy.__file__}")
    print(f"numpy version: {numpy.__version__}")
except ImportError as e:
    print(f"numpy import failed: {e}")

# Check what pip list shows from within the script
try:
    result = subprocess.run([sys.executable, "-m", "pip", "list"], 
                          capture_output=True, text=True, check=True)
    print("pip list from within script:")
    print(result.stdout)
except Exception as e:
    print(f"pip list failed: {e}")

print("=== END PYTHON SCRIPT DEBUG ===")

import pandas as pd  # noqa: E501 # pylint: disable=import-error # pyright: ignore[reportMissingImports]
import numpy as np  # noqa: E501 # pylint: disable=import-error # pyright: ignore[reportMissingImports]


def main():
    df = pd.DataFrame({
        'A': [np.sum([1, 2])]
    })

    print(df)


if __name__ == "__main__":
    main()
