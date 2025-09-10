print("SCRIPT STARTED - This should appear if the script runs at all")

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
    print("MAIN FUNCTION STARTED")
    df = pd.DataFrame({
        'A': [np.sum([1, 2])]
    })

    print("DataFrame created:")
    print(df)
    print("MAIN FUNCTION COMPLETED")


if __name__ == "__main__":
    print("SCRIPT RUNNING AS MAIN")
    main()
    print("SCRIPT COMPLETED")
