import pandas as pd # pylint: disable=import-error # pyright: ignore[reportMissingImports] # noqa: F501
import numpy as np # pylint: disable=import-error # pyright: ignore[reportMissingImports] # noqa: F501

def main():
    df = pd.DataFrame({
        'A': [np.sum([1,2])]
    })

    print(df)

if __name__ == "__main__":
    main()
