import pandas as pd  # noqa: E501 # pylint: disable=import-error # pyright: ignore[reportMissingImports]
import numpy as np  # noqa: E501 # pylint: disable=import-error # pyright: ignore[reportMissingImports]


def main():
    df = pd.DataFrame({
        'A': [np.sum([1, 2])]
    })

    print(df)


if __name__ == "__main__":
    main()
