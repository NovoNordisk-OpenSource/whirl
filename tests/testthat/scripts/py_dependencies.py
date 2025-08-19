import pandas as pd
import numpy as np

def main():
    # Create a DataFrame with random data
    df = pd.DataFrame({
        'A': [np.sum([1,2])]
    })

    # Print the DataFrame
    print("DataFrame:")
    print(df)

if __name__ == "__main__":
    main()
