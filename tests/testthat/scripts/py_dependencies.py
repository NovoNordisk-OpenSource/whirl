import pandas as pd
import numpy as np

def main():
    # Create a DataFrame with random data
    df = pd.DataFrame({
        'A': np.random.rand(10),
        'B': np.random.rand(10),
        'C': np.random.rand(10)
    })

    # Print the DataFrame
    print("Original DataFrame:")
    print(df)

    # Calculate the mean of each column
    means = df.mean()
    print("\nMeans of each column:")
    print(means)

if __name__ == "__main__":
    main()
