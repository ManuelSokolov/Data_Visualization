import pandas as pd
import numpy as np



df = pd.read_csv('unicorn_ready_for_clustering_and_map.csv', index_col=False)
df = df.drop(['Company', 'Valuation ($B)', 'Date Joined', 'Country'], axis=1)

print(df['City'].nunique())
