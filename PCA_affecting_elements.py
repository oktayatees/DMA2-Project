import pandas as pd
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler
import numpy as np

# Load the CSV
df = pd.read_csv("Source Data/2023-2024 NBA Player Stats Per Game.csv")

# Keep players with 10+ games
df = df[df['G'] >= 10]

# Select relevant columns
features = ['PTS', 'AST','TRB', 'STL', 'TOV', 'BLK', '3P%', 'FG%', "FT%"]
df = df[['Player', 'Pos'] + features]

# Drop rows with missing data
df_clean = df.dropna()

# Standardize features
X = StandardScaler().fit_transform(df_clean[features])

# Apply PCA
pca = PCA(n_components=2)
principal_components = pca.fit_transform(X)

# Create final PCA score DataFrame
pca_df = pd.DataFrame(data=principal_components, columns=['PC1', 'PC2'])
pca_df['Position'] = df_clean['Pos'].values
pca_df['Player'] = df_clean['Player'].values

# Save to CSV
pca_df.to_csv("Source Data/new_clustering_2023-2024.csv", index=False)
print("CSV saved successfully!")

# --------------------------
# PCA Loadings Interpretation
# --------------------------

# Loadings = PCA.components_ × sqrt(explained variance)
loadings = pca.components_.T  # shape: (features, components)
loading_df = pd.DataFrame(loadings, index=features, columns=['PC1', 'PC2'])

# Sort by absolute value of contribution to PC1 and PC2
sorted_pc1 = loading_df['PC1'].abs().sort_values(ascending=False)
sorted_pc2 = loading_df['PC2'].abs().sort_values(ascending=False)

print("\nTop contributions to PC1:")
print(loading_df.loc[sorted_pc1.index])

print("\nTop contributions to PC2:")
print(loading_df.loc[sorted_pc2.index])
