import pandas as pd
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler

# Load the CSV
df = pd.read_csv("Source Data/2023-2024 NBA Player Stats Per Game.csv")

df = df[df['G'] >= 10]  # Keep players with 10+ games

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

# Create final DataFrame
pca_df = pd.DataFrame(data=principal_components, columns=['PC1', 'PC2'])
pca_df['Position'] = df_clean['Pos'].values
pca_df['Player'] = df_clean['Player'].values

# Save to CSV for R
pca_df.to_csv("Source Data/new_clustering_2023-2024.csv")
print("CSV saved successfully!")
