
# 🏀 NBA Player Role Evolution Analysis (1993–94 vs. 2023–24)

## 📌 Project Overview
This project investigates how the roles, tendencies, and statistical profiles of NBA players have evolved over the last three decades—specifically comparing the 1993–94 and 2023–24 seasons. It focuses on analyzing whether modern players, particularly centers and power forwards, have adopted more versatile roles in response to the evolution of the game.

> **Author:** Oktay Taha Ateş  
> **Data Size:** 977 Players  
> **Tools:** Python, R, Excel, scikit-learn, matplotlib, seaborn  
> **Data Source:** [Basketball Reference](https://www.basketball-reference.com)

---

## 📂 Repository Structure

```plaintext
📁 Images/                        # Visuals used in the report/presentation
📁 Source Data/                  # Raw and cleaned datasets
📁 Presentation/                  # Includes the ppt slides for the project
📄 Analysis and summary on the correlation of 3P vs VORP.R
📄 Analysis table.R
📄 Clustering.py
📄 Correlation of 3P Stats with VORP (P-Value Analysis).R
📄 Missing Value Analysis.R
📄 Over_Under Performing Players Based on VORP Model.R
📄 PCA_affecting_elements.py
📄 PCA_Graph_Creator.R
📄 Role Archetypes(93-94) vs. (23-24).R
📄 Stat Averages.R
📄 TRB% vs 3PAr Correlation by Position and Year.R
📄 Z-Score.R
```

---

## 📊 Key Analyses

### 🧹 Missing Value Handling
- All missing values were percentage-related stats.
- Replaced missing entries with `0` to preserve player count (negligible effect: <1%).

### 📈 Statistical Analysis
- **Z-Score Normalization** for calculating positionless index.
- **ANOVA** for checking the significance of stat changes.
- **Correlation Analysis** between 3PM and VORP across seasons and positions.
- **3PAr, 3P%, AST** metrics showed significant increase over time, especially among big men.

### 🔬 PCA + Clustering (Position Archetypes)
- Conducted PCA and k-means clustering on normalized stats.
- 1993–94: Clear separation of roles (e.g., traditional centers).
- 2023–24: Emergence of hybrid roles, such as "playmaking centers" and "shooting bigs".

### 📉 Over/Under-Performing Analysis
- Used a model comparing predicted VORP vs actual VORP to classify players.
- Helps identify outliers in performance and emerging role players.

---

## 📌 Positionless Index
A custom metric combining **AST, STL, 3PAr, USG%, TOV, TRB, and BLK** to reflect a player's deviation from traditional role expectations. High values often indicate modern, hybrid players.

---

## 🧠 Key Findings

- 📈 3-point reliance has significantly increased league-wide.
- 🏀 Centers and forwards now contribute more in assists, spacing, and overall versatility.
- 📉 Traditional roles are dissolving — cluster overlap suggests fluidity in roles today.
- ✅ There’s statistical evidence backing the "positionless basketball" narrative.

---

## 📽 Presentation
Check out the `Data Management and Analysis Presentation.pptx` for visualizations, insights, and summary slides.

---

## 🛠️ Technologies Used

- **Python:** Data preprocessing, clustering (`sklearn`)
- **R:** Statistical tests, correlation, visualizations
- **Excel:** Manual data compilation
- **Libraries:** pandas, seaborn, matplotlib, sklearn

---

## 📎 Citation
Data sourced from: [https://www.basketball-reference.com](https://www.basketball-reference.com)

---

## ✅ To-Do / Future Work
- Add interactive dashboard (e.g., with Streamlit or Shiny)
- Expand analysis to include mid-2000s seasons
- Automate scraping for future seasons
