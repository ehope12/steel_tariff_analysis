# Trade, Tariffs, and Steel: A Multi-Pronged Econometric Analysis

## 📌 Project Overview

This paper investigates how the 2018 tariffs on steel, aluminum, and Chinese imports impacted the steel industry in the United States. Specifically, it seeks to answer the question: did the 2018 tariffs on steel impact employment and trade metrics in the United States steel industry?

[Link to paper PDF](https://drive.google.com/file/d/1aotxiQV0tOdJVZ__d2sFc_dJmyYC8EKq/view?usp=sharing)

## 📊 Data

- **Sources**: UN Comtrade Database, The World Bank Group, The World Steel Association
- **Files Included**:
  - `TotalExport.xlsx`: Total exports per year by country.
  - `TotalImport.xlsx`: Total imports per year by country.
  - `Export.xlsx`: Steel exports per year by country.
  - `Import.xlsx`: Steel imports per year by country.
  - `GDP_Pop_Data.xlsx`: GDP and population data per year by country.

## ⚙️ How to Run

### Requirements

Install necessary R packages if you haven't already:

```r
install.packages(c("Synth", "dplyr", "readr", "ggplot2", "gsynth"))
```

For the Synthetic Control analysis in the Synthetic Control folder, run
```r
Export.R
```

for the steel export counterfactual, and
```r
Import.R
```

for the steel import counterfactual.

## 📈 Graphs and Visualizations

### Difference in Differences

### Synthetic Control

#### Best Model Visualizations:

Export:

![Best Model Export](Synthetic%20Control/Best%20Model%20(Export).png)

Import:

![Best Model Import](Synthetic%20Control/Best%20Model%20(Import).png)

#### All Model Visualizations:

Export:

![Best Model Export](Synthetic%20Control/All%20Models%20(Export).png)

Import:

![Best Model Import](Synthetic%20Control/All%20Models%20(Import).png)

## 👥 Authors

- Emily Hsieh [emilyh10@illinois.edu]: Worked on Synthetic Control model and analysis
