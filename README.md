# Time Series Model Analysis

A simple R-based project for monthly sales time series analysis, forecasting, and model comparison.

## Project Structure

```text
TimeSeriesModelAnalysis/
├── Sales.csv
├── project.Rproj
├── generateSalesData.R
├── loadSplitTrainTest.R
├── checkOutliers.R
├── SARIMA.R
├── ETS.R
├── HW.R
├── TBATS.R
├── SNaive.R
└── compareModels.R
```

## Models Used

- SARIMA
- ETS (AAA)
- Holt-Winters Additive
- TBATS
- Seasonal Naive

## Notes

- The dataset is loaded from `Sales.csv`.
- Training and test split is handled in `loadSplitTrainTest.R`.
- Final model metrics are summarized in `compareModels.R`.

## Author

Lim Feng Zhi
Ng Chiao Han
Ngoh Jia Ying
Tham Zhen Hern
Tho Hui Yee
