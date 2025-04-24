# PSA for Top 100+ Fishes in the Philippine Marine Ornamental Trade

This repository includes the R code used for the Productivity Susceptibility Analysis conducted for the top 100+ fishes in the Philippine Marine Ornamental Trade.

After data was collated, an R code applying a a Gaussian Mixture Model in the Mclust package was used to cluster datapoints in the Productivity-Susceptibility Analysis conducted for the top 100+ species in the Philippine marine ornamental trade. The input file (PSAscoresPH.csv) is attached, as well as the resulting table showing the clusters & categories (PSAresults.csv) & the graph (Gmm Clustering PSA PH.png).

A summary for Vulnerability scores per Family is also included in this page where data was wrangled to show the range of V, mean V, and V for each species. The R code used to visualize this data (VbyFamily.R) is also included in this page, along with the input data (FamilyV.csv) & resulting plot (VbyFamily.png).

Statistical tests to compare Global PSA vulnerability scores with National PSA vulnerability scores are also included (StatisticalComparisonGlobalvsNational.R).
