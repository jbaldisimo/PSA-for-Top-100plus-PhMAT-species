# PSA for Top 100+ Fishes in the Philippine Marine Ornamental Trade

This repository includes the data (Ph PSA Scoring) & R code used for the Productivity Susceptibility Analysis conducted for the top 100+ fishes in the Philippine Marine Ornamental Trade.

A description of how the following were done is available:
1. GMM clustering for Vulnerability categories
2. Summarizing V scores per Family
3. Conducting Statistical Tests comparing National & Global PSA scores

<details><summary>1. GMM Clustering </summary>
<p>

## 1. GMM Clustering for Vulnerability categories

After Productivity, Susceptibility & Vulnerability scores were calculated, an R code applying a a Gaussian Mixture Model in the Mclust package was used to cluster datapoints from the Productivity Susceptibility Analysis conducted for the top 100+ species in the Philippine marine aquarium trade. The input file (PSAscoresPH.csv) is attached, as well as the resulting table showing the clusters & categories (PSAresults.csv) & the graph (Gmm Clustering PSA PH.png).

</p>
</details>

<details><summary>2. Summarizing V scores per Family </summary>
<p>

## 2. Summarizing V scores per Family
A summary for Vulnerability scores per Family is also included in this page where data was wrangled to show the range of V, mean V, and V for each species. The R code used to visualize this data (VbyFamily.R) is also included in this page, along with the input data (FamilyV.csv) & resulting plot (VbyFamily.png).

</p>
</details>

<details><summary>3. Conducting Statistical Tests comparing National & Global PSA scores</summary>
<p>

## 3. Conducting Statistical Tests comparing National & Global PSA scores

Statistical tests to compare Global PSA vulnerability scores with National PSA vulnerability scores are also included (StatisticalComparisonGlobalvsNational.R), along with input files & resulting plots.

</p>
</details>
