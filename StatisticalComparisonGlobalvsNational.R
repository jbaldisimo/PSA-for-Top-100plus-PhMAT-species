#Statistical Comparison of Global PSA Vulnerability Scores vs National PSA Vulnerability Scores
#0. Install & load necessary libraries
install.packages("effsize")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")

library(tidyverse)
library(effsize)
library(ggplot2)
library(dplyr)

#1. Load data
v_data <- read_csv("/Users/findingjemo/Documents/PSA/PH/Vcomparison.csv")

v_data <- v_data %>%
  rename(Species = species,
         National_V = `PhV`,
         Global_V = `GV`)

#2. Calculate difference between national & global V scores
v_data <- v_data %>%
  mutate(Change = National_V - Global_V,
         Direction = case_when(
           Change > 0 ~ "Increase",
           Change < 0 ~ "Decrease",
           TRUE ~ "No Change"
         ))

#3. Employ statistical tests
####Wilcoxon signed-rank test (non-parametric paired test)####
wilcox_result <- wilcox.test(v_data$National_V, v_data$Global_V, paired = TRUE)
print("Wilcoxon Signed-Rank Test:")
print(wilcox_result)

#Wilcoxon signed rank test with continuity correction
#data:  v_data$National_V and v_data$Global_V
#V = 3632, p-value = 1.92e-05
#alternative hypothesis: true location shift is not equal to 0

#####Cohen's d (effect size)####
cohen_result <- cohen.d(v_data$National_V, v_data$Global_V, paired = TRUE)
print("Cohen's d Effect Size:")
print(cohen_result)

#Cohen's d
#d estimate: 0.3652921 (small)
#95 percent confidence interval:
#    lower     upper 
#0.1991273 0.5314568 

####Spearman correlation####
cor_result <- cor(v_data$National_V, v_data$Global_V, method = "spearman")
cat("Spearman Correlation:", cor_result, "\n")
#Spearman Correlation: 0.6482917 

####Summary of changes###
cat("Summary of Directional Change:\n")
print(table(v_data$Direction))

#Decrease  Increase No Change 
#30        68         5 

cat("Summary Stats of Score Change:\n")
print(summary(v_data$Change))

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-0.5100 -0.0200  0.0800  0.1015  0.2300  0.9900 

#4. Visually inspect results
###Histogram of Differences###
ggplot(v_data, aes(x = Change)) +
  geom_histogram(binwidth = 0.1, fill = "black", color = "white") +
  labs(title = "Distribution of Vulnerability Score Differences",
       x = "National V - Global V", y = "Number of Species") +
  theme_minimal()

####Scatterplot with 1:1 line###
ggplot(v_data, aes(x = Global_V, y = National_V)) +
  geom_point(color = "black") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "National vs Global Vulnerability Scores",
       x = "Global V", y = "National V") +
  theme_minimal()


####Horizontal bar plot of changes####
v_data <- v_data %>% arrange(desc(Change))
v_data$Species <- factor(v_data$Species, levels = v_data$Species)

ggplot(v_data, aes(x = Change, y = Species)) +
  geom_col(fill = "black") +
  labs(title = "Change in Vulnerability Scores (Global to National)",
       x = "Change in V Score", y = "Species") +
  theme_minimal()

ggsave("/Users/findingjemo/Documents/PSA/PH/vulnerability_changes_barplot.png", width = 8, height = 20, dpi = 300)

