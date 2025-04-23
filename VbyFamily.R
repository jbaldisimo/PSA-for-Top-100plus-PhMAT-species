#Plot for Vulnerability Scores per Family

#1. Install & Load required libraries
install.packages("tidyverse")
install.packages("dplyr")
installed.packages("ggplot2")

library(tidyverse)
library(dplyr)
library(ggplot2)

#2. Read the CSV file
vdata <- read.csv("/Users/findingjemo/Documents/PSA/PH/FamilyV.csv", stringsAsFactors = FALSE)

#3. Summarize data by grouping by family & getting mean V, then order to show decreasing V
summary_stats <- vdata %>%
  group_by(Family) %>%
  summarise(
    mean_v = mean(Vulnerability, na.rm = TRUE),
    min_v = min(Vulnerability, na.rm = TRUE),
    max_v = max(Vulnerability, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_v)) %>%
  mutate(Family = factor(Family, levels = Family))

#4. Make bar plot w/ bar showing range, mean V shown as a line & V scores as dots

ggplot() +
  geom_rect(
    data = summary_stats,
    aes(
      xmin = min_v, xmax = max_v,
      ymin = as.numeric(Family) - 0.4,
      ymax = as.numeric(Family) + 0.4
    ),
    fill = "gray80", color = NA
  ) +
  geom_segment(
    data = summary_stats,
    aes(
      x = mean_v, xend = mean_v,
      y = as.numeric(Family) - 0.4,
      yend = as.numeric(Family) + 0.4
    ),
    color = "black", linewidth = 0.8
  ) +
  geom_point(
    data = vdata,
    aes(x = Vulnerability, y = as.numeric(factor(Family, levels = summary_stats$Family))),
    shape = 21, fill = "black", color = "black", size = 1, alpha = 0.5
  ) +
  scale_y_continuous(
    breaks = seq_along(summary_stats$Family),
    labels = summary_stats$Family,
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    title = "Vulnerability Scores by Family",
    x = "Vulnerability",
    y = "Family"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(hjust=0.5)
  )