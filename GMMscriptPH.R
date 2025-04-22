# Load libraries
install.packages("plotly")
install.packages("cluster")
install.packages("tidyverse")
install.packages("mclust")
install.packages("RColorBrewer")

library(ggplot2)
library(plotly)
library(cluster)
library(tidyverse)
library(mclust)
library(grid)
library(RColorBrewer)

# Set seed for reproducibility
set.seed(123)

# Read in data
PSAdata <- read.csv("/Users/findingjemo/Documents/PSA/PH/PSVscores.csv", stringsAsFactors = TRUE)

# Select relevant columns
dataGMM <- select(PSAdata, p, s, v)

# Run GMM with 3 clusters
GMM6 <- Mclust(dataGMM, modelNames = "VVI", G = 3)

# Assign cluster classification
PSAdata$cluster <- factor(GMM6$classification)

# Define custom cluster labels
cluster_labels <- c("Least Vulnerable", "Moderately Vulnerable", "Most Vulnerable")
PSAdata$cluster <- factor(GMM6$classification,
                          levels = 1:3,
                          labels = cluster_labels)

# Create centroids data
centroids <- data.frame(
  p = GMM6$parameters$mean[1, ],
  s = GMM6$parameters$mean[2, ],
  v = GMM6$parameters$mean[3, ],
  cluster = factor(1:3, labels = cluster_labels)
)

# Calculate silhouette score
silhouette_coef <- silhouette(GMM6$classification, dist(dataGMM[, c("p", "s", "v")]))
mean_silhouette <- mean(silhouette_coef[, "sil_width"])

# Optional: background gradient
make_gradient <- function(deg = 45, n = 100, cols = blues9) {
  cols <- colorRampPalette(cols)(n + 1)
  rad <- deg / (180 / pi)
  mat <- matrix(
    data = rep(seq(0, 2.8, length.out = n) * cos(rad), n),
    byrow = TRUE,
    ncol = n
  ) +
    matrix(
      data = rep(seq(0, 2.8, length.out = n) * sin(rad), n),
      byrow = FALSE,
      ncol = n
    )
  mat <- mat - min(mat)
  mat <- mat / max(mat)
  mat <- 1 + mat * n
  mat <- matrix(data = cols[round(mat)], ncol = n)
  grid::rasterGrob(
    image = mat,
    width = unit(1, "npc"),
    height = unit(1, "npc"), 
    interpolate = TRUE
  )
}

# Create plot (no ellipse labels)
gg <- ggplot(PSAdata, aes(x = p, y = s, shape = cluster, fill = factor(cluster))) +
  geom_point(size = 4, color = "black", stroke = 1, alpha = 0.8) +  # black outline
  stat_ellipse(aes(group = cluster),
               level = 0.95,
               size = 0.7,
               linetype = "dashed") +
  labs(title = paste("Productivity vs. Susceptibility with GMM Clustering\nMean Silhouette Coefficient:", 
                     round(mean_silhouette, 2)),
       x = "Productivity",
       y = "Susceptibility",
       shape = "Cluster",
       fill = "Cluster") +
  theme_minimal() +
  scale_shape_manual(values = c(16, 22, 17)) +  # circle, square, triangle
  scale_fill_manual(values = c("white", "white", "white")) +  # matching fill order: circle, square, triangle
  scale_x_reverse()

# Display
final <- gg
final

write.csv(PSAdata, "/Users/findingjemo/Documents/PSA/PH/PSAdatawithclusters.csv", row.names = FALSE)


