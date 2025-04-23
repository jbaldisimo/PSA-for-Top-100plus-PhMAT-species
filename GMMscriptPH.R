# Gaussian Mixture Model Clustering Method for Productivity-Susceptibility Analysis of Top 103 Exported Marine Aquarium Fishes from the Philippines

# Install necessary packages
install.packages("dplyr")
install.packages("tidyverse")
install.packages("mclust")
install.packages("ggplot2")

# Load required libraries
library(mclust)
library(cluster)
library(ggplot2)
library(dplyr)

# 1. Read data
PSAdata <- read.csv("/Users/findingjemo/Documents/PSA/PH/PSVscoresPH.csv", stringsAsFactors = TRUE)

# 2. Select p, s, v
dataGMM <- select(PSAdata, p, s, v)
dim(dataGMM)

# 3. Test which model will be the best fit for enforcing 3 clusters:
find_best_GMM_silhouette <- function(data, max_clusters = 6, enforce_G = NULL) {
  # Step 1: Scale the data
  data_scaled <- scale(data)
  
  models <- c("EII", "VII", "EEE", "VVV", "VVI", "VEI")
  results <- data.frame()
  
  for (model in models) {
    for (g in 2:max_clusters) {
      try({
        fit <- Mclust(data_scaled, G = g, modelNames = model)
        sil <- silhouette(fit$classification, dist(data_scaled))
        mean_sil <- mean(sil[, "sil_width"])
        
        results <- rbind(results, data.frame(
          G = g,
          model = model,
          mean_silhouette = mean_sil
        ))
      }, silent = TRUE)
    }
  }
  
  # Print top combinations by silhouette
  best_overall <- results %>% arrange(desc(mean_silhouette)) %>% head(5)
  print("Top 5 clustering setups by mean silhouette:")
  print(best_overall)
  
  # Plot the silhouette scores
  ggplot(results, aes(x = G, y = mean_silhouette, color = model)) +
    geom_line() +
    geom_point() +
    labs(title = "Silhouette Scores for GMM Models", x = "Number of Clusters", y = "Mean Silhouette") +
    theme_minimal()
  
  # If enforcing a specific number of clusters (e.g., G = 3)
  if (!is.null(enforce_G)) {
    best_model_fixed_G <- results %>%
      filter(G == enforce_G) %>%
      arrange(desc(mean_silhouette)) %>%
      slice(1)
    
    message(paste0("\nBest model for G = ", enforce_G, ":"))
    print(best_model_fixed_G)
    
    # Refit using the selected model
    final_model <- Mclust(data_scaled, G = enforce_G, modelNames = best_model_fixed_G$model)
    sil <- silhouette(final_model$classification, dist(data_scaled))
    cat("\nFinal silhouette score for forced G =", enforce_G, ":", mean(sil[, "sil_width"]), "\n")
    
    return(list(model = final_model, silhouette = sil, summary = best_model_fixed_G, scaled_data = data_scaled))
  }
  
  return(results)
}


# 4. Run clustering and store results
result <- find_best_GMM_silhouette(dataGMM, max_clusters = 6, enforce_G = 3)

#"Top 5 clustering setups by mean silhouette:"
#G model mean_silhouette
#1 3   EII       0.4435771
#2 3   VEI       0.4363097
#3 3   VVI       0.4221531
#4 2   EII       0.4163910
#5 2   VVI       0.4132399

#Best model for G = 3:
#  G model mean_silhouette
#1 3   EII       0.4435771

#Final silhouette score for forced G = 3 : 0.4435771

# 5. Add cluster classification to original data
PSAdata$cluster <- factor(result$model$classification)

# 6. Unscale cluster centroids to original values
means <- attr(result$scaled_data, "scaled:center")
sds <- attr(result$scaled_data, "scaled:scale")
scaled_means <- result$model$parameters$mean

centroids_df <- data.frame(
  cluster = 1:3,
  p = scaled_means[1, ] * sds["p"] + means["p"],
  s = scaled_means[2, ] * sds["s"] + means["s"],
  v = scaled_means[3, ] * sds["v"] + means["v"]
)

# 7. Sort centroids by vulnerability and label them
centroids_df <- centroids_df %>%
  arrange(desc(v)) %>%
  mutate(label = c("Highly Vulnerable", "Moderately Vulnerable", "Least Vulnerable"))

print(centroids_df)

# 8. Reassign cluster numbers by vulnerability level
label_lookup <- centroids_df %>%
  mutate(original_cluster = cluster) %>%
  select(original_cluster, label)

# 9. Merge labels back to PSAdata
PSAdata <- PSAdata %>%
  mutate(original_cluster = result$model$classification) %>%
  left_join(label_lookup, by = "original_cluster") %>%
  select(-original_cluster)

# 10. View summary tables
table(PSAdata$label)

# 11. Export table with species and assigned cluster
write.csv(PSAdata, "/Users/findingjemo/Documents/PSA/PH/PSAresults.csv", row.names = FALSE)

# 12. Visualize data
mean_sil <- round(mean(result$silhouette[, "sil_width"]), 3)

# Reorder factor levels for correct legend order
PSAdata$label <- factor(PSAdata$label, levels = c("Highly Vulnerable", "Moderately Vulnerable", "Least Vulnerable"))

ggplot(PSAdata, aes(x = p, y = s, shape = label)) +
  geom_point(size = 2, alpha = 0.7) +
  stat_ellipse(aes(group = label), linetype = "dashed", size = 1, alpha = 0.2) +
  scale_x_reverse(limits = c(3.0, 1)) +
  scale_y_continuous(limits = c(1.20, 2.25)) +
  labs(
    title = paste("GMM Clustering for PSA (Silhouette coefficient =", 
                  round(mean(result$silhouette[, "sil_width"]), 2), ")"),
    x = "Productivity",
    y = "Susceptibility",
    shape = "Category"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.key.size = unit(0.6, "lines"),
        legend.title = element_text(size = 9),
        legend.position = "right")
