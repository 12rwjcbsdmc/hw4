---
  title: "hw4"
output: html_document
date: "2024-12-07"
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(ggplot2)
library(factoextra)

data_url <- "https://raw.githubusercontent.com/12rwjcbsdmc/hw4/refs/heads/main/gym_members_exercise_tracking_synthetic_data.csv"
data <- read.csv(data_url)

head(data)

numeric_data <- data[sapply(data, is.numeric)]

numeric_data[!is.finite(as.matrix(numeric_data))] <- NA
numeric_data <- na.omit(numeric_data)

pca_result <- prcomp(numeric_data, scale. = TRUE)

set.seed(123)
clusters <- kmeans(pca_result$x[, 1:2], centers = 3)$cluster

data_with_clusters <- data.frame(PC1 = pca_result$x[, 1],
                                 PC2 = pca_result$x[, 2],
                                 Cluster = as.factor(clusters))

scatter_plot <- ggplot(data_with_clusters, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 2) +
  labs(title = "Clusters of Gym Members Based on Exercise Preferences",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Cluster") +
  theme_minimal()

print(scatter_plot)

loadings <- as.data.frame(pca$rotation[, 1:2])
loadings$Feature <- rownames(pca$rotation)

pca_loadings_plot <- ggplot(loadings, aes(x = PC1, y = PC2, label = Feature)) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "blue", size = 0.8) +
  geom_text(size = 4, hjust = 1.2) +
  labs(title = "Biplot of PCA Loadings and Clusters",
       x = "Principal Component 1 (Intensity and Energy Expenditure Differences)",
       y = "Principal Component 2 (Body Traits and Heart Rate Variations)") +
  theme_minimal()
print(pca_loadings_plot)

loadings <- pca_result$rotation[, 1:2]
print(loadings)

```