library(tidyverse)
library(ggplot2)

data_url <- "https://raw.githubusercontent.com/12rwjcbsdmc/hw4/refs/heads/main/gym_members_exercise_tracking_synthetic_data.csv"
gym_data <- read.csv(data_url)
str(gym_data)
summary(gym_data)

gym_data <- na.omit(gym_data)
num_cols <- gym_data |> 
  select_if(is.numeric) |>
  names()
gym_data_scaled <- gym_data
gym_data_scaled[num_cols] <- scale(gym_data[num_cols])

pca <- prcomp(gym_data_scaled[num_cols], center = TRUE, scale. = TRUE)
pca_data <- data.frame(pca$x[, 1:2])
colnames(pca_data) <- c("PC1", "PC2")

explained_variance <- summary(pca)$importance[2, ]
components <- 1:length(explained_variance)
explained_variance_df <- data.frame(
  Component = components,
  Variance = explained_variance
)

scree_plot <- ggplot(explained_variance_df, aes(x = Component, y = Variance)) +
  geom_bar(stat = "identity", fill = "gray") +
  theme_minimal() +
  labs(
    title = "Scree Plot of PCA Components",
    x = "Principal Component",
    y = "Explained Variance"
  )
print(scree_plot)

loadings <- as.data.frame(pca$rotation[, 1:2])
loadings$Feature <- rownames(pca$rotation)

pca_loadings_plot <- ggplot(loadings, aes(x = PC1, y = PC2, label = Feature)) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "blue", size = 0.8) +
  geom_text(size = 4, hjust = 1.2) +
  theme_minimal() +
  labs(
    title = "PCA Loadings: Feature Contributions",
    x = "Principal Component 1",
    y = "Principal Component 2"
  )
print(pca_loadings_plot)

