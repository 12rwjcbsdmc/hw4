---
  title: "hw4"
output: html_document
date: "2024-12-06"
---
  
```{r}
data_url <- "https://raw.githubusercontent.com/12rwjcbsdmc/hw4/refs/heads/main/gym_members_exercise_tracking%202.csv"
library(shiny)
library(ggplot2)
library(randomForest)
library(cluster)
library(factoextra)
gym_data <- read.csv(data_url)
gym_data <- na.omit(gym_data)

ui <- fluidPage(
  titlePanel("Random Forest Model and PCA Visualization - Gym Data"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Data Exploration"),
      selectInput("x_var", "Select X-axis Variable:", choices = names(gym_data)),
      selectInput("y_var", "Select Y-axis Variable:", choices = names(gym_data)),
      selectInput("color_var", "Select Color Variable:", choices = names(gym_data), selected = names(gym_data)[1]),
      
      hr(),
      
      h4("Model Parameters"),
      sliderInput("ntree", "Number of Trees:", min = 10, max = 200, value = 100),
      sliderInput("mtry", "Number of Variables Tried at Each Split:", min = 2, max = 10, value = 3)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Exploration", plotOutput("data_plot")),
        tabPanel("Model Visualization", 
                 h4("Feature Importance"),
                 plotOutput("feature_importance"),
                 h4("Model Accuracy"),
                 verbatimTextOutput("model_accuracy")),
        tabPanel("PCA and Clustering", 
                 h4("PCA Plot"),
                 plotOutput("pca_plot"),
                 h4("Biplot"),
                 plotOutput("bi_plot"),
                 h4("Clustering Summary"),
                 verbatimTextOutput("cluster_summary"))
      )
    )
  )
)

server <- function(input, output, session) {
  output$data_plot <- renderPlot({
    input$plot_data 
    ggplot(gym_data, aes_string(x = input$x_var, y = input$y_var, color = input$color_var)) +
      geom_point(alpha = 0.7) +
      theme_minimal() +
      labs(title = "Data Exploration", x = input$x_var, y = input$y_var, color = input$color_var)
  })
  
  model <- reactive({
    rf <- randomForest(Calories_Burned ~ ., data = gym_data, 
                       ntree = input$ntree, mtry = input$mtry)
    return(rf)
  })
  output$feature_importance <- renderPlot({
    rf <- model()
    importance <- data.frame(Feature = rownames(importance(rf)), 
                             Importance = importance(rf)[, 1])
    ggplot(importance, aes(x = reorder(Feature, Importance), y = Importance, fill = Importance)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      scale_fill_gradient(low = "blue", high = "red") +
      labs(title = "Feature Importance", x = "Feature", y = "Importance")
  })
  
  output$model_accuracy <- renderText({
    rf <- model()
    if (!is.null(rf$rsq)) {
      paste("Model R-Squared:", round(max(rf$rsq, na.rm = TRUE), 2))
    } else {
      "Unable to compute model R-squared."
    }
  })
  
  pca_result <- reactive({
    num_data <- gym_data[sapply(gym_data, is.numeric)]
    pca <- prcomp(num_data, scale. = TRUE)
    return(pca)
  })
  
  cluster_result <- reactive({
    pca <- pca_result()
    kmeans_res <- kmeans(pca$x[, 1:3], centers = 3, nstart = 25)
    return(kmeans_res)
  })
  output$pca_plot <- renderPlot({
    pca <- pca_result()
    clusters <- cluster_result()
    fviz_cluster(clusters, data = as.data.frame(pca$x[, 1:3]),
                 geom = "point", ellipse.type = "convex",
                 ggtheme = theme_minimal(),
                 main = "PCA Cluster Plot")
  })
  
  output$bi_plot <- renderPlot({
    pca <- pca_result()
    fviz_pca_var(pca, axes = c(1, 2), repel = TRUE, 
                 col.var = "steelblue", 
                 select.var = list(contrib = 10), 
                 arrow.size = 1.0, 
                 ggtheme = theme_minimal(),
                 title = "PCA Biplot - Optimized Arrows")
  })
  
  output$cluster_summary <- renderText({
    clusters <- cluster_result()
    paste("Cluster sizes:", paste(clusters$size, collapse = ", "))
  })
}

shinyApp(ui = ui, server = server)

```