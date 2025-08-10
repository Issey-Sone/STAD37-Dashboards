library(shiny)
library(shinythemes)
library(MASS)
library(ggplot2)
library(DT)
library(plotly)
library(factoextra)
library(corrplot)
library(mclust)


if (FALSE) library(munsell)


ui <- navbarPage("Clustering Dashboard",
  theme = shinytheme("flatly"),
  tabPanel(
    "Hierarchical Clustering Dashboard",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h4("Clustering Parameters"),
          selectInput("dataset", "Select Dataset: ",
                      choices = list(
                        "Iris Dataset" = "iris",
                        "Random Gaussian Clusters" = "random_gaussian"
                      ), selected = "iris"),
          selectInput("linkage", "Linkage Method: ",
                      choices = list(
                        "Complete" = "complete",
                        "Average" = "average",
                        "Single" = "single",
                        "Ward" = "ward"
                      ), selected = "complete"),
          helpText("Pay attention to how the height scale changes when choosing between different linkage methods."), 
          selectInput("distance", "Distance Method: ",
                      choices = list(
                        "Euclidean" = "euclidean",
                        "Manhattan" = "manhattan"
                      )),
          numericInput("n_clusters", "Number of clusters: ",
                       value = 3, min = 2, max = 10, step = 1),
          
          checkboxInput("scale_data", "Scale Data", value = TRUE),
          
          actionButton("run_clustering", "Run Clustering", class = 'btn-primary')
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Dendogram",
                     plotlyOutput("dendogram", height = "600px"),
                     fluidRow(
                       column(12,
                              h4("Interpretation"),
                              textOutput("interpretation1")
                       )
                     ),
                     br(),
                     br(),
                     br()
            ),
            tabPanel("Cluster Visualization",
                     plotlyOutput("cluster_plot", height = "600px"),
                     fluidRow(
                       column(12,
                              h4("Interpretation"),
                              textOutput("interpretation2")
                       )
                     ),
                     br(),
                     br(),
                     br()
            ),
            tabPanel("Correlation Matrix",
                     plotOutput("correlation_plot")
            )
          )
        )
      )
    )
  ),
  
  tabPanel(
    "Non-Hierarchical Clustering Methods",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h4("Clustering Parameters"),
          selectInput("dataset2", "Select Dataset: ",
                      choices = list(
                        "Iris Dataset" = "iris",
                        "Random Gaussian Clusters" = "random_gaussian"
                      ), selected = "iris"),
          selectInput("method", "Clustering Algorithm",
                      choices = list(
                        "K-means" = "kmeans",
                        "Gaussian Mixture Model" = "gmm"
                      )),
          numericInput("n_clusters_alt", "Number of clusters: ",
                       value = 3, min = 2, max = 10, step = 1),
          checkboxInput("scale_data_2", "Scale Data", value = TRUE),
          actionButton("run_clustering_2", "Run Clustering", class = 'btn-primary')
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Cluster Visualization",
                     plotlyOutput("cluster_plot_2", height = "600px"),
                     fluidRow(
                       column(12,
                              h4("Interpretation"),
                              textOutput("interpretation3"))
                     ),
                     br(),
                     br(),
                     br()
            )
          )
        )
      )
    )
  ),
  footer = tags$footer(
    style = "
      position: fixed;
      bottom: 0;
      left: 0;
      width: 100%;
      background-color: #f8f9fa;
      border-top: 1px solid #ddd;
      text-align: center;
      padding: 8px;
      font-size: 12px;
      color: #6c757d;
      z-index: 1000;
    ",
    "2025 Created by Shahriar Shams and Issey Sone"
  )
)


server <- function(input, output, session) {
  
  generate_sample_data <- function(dataset, n_samples = 250) {
    set.seed(123)
    sigma1 <- matrix(c(1, 0.2, 0.2, 1), ncol = 2)
    sigma2 <- matrix(c(1, -0.3, -0.3, 1), ncol = 2)
    sigma3 <- matrix(c(1, 0, 0, 1), ncol = 2)
    switch(dataset,
           "iris" = {
             data(iris)
             return(iris[, 1:4])
           },
           "random_gaussian" = {
             data.frame(
               x1 = c(rnorm(n_samples/3, mean = 2, sd = 0.5),
                      rnorm(n_samples/3, mean = 6, sd = 0.7),
                      rnorm(n_samples/3, mean = 4, sd = 0.4)),
               x2 = c(rnorm(n_samples/3, mean = 2, sd = 0.6),
                      rnorm(n_samples/3, mean = 6, sd = 0.5),
                      rnorm(n_samples/3, mean = 8, sd = 0.7)),
               x3 = c(rnorm(n_samples/3, mean = 1, sd = 0.3),
                      rnorm(n_samples/3, mean = 5, sd = 0.4),
                      rnorm(n_samples/3, mean = 3, sd = 0.5))
             )
           }
           )
  }
  data_reactive <- reactive({
    if(input$dataset == "iris") {
      generate_sample_data("iris")
    } else {
      n_samples <- 250
      generate_sample_data(input$dataset, n_samples)
    }
  })
  
  clustering_results <- eventReactive(input$run_clustering, {
    data <- data_reactive()
    
    if (input$scale_data) {
      data_scaled <- scale(data)
    } else {
      data_scaled <- data
    }
    
    dist_matrix <- dist(data_scaled, method = input$distance)
    
    hc <- hclust(dist_matrix, method = input$linkage)
    
    clusters <- cutree(hc, k = input$n_clusters)
    
    list(
      data = data,
      data_scaled = data_scaled,
      hc = hc,
      clusters = clusters,
      dist_matrix = dist_matrix
    )
    
  })
  
  output$dendogram <- renderPlotly({
    req(clustering_results())
    
    results <- clustering_results()
    
    p <- fviz_dend(results$hc, k = input$n_clusters,
                   cex = 0.5, k_colors = "jco",
                   color_labels_by_k = TRUE, rect = TRUE, rect_fill = TRUE, rect_border = "jco") +
      labs(title = "Hierarchical Clustering Dendogram") +
      theme_minimal()
    ggplotly(p, tooltip = 'text')
  })
  
  output$cluster_plot <- renderPlotly({
    req(clustering_results())
    results <- clustering_results()
    
    data_with_clusters <- cbind(results$data, Cluster = as.factor(results$clusters))
    
    if (ncol(results$data) > 2) {
      pca <- prcomp(results$data_scaled)
      plot_data <- data.frame(
        PC1 = pca$x[, 1],
        PC2 = pca$x[, 2],
        Cluster = as.factor(results$clusters)
      )
      
      p <- ggplot(plot_data, aes(x = PC1, y = PC2, color = Cluster)) +
        geom_point(size = 3, alpha = 0.7) +
        labs(title = "Clusters (PCA) Projection", x = "First Principal Component", y = "Second Principal Component") + 
        theme_minimal() +
        scale_color_brewer(palette = "Set1")
    } else {
      plot_data <- data.frame(
        x = results$data[, 1],
        y = results$data[, 2],
        Cluster = as.factor(results$clusters)
      )
      
      p <- ggplot(plot_data, aes(x = x, y = y, color = Cluster)) + 
        geom_point(size = 3, alpha = 0.7) +
        labs(tilte = "Clusters",
             x = names(results$data)[1],
             y = names(results$data)[2]) +
        theme_minimal() +
        scale_color_brewer(palette = "Set1")
    }
    
    ggplotly(p)
  })
  data_reactive_2 <- reactive({
    if(input$dataset2 == "iris") {
      generate_sample_data("iris")
    } else {
      n_samples <- 250
      generate_sample_data(input$dataset2, n_samples)
    }
  })
  
  output$correlation_plot <- renderPlot({
    data <- data_reactive_2()
    if (ncol(data) > 1) {
      cor_matrix <- cor(data)
      corrplot(cor_matrix, method = "color", type = "upper", order = "hclust")
    }
    
  })
  
  
  clustering_results_2 <- eventReactive(input$run_clustering_2, {
    data <- data_reactive_2()
    if(input$scale_data_2) {
      data_scaled <- scale(data)
    } else {
      data_scaled <- data
    }
    
    if (input$method == "kmeans") {
      model <- kmeans(data_scaled, centers = input$n_clusters_alt)
      clusters <- model$cluster
    } else if (input$method == "gmm") {
      model <- Mclust(data_scaled, G = input$n_clusters_alt)
      clusters <- model$classification
    }
    
    list(
      data = data,
      data_scaled = data_scaled,
      clusters = clusters
    )
    
  })
  
  output$cluster_plot_2 <- renderPlotly({
    req(clustering_results_2())
    results <- clustering_results_2()
    
    pca <- prcomp(results$data_scaled)
    
    plot_data <- data.frame(
      PC1 = pca$x[, 1],
      PC2 = pca$x[, 2],
      Cluster = as.factor(results$clusters)
    )
    
    p <- ggplot(plot_data, aes(x = PC1, y = PC2, color = Cluster)) + 
      geom_point(size = 3, alpha = 0.7) +
      labs(title = paste(input$method, "Clustering Projection"),
           x = "First Principal Component",
           y = "Second Principal Component") +
      theme_minimal() + 
      scale_color_brewer(palette = "Set1")
    
    ggplotly(p)
    
  })
  
  # Interpretations
  
  output$interpretation1 <- renderText({
    
    "In a dendogram look at the vertical lines (merge heights) to understand cluster similarity, shorter merges means clusters are combined with lower distance (higher similarity),
    tall merges means larger distance (lower similarity). Horizontal cluster widths are an indicator of how many points each branch contains wider clusters contain more points, whilst
    narrower clusters are smaller, tightly related groups. 
    The four linkage methods determine how distance between two clusters is measured when merging clusters. Pay attention to how each of these can change the dendogram.
    The number of clusters determines where the dendogram is
    'cut' higher number of clusters means you cut lower and capture earlier short merges, less clusters means you have taller merges and you cut higher."
  })
  
  output$interpretation2 <- renderText({
    "For the PCA projection, check if the clusters are well seperated or if they are overlapping. If clusters are overlapping heavily, then the seperation might be weak or could potentailly
    be merged. You can also check for the compactness of clusters,
    larger clusters could be broken down more or could have higher variance. Since PC1 explains the largest variance, clusters far apart along PC1 differ the most in
    features. "
  })
}




shinyApp(ui = ui, server = server)