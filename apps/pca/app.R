library(shiny)
library(tidyverse)
library(shinythemes)
library(DT)
library(gridExtra)


if (FALSE) library(munsell)

ui <- navbarPage("PCA Dashboard",
                 theme = shinytheme("flatly"),
                 tabPanel("2D PCA Visualization",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                h3("Variable Selection"),
                                selectInput("var1", 
                                            "Select X Variable:",
                                            choices = names(iris)[1:4],
                                            selected = "Sepal.Length"),
                                
                                selectInput("var2", 
                                            "Select Y Variable:",
                                            choices = names(iris)[1:4],
                                            selected = "Sepal.Width"),
                                helpText("Tip 1: 
                                  Try selecting Sepal.Width and Sepal.Length to explore an example where the first principal component (PC1) captures the least variance."),
                                
                                helpText("Tip 2:
                                  Try selecting Petal.Length and Petal.Width to explore an example where the first principal component (PC1) captures the most variance."),
                                br(),
                                h4("PCA Information"),
                                verbatimTextOutput("pca_summary"),
                                
                                br(),
                                h4("Variance Explained"),
                                verbatimTextOutput("variance_explained")
                              ),
                              
                              mainPanel(
                                plotOutput("scatter_plot", height = "600px"),
                                fluidRow(
                                  column(12,
                                         h4("Interpretation"),
                                         textOutput("pca_explanation")
                                  )
                                ),
                                br(),
                                br()
                              )
                            )
                          )
                 ),
                 tabPanel("PCA Biplot",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                h3("Full Dataset PCA"),
                                helpText("This biplot shows PCA performed on all 4 iris variables"),
                                br(),
                                h4("Variance Explained by Each PC"),
                                verbatimTextOutput("full_pca_variance"),
                                br(),
                                h4("Loadings (PC1 and PC2)"),
                                verbatimTextOutput("full_pca_loadings")
                              ),
                              mainPanel(
                                plotOutput("biplot", height = "600px"),
                                fluidRow(
                                  column(12,
                                         h4("Interpretation"),
                                         textOutput("Biplot_explanation")
                                  )
                                ),
                                br(),
                                br()
                              )
                            )
                          )
                 ),
                 tabPanel("Scree Plot",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                h3("Scree Plot Analysis"),
                                helpText("The scree plot shows the variance explained by each principal component. Look for the 'elbow' point where the curve levels off."),
                                br(),
                                h4("Variance Summary"),
                                verbatimTextOutput("scree_variance_summary"),
                                br(),
                              ),
                              mainPanel(
                                plotOutput("scree_plot", height = "600px"),
                                fluidRow(
                                  column(12,
                                         h4("Interpretation"),
                                         textOutput("scree_explanation")
                                  )
                                ),
                                br(),
                                br()
                                
                              )
                            )
                          )
                 ),
                 tabPanel("Reconstruction & Information Loss",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons("dataset", "Dataset: ",
                                  choices = list(
                                    "IRIS Data - 4 Variables" = "iris_dataset",
                                    "Diamonds Dataset - 7 Variables" = "diamonds_dataset"
                                  ),
                                  selected = "iris_dataset"),
                                h3("Data Reconstruction"),
                                helpText("See how well the original data can be reconstructed using different numbers of principal components."),
                                br(),
                                sliderInput("n_components",
                                            "Number of Principal Components to Use:",
                                            min = 1, max = 4, value = 4, step = 1),
                                br(),
                                h4("Reconstruction Quality"),
                                verbatimTextOutput("reconstruction_quality"),
                                br(),
                                h4("Information Loss"),
                                verbatimTextOutput("information_loss"),
                                br(),
                              ),
                              mainPanel(
                                plotOutput("reconstruction_plots", height = "800px"),
                                fluidRow(
                                  column(12,
                                         h4("Interpretation"),
                                         textOutput("information_loss_explanation")
                                  )
                                ),
                                br(),
                                br()
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
  data("diamonds")
  set.seed(123)
  numeric_diamonds <- diamonds[sapply(diamonds, is.numeric)]
  numeric_diamonds <- numeric_diamonds[sample(nrow(numeric_diamonds), 2000), ]
  rownames(numeric_diamonds) <- NULL
  
  
  
  selected_data <- reactive({
    data.frame(
      x = iris[[input$var1]],
      y = iris[[input$var2]],
      Species = iris$Species
    )
  })
  
  
  current_dataset <- reactive({
    if(input$dataset == "iris_dataset") {
      list(
        data = iris[, 1:4],
        name = "IRIS",
        n_vars = 4,
        var_names = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
      )
    } else {
      list(
        data = numeric_diamonds,
        name = "Diamonds",
        n_vars = ncol(numeric_diamonds),
        var_names = names(numeric_diamonds)
      )
    }
  })
  
  
  observe({
    dataset_info <- current_dataset()
    updateSliderInput(session, "n_components",
                      max = dataset_info$n_vars,
                      value = min(input$n_components, dataset_info$n_vars))
  })
  
  
  
  pca_result <- reactive({
    data_matrix <- selected_data()[, c("x", "y")]
    prcomp(data_matrix, center = TRUE, scale. = TRUE)
  })
  
  full_pca_result <- reactive({
    data_matrix <- iris[, 1:4]  # All 4 numeric variables
    prcomp(data_matrix, center = TRUE, scale. = TRUE)
  })
  
  current_pca_result <- reactive({
    dataset_info <- current_dataset()
    data_matrix <- dataset_info$data
    prcomp(data_matrix, center = TRUE, scale. = TRUE)
  })
  
  output$pca_summary <- renderText({
    pca <- pca_result()
    paste("Principal Component 1 (PC1):\n",
          sprintf("%.3f * %s + %.3f * %s\n\n", 
                  pca$rotation[1,1], input$var1,
                  pca$rotation[2,1], input$var2),
          "Principal Component 2 (PC2):\n",
          sprintf("%.3f * %s + %.3f * %s", 
                  pca$rotation[1,2], input$var1,
                  pca$rotation[2,2], input$var2))
  })
  
  output$variance_explained <- renderText({
    pca <- pca_result()
    var_exp <- summary(pca)$importance[2,] * 100
    paste("PC1 explains:", sprintf("%.1f%%", var_exp[1]), "of variance\n",
          "PC2 explains:", sprintf("%.1f%%", var_exp[2]), "of variance\n",
          "Total explained:", sprintf("%.1f%%", sum(var_exp)))
  })
  
  output$full_pca_variance <- renderText({
    pca <- full_pca_result()
    var_exp <- summary(pca)$importance[2,] * 100
    paste("PC1:", sprintf("%.1f%%", var_exp[1]), "\n",
          "PC2:", sprintf("%.1f%%", var_exp[2]), "\n",
          "PC3:", sprintf("%.1f%%", var_exp[3]), "\n",
          "PC4:", sprintf("%.1f%%", var_exp[4]), "\n",
          "Cumulative (PC1+PC2):", sprintf("%.1f%%", sum(var_exp[1:2])))
  })
  
  output$scree_variance_summary <- renderText({
    pca <- full_pca_result()
    var_exp <- summary(pca)$importance[2,] * 100
    cum_var <- cumsum(var_exp)
    
    paste("Individual Variance:\n",
          sprintf("PC1: %.1f%%\n", var_exp[1]),
          sprintf("PC2: %.1f%%\n", var_exp[2]),
          sprintf("PC3: %.1f%%\n", var_exp[3]),
          sprintf("PC4: %.1f%%\n\n", var_exp[4]),
          "Cumulative Variance:\n",
          sprintf("PC1: %.1f%%\n", cum_var[1]),
          sprintf("PC1-PC2: %.1f%%\n", cum_var[2]),
          sprintf("PC1-PC3: %.1f%%\n", cum_var[3]),
          sprintf("PC1-PC4: %.1f%%", cum_var[4]))
  })
  
  output$full_pca_loadings <- renderText({
    pca <- full_pca_result()
    loadings <- pca$rotation[, 1:2]
    
    paste("PC1 Loadings:\n",
          sprintf("Sepal.Length: %.3f\n", loadings[1,1]),
          sprintf("Sepal.Width:  %.3f\n", loadings[2,1]),
          sprintf("Petal.Length: %.3f\n", loadings[3,1]),
          sprintf("Petal.Width:  %.3f\n\n", loadings[4,1]),
          "PC2 Loadings:\n",
          sprintf("Sepal.Length: %.3f\n", loadings[1,2]),
          sprintf("Sepal.Width:  %.3f\n", loadings[2,2]),
          sprintf("Petal.Length: %.3f\n", loadings[3,2]),
          sprintf("Petal.Width:  %.3f", loadings[4,2]))
  })
  
  output$scatter_plot <- renderPlot({
    data <- selected_data()
    pca <- pca_result()
    
    center_x <- mean(data$x)
    center_y <- mean(data$y)
    
    pc1_dir <- pca$rotation[,1]
    pc2_dir <- pca$rotation[,2]
    
    pc1_sd <- pca$sdev[1]
    pc2_sd <- pca$sdev[2]
    
    scale_factor <- min(sd(data$x), sd(data$y)) * 2
    
    pc1_end_x <- center_x + pc1_dir[1] * pc1_sd * scale_factor
    pc1_end_y <- center_y + pc1_dir[2] * pc1_sd * scale_factor
    pc1_start_x <- center_x - pc1_dir[1] * pc1_sd * scale_factor
    pc1_start_y <- center_y - pc1_dir[2] * pc1_sd * scale_factor
    
    pc2_end_x <- center_x + pc2_dir[1] * pc2_sd * scale_factor
    pc2_end_y <- center_y + pc2_dir[2] * pc2_sd * scale_factor
    pc2_start_x <- center_x - pc2_dir[1] * pc2_sd * scale_factor
    pc2_start_y <- center_y - pc2_dir[2] * pc2_sd * scale_factor
    
    p <- ggplot(data, aes(x = x, y = y, color = Species)) +
      geom_point(size = 3, alpha = 0.7) +
      
      geom_segment(aes(x = pc1_start_x, y = pc1_start_y, 
                       xend = pc1_end_x, yend = pc1_end_y),
                   color = "red", size = 1.0, alpha = 0.8,
                   arrow = arrow(length = unit(0.3, "cm"), ends = "both")) +
      
      geom_segment(aes(x = pc2_start_x, y = pc2_start_y, 
                       xend = pc2_end_x, yend = pc2_end_y),
                   color = "blue", size = 1.0, alpha = 0.8,
                   arrow = arrow(length = unit(0.3, "cm"), ends = "both")) +
      
      geom_point(aes(x = center_x, y = center_y), 
                 color = "black", size = 2, shape = 3) +
      
      labs(title = paste("PCA Analysis:", input$var1, "vs", input$var2),
           subtitle = "Red: PC1 (Major Axis), Blue: PC2 (Minor Axis)",
           x = input$var1,
           y = input$var2) +
      
      theme_minimal() +
      theme(plot.title = element_text(size = 16, hjust = 0.5),
            plot.subtitle = element_text(size = 12, hjust = 0.5),
            legend.position = "bottom") +
      
      scale_color_manual(values = c("setosa" = "#E69F00", 
                                    "versicolor" = "#56B4E9", 
                                    "virginica" = "#009E73"))
    
    print(p)
  })
  
  # Interpretations
  
  output$pca_explanation <- renderText({
    "PCA is only being performed on two variables here. The red line (PC1) shows the direction of maximum variance in the data, while the blue line (PC2) is orthagonal to PC1 and
    represents the second highest variance direction. Projecting the data onto those axes transforms the variances into uncorrelated
    components ranked by how much variance they capture. Each principal component is a linear combination of the original variables, the coefficient is the loading value, which
    defines the rotation of the new axes. Unlike a regression line, the principal component axis does not aim to fit or pass through all data points,
    it identifies the direction in which the data has the greatest variance."
  })
  
  output$Biplot_explanation <- renderText({
    "Red arrow represents how each of the original variables contributes to the principal components.
    Petal.Length and Petal.Width point in the same direction and are relatively long so they are highly correlated
    and influence PC1 the most. Sepal.Width is negatively correlated and contributes more to PC2. Each arrow is drawn as
    (x, y) = (loading PC1 * k, loading PC2 * k) where k is a scaling factor."
  })
  
  output$scree_explanation <- renderText({
    "The Scree plot shows how much individual variance is contributed by each principal component. Typically you want to look for the elbow point which
    is where the plot shows a sharp drop followed by a flattened shape. In our example its PC2."
  })
  
  output$information_loss_explanation <- renderText({
    "Each scatter plot compared the original variable values (x-axis) to teh values reconstructed from the selected number of PCs (y-axis). Points closer to the red diagonal line
    indicate better reconstruction accuracy. Using more prinicpal componetns captures more of the dataset's variance which improves the reconstruction. High R^2 values and low RMSE values
    indicate good reconstruction quality for that particular variable. The % of variance retained in the amount of variance retained in the lower-dimensional representation."
  })
  
  output$scree_plot <- renderPlot({
    pca <- full_pca_result()
    
    var_exp <- summary(pca)$importance[2,] * 100
    cum_var <- cumsum(var_exp)
    
    scree_data <- data.frame(
      PC = factor(1:4, labels = paste0("PC", 1:4)),
      Variance = var_exp,
      Cumulative = cum_var
    )
    
    p1 <- ggplot(scree_data, aes(x = PC, y = Variance, group = 1)) +
      geom_line(size = 1.2, color = "skyblue") +
      geom_point(size = 4, color = "skyblue") +
      labs(title = "Scree Plot - Variance Explained by Each PC",
           x = "Principal Component",
           y = "Variance Explained (%)") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, hjust = 0.5),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12)) +
      ylim(0, max(var_exp) + 5)
    
    p2 <- ggplot(scree_data, aes(x = PC, y = Cumulative, group = 1)) +
      geom_line(size = 1.2, color = "red") +
      geom_point(size = 4, color = "red") +
      geom_hline(yintercept = 80, linetype = "dashed", color = "gray", alpha = 0.7) +
      geom_hline(yintercept = 95, linetype = "dashed", color = "gray", alpha = 0.7) +
      labs(title = "Cumulative Variance Explained",
           x = "Principal Component",
           y = "Cumulative Variance (%)") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, hjust = 0.5),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12)) +
      ylim(0, 100) +
      annotate("text", x = 3.5, y = 82, label = "80%", color = "gray", size = 3) +
      annotate("text", x = 3.5, y = 97, label = "95%", color = "gray", size = 3)
    
    grid.arrange(p1, p2, ncol = 1)
  })
  
  reconstructed_data <- reactive({
    pca <- full_pca_result()
    n_comp <- input$n_components
    
    scores <- pca$x[, 1:n_comp, drop = FALSE]
    loadings <- pca$rotation[, 1:n_comp, drop = FALSE]
    
    reconstructed_scaled <- scores %*% t(loadings)
    
    reconstructed <- scale(reconstructed_scaled, center = FALSE, scale = 1/pca$scale) + 
      matrix(rep(pca$center, each = nrow(reconstructed_scaled)), ncol = 4)
    
    colnames(reconstructed) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
    as.data.frame(reconstructed)
  })
  
  output$reconstruction_quality <- renderText({
    original <- iris[, 1:4]
    reconstructed <- reconstructed_data()
    n_comp <- input$n_components
    
    r_squared <- sapply(1:4, function(i) {
      cor(original[,i], reconstructed[,i])^2
    })
    
    rmse <- sapply(1:4, function(i) {
      sqrt(mean((original[,i] - reconstructed[,i])^2))
    })
    
    var_names <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
    
    paste("Using", n_comp, "component(s):\n\n",
          "R² Values:\n",
          paste(sprintf("%s: %.3f", var_names, r_squared), collapse = "\n"),
          "\n\nRMSE Values:\n",
          paste(sprintf("%s: %.3f", var_names, rmse), collapse = "\n"),
          "\n\nMean R²:", sprintf("%.3f", mean(r_squared)),
          "\nMean RMSE:", sprintf("%.3f", mean(rmse)))
  })
  
  output$information_loss <- renderText({
    pca <- full_pca_result()
    n_comp <- input$n_components
    
    var_exp <- summary(pca)$importance[2,] * 100
    retained_var <- sum(var_exp[1:n_comp])
    lost_var <- 100 - retained_var
    
    paste("Variance Analysis:\n",
          sprintf("Retained: %.1f%%\n", retained_var),
          sprintf("Lost: %.1f%%\n", lost_var),
          sprintf("Information Loss: %.1f%%", lost_var))
  })
  
  
  reconstructed_data <- reactive({
    dataset_info <- current_dataset()
    pca <- current_pca_result()
    n_comp <- input$n_components
    
    scores <- pca$x[, 1:n_comp, drop = FALSE]
    loadings <- pca$rotation[, 1:n_comp, drop = FALSE]
    
    reconstructed_scaled <- scores %*% t(loadings)
    
    reconstructed <- scale(reconstructed_scaled, center = FALSE, scale = 1/pca$scale) +
      matrix(rep(pca$center, each = nrow(reconstructed_scaled)), ncol = dataset_info$n_vars)
    
    colnames(reconstructed) <- dataset_info$var_names
    as.data.frame(reconstructed)
  })
  
  output$reconstruction_quality <- renderText({
    dataset_info <- current_dataset()
    original <- dataset_info$data
    reconstructed <- reconstructed_data()
    n_comp <- input$n_components
    n_vars <- dataset_info$n_vars
    
    
    
    r_squared <- numeric(n_vars)
    rmse <- numeric(n_vars)
    
    for (i in 1:n_vars) {
      orig_col <- as.numeric(original[[i]])
      recon_col <- as.numeric(reconstructed[[i]])
      
      if (all(is.finite(orig_col)) && all(is.finite(recon_col))) {
        r_squared[i] <- cor(orig_col, recon_col)^2
        rmse[i] <- sqrt(mean((orig_col - recon_col)^2))
      } else {
        r_squared[i] <- NA
        rmse[i] <- NA
      }
    }
    
    var_names <- dataset_info$var_names
    
    paste("Dataset:", dataset_info$name, "\n",
          "Using", n_comp, "component(s) out of", dataset_info$n_vars, "total:\n\n",
          "R² Values:\n",
          paste(sprintf("%s: %.3f", var_names, r_squared), collapse = "\n"),
          "\n\nRMSE Values:\n",
          paste(sprintf("%s: %.3f", var_names, rmse), collapse = "\n"),
          "\n\nMean R²:", sprintf("%.3f", mean(r_squared)),
          "\nMean RMSE:", sprintf("%.3f", mean(rmse)))
  })
  
  output$information_loss <- renderText({
    dataset_info <- current_dataset()
    pca <- current_pca_result()
    n_comp <- input$n_components
    
    var_exp <- summary(pca)$importance[2, ] * 100
    
    retained_var <- sum(var_exp[1:n_comp])
    lost_var <- 100 - retained_var
    
    paste("Variance Analysis for", dataset_info$name, "Dataset:\n",
          sprintf("Components used: %d out of %d\n", n_comp, dataset_info$n_vars),
          sprintf("Retained: %.1f%%\n", retained_var),
          sprintf("Information Loss: %.1f%%", lost_var))
    
    
  })
  
  output$reconstruction_plots <- renderPlot({
    dataset_info <- current_dataset()
  
    original <- dataset_info$data 
    reconstructed <- reconstructed_data()
    n_comp <- input$n_components
    
    var_names <- dataset_info$var_names
    n_vars <- dataset_info$n_vars
    
    plot_list <- list()
    
    n_cols <- ifelse(n_vars <= 4, 2, 3)
    n_rows <- ceiling(n_vars / n_cols)
    
    for(i in 1:n_vars) {
      
      original_vec <- as.numeric(original[[i]])
      reconstructed_vec <- as.numeric(reconstructed[[i]])
      
      r_squared <- cor(original_vec, reconstructed_vec)^2
      rmse <- sqrt(mean((original_vec - reconstructed_vec)^2))
      
     
      
      plot_data <- data.frame(
        Original = original_vec,
        Reconstructed = reconstructed_vec
      )
      
      if(dataset_info$name == "IRIS") {
        plot_data$Group <- iris$Species
        color_mapping <- c("setosa" = "#E69F00", "versicolor" = "#56B4E9", "virginica" = "#009E73")
      } else {
        plot_data$Group <- diamonds$cut[1:nrow(plot_data)]
        color_mapping <- c("Fair" = "#E69F00", "Good" = "#56B4E9", "Very Good" = "#009E73", 
                           "Premium" = "#CC79A7", "Ideal" = "#D55E00")
      }
      
      p <- ggplot(plot_data, aes(x = Original, y = Reconstructed, color = Group)) +
        geom_point(size = 1.5, alpha = 0.7) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
        geom_smooth(method = "lm", se = FALSE, color = "red", size = 0.8) +
        labs(title = paste0(var_names[i], " (", n_comp, " PC", ifelse(n_comp > 1, "s", ""), ")"),
             subtitle = sprintf("R² = %.3f, RMSE = %.3f", r_squared, rmse),
             x = "Original Values",
             y = "Reconstructed Values") +
        theme_minimal() +
        theme(plot.title = element_text(size = 12, hjust = 0.5),
              plot.subtitle = element_text(size = 10, hjust = 0.5),
              legend.position = "none") +
        scale_color_manual(values = color_mapping) +
        coord_fixed()
      
      plot_list[[i]] <- p
    }
    
    do.call(grid.arrange, c(plot_list, ncol = n_cols))
  })
  
  output$biplot <- renderPlot({
    pca <- full_pca_result()
    
    pca_scores <- data.frame(
      PC1 = pca$x[,1], 
      PC2 = pca$x[,2], 
      Species = iris$Species
    )
    
    loadings <- pca$rotation[, 1:2]
    
    arrow_scale <- 3
    
    p <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = Species)) +
      geom_point(size = 3, alpha = 0.7) +
      
      geom_segment(aes(x = 0, y = 0, 
                       xend = loadings[1,1] * arrow_scale, 
                       yend = loadings[1,2] * arrow_scale),
                   color = "red", size = 1,
                   arrow = arrow(length = unit(0.3, "cm"))) +
      
      geom_segment(aes(x = 0, y = 0, 
                       xend = loadings[2,1] * arrow_scale, 
                       yend = loadings[2,2] * arrow_scale),
                   color = "red", size = 1,
                   arrow = arrow(length = unit(0.3, "cm"))) +
      
      geom_segment(aes(x = 0, y = 0, 
                       xend = loadings[3,1] * arrow_scale, 
                       yend = loadings[3,2] * arrow_scale),
                   color = "red", size = 1,
                   arrow = arrow(length = unit(0.3, "cm"))) +
      
      geom_segment(aes(x = 0, y = 0, 
                       xend = loadings[4,1] * arrow_scale, 
                       yend = loadings[4,2] * arrow_scale),
                   color = "red", size = 1,
                   arrow = arrow(length = unit(0.3, "cm"))) +
      
      geom_text(aes(x = loadings[1,1] * (arrow_scale + 0.5), 
                    y = loadings[1,2] * (arrow_scale + 0.5),
                    label = "Sepal.Length"),
                color = "red", size = 4, fontface = "bold") +
      
      geom_text(aes(x = loadings[2,1] * (arrow_scale + 0.5), 
                    y = loadings[2,2] * (arrow_scale + 0.5),
                    label = "Sepal.Width"),
                color = "red", size = 4, fontface = "bold") +
      
      geom_text(aes(x = loadings[3,1] * (arrow_scale + 0.5), 
                    y = loadings[3,2] * (arrow_scale + 0.5),
                    label = "Petal.Length"),
                color = "red", size = 4, fontface = "bold") +
      
      geom_text(aes(x = loadings[4,1] * (arrow_scale + 0.5), 
                    y = loadings[4,2] * (arrow_scale + 0.5),
                    label = "Petal.Width"),
                color = "red", size = 4, fontface = "bold") +
      
      labs(title = "PCA Biplot - Full Iris Dataset",
           subtitle = "All 4 variables shown as red arrows with loadings",
           x = paste0("PC1 (", sprintf("%.1f%%", summary(pca)$importance[2,1] * 100), " variance)"),
           y = paste0("PC2 (", sprintf("%.1f%%", summary(pca)$importance[2,2] * 100), " variance)")) +
      
      theme_minimal() +
      theme(plot.title = element_text(size = 16, hjust = 0.5),
            plot.subtitle = element_text(size = 12, hjust = 0.5),
            legend.position = "bottom") +
      
      scale_color_manual(values = c("setosa" = "#E69F00", 
                                    "versicolor" = "#56B4E9", 
                                    "virginica" = "#009E73")) +
      
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
      geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5)
    
    print(p)
  })
  
}

shinyApp(ui = ui, server = server)