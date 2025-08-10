library(shiny)
library(MASS)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(scales)
library(ellipse)

if (FALSE) library(munsell)

ui <- navbarPage("Multivariate Normality Assumption Dashboard",
  theme = shinytheme("flatly"),
  sidebarLayout(
    sidebarPanel(
      selectInput("distribution", "Select Distribution:",
                  choices = list("Normal" = "normal",
                                 "Uniform" = "uniform", 
                                 "Exponential" = "exponential"),
                  selected = "normal"),
      
      sliderInput("sample_size", "Sample Size:", 
                  value = 500, min = 50, max = 1000, step = 50),
      
      br(), br(),
      
      h4("Summary Statistics"),
      h5("Sample Size"),
      textOutput("sample_size_display"),
      br(),
      h5("Mean Vector"),
      verbatimTextOutput("mean_vector"),
      br(),
      h5("Covariance Matrix"),
      verbatimTextOutput("covariance_matrix"),
      br(),
      h5("Contour Output"),
      tableOutput("contour_table")
    ),
    
    mainPanel(
      fluidRow(
        column(6,
               h4("Contour Plot"),
               plotOutput("contour_plot", height = "400px")
        ),
        column(6,
               h4("Q-Q Plot"),
               plotOutput("qq_plot", height = "400px")
        )
      ),
      
      br(),
      
      fluidRow(
        column(12,
               h4("Interpretation"),
               textOutput("interpretation")
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
  
  contour_level <- 0.5
  
  data_reactive <- reactive({
    n <- input$sample_size
    
    if (input$distribution == "normal") {
      mu <- c(0, 0)
      Sigma <- matrix(c(3, 1, 1, 3), nrow = 2)
      X <- as.data.frame(mvrnorm(n = n, mu = mu, Sigma = Sigma))
      
    } else {
      if (input$distribution == "uniform") {
        X <- data.frame(
          x1 = runif(n, -3, 3),
          x2 = runif(n, -3, 3)
        )
      } else if (input$distribution == "exponential") {
        X <- data.frame(
          x1 = rexp(n, rate = 0.5),
          x2 = rexp(n, rate = 0.5)
        )
      }
      
      # Whitening and transformation to target covariance
      X_centered <- scale(X, center = TRUE, scale = FALSE)
      eig <- eigen(cov(X_centered))
      D_inv_sqrt <- diag(1 / sqrt(eig$values))
      X_whitened <- as.matrix(X_centered) %*% eig$vectors %*% D_inv_sqrt %*% t(eig$vectors)
      
      Sigma_target <- matrix(c(3, 1, 1, 3), nrow = 2)
      eig_target <- eigen(Sigma_target)
      D_target_sqrt <- diag(sqrt(eig_target$values))
      X_transformed <- X_whitened %*% eig_target$vectors %*% D_target_sqrt %*% t(eig_target$vectors)
      
      X <- as.data.frame(X_transformed)
    }
    
    colnames(X) <- c("x1", "x2")
    mu_hat <- colMeans(X)
    Sigma_hat <- cov(X)
    X$mahal_sq <- mahalanobis(X, center = mu_hat, cov = Sigma_hat)
    
    cutoff <- qchisq(contour_level, df = 2)
    X$inside <- X$mahal_sq <= cutoff
    
    ellipse_data <- as.data.frame(ellipse(Sigma_hat, centre = mu_hat, level = contour_level))
    colnames(ellipse_data) <- c("x1", "x2")
    
    list(
      data = X,
      ellipse = ellipse_data,
      mu_hat = mu_hat,
      Sigma_hat = Sigma_hat,
      distribution = input$distribution
    )
  })
  
  output$contour_plot <- renderPlot({
    data_list <- data_reactive()
    X <- data_list$data
    ellipse_data <- data_list$ellipse
    dist_name <- tools::toTitleCase(data_list$distribution)
    
    colors <- switch(data_list$distribution,
                     "normal" = c("FALSE" = "darkblue", "TRUE" = "skyblue"),
                     "uniform" = c("FALSE" = "darkorchid", "TRUE" = "orchid1"),
                     "exponential" = c("FALSE" = "darkgreen", "TRUE" = "lightgreen"))
    
    ggplot() +
      geom_point(data = X, aes(x = x1, y = x2, color = inside), alpha = 0.7, size = 2) +
      geom_path(data = ellipse_data, aes(x = x1, y = x2), color = "black", size = 1.2) +
      scale_color_manual(values = colors,
                         labels = c(paste0("Outside ", contour_level*100, "%"), 
                                    paste0("Inside ", contour_level*100, "%")),
                         name = "Location") +
      coord_fixed() +
      labs(title = paste0(dist_name, " Distribution with ", contour_level*100, "% Contour"),
           subtitle = paste0("Proportion inside ellipse: ", round(mean(X$inside), 3)),
           x = "X1", y = "X2") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"),
            legend.position = "bottom")
  })
  
  output$qq_plot <- renderPlot({
    data_list <- data_reactive()
    X <- data_list$data
    sorted_mahal <- sort(X$mahal_sq)
    theoretical <- qchisq(ppoints(length(sorted_mahal)), df = 2)
    
    point_color <- switch(data_list$distribution,
                          "normal" = "darkblue",
                          "uniform" = "darkorchid",
                          "exponential" = "darkgreen")
    
    qqplot(theoretical, sorted_mahal,
           main = "Q-Q Plot: Mahalanobis Distances vs. Chi-Squared(2)",
           xlab = "Theoretical Quantiles (Chi-Squared)",
           ylab = "Sample Mahalanobis Distances Squared",
           pch = 16, col = point_color, cex = 0.8)
    abline(0, 1, col = "black", lwd = 2)
    grid()
  })
  
  output$sample_size_display <- renderText({
    paste("n =", input$sample_size)
  })
  
  output$mean_vector <- renderText({
    mean_vec <- round(data_reactive()$mu_hat, 4)
    paste0("μ̂ = (", paste(mean_vec, collapse = ", "), ")")
  })
  
  output$covariance_matrix <- renderText({
    cov_mat <- round(data_reactive()$Sigma_hat, 4)
    paste0("Σ̂ = \n",
           sprintf("    [%8.4f  %8.4f]\n", cov_mat[1,1], cov_mat[1,2]),
           sprintf("    [%8.4f  %8.4f]", cov_mat[2,1], cov_mat[2,2]))
  })
  
  output$contour_table <- renderTable({
    X <- data_reactive()$data
    data.frame(
      Metric = c("Observed Proportion Inside", "Expected Proportion"),
      Value = c(round(mean(X$inside), 4), contour_level)
    )
  }, striped = TRUE, hover = TRUE)
  
  output$interpretation <- renderText({
    X <- data_reactive()$data
    prop_inside <- mean(X$inside)
    expected <- contour_level
    
    if (data_reactive()$distribution == "normal") {
      paste("Normal distribution: The points should follow the elliptical contour closely, and the Q-Q plot should show points near the diagonal line. The observed proportion (", 
            round(prop_inside, 3), ") should be close to the expected (", expected, ").")
    } else {
      paste("Non-normal distribution: Notice how the points deviate from the expected pattern. The Q-Q plot shows systematic departures from the diagonal, and the observed proportion (", 
            round(prop_inside, 3), ") may differ from the expected (", expected, "). This indicates violation of the normality assumption.")
    }
  })
}

shinyApp(ui = ui, server = server)
