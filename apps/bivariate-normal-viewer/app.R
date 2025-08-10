library(shiny)
library(shinythemes)
library(tidyverse)
library(mvtnorm)
library(plotly)
library(metR)

if (FALSE) library(munsell)

ui <- navbarPage(
  title = "STAD37 Bivariate Normal Visualizer",
  theme = shinytheme("flatly"),
  
  tabPanel("Interactive Viewer",
           sidebarLayout(
             sidebarPanel(
               h4("Mean Vector (μ):"),
               textInput("mu", "μ (e.g., 0, 0):", value = "0, 0"),
               
               
               h4("Covariance Matrix (Σ):"),
               textInput("sigma", "Σ (row-wise, e.g., 1, 0, 0, 1):", value = "1, 0, 0, 1"),
               
               helpText("Insert values with a , as the delimeter"),
               
               
             ),
             
             mainPanel(
               tabsetPanel(
                 tabPanel("3D Plot",
                          h4("3D Density Plot"),
                          plotlyOutput("densityPlot", height = "700px"),
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
                 tabPanel("Contour Plot",
                          h4("Contour Plot"),
                          plotlyOutput("contourPlot", height = "700px"),
                 )
               )
             ),
             
           )
  ),
  
  tabPanel("Transformation Demo",
           fluidPage(
             h4("Transformation Matrix A (row-wise: a11, a12, a21, a22)"),
             h5("The original distribution is a bivariate normal with identity covariance matrix and is centered at 5 for both X and Y"),
             textInput("A_demo", "A:", value = "1, 0, 0, 1"),
             actionButton("resetA", "Reset A to Identity", class = "btn-secondary"),
             br(), br(),
             helpText("Insert values with a , as the delimeter"),
             splitLayout(
               cellWidths = c("50%", "50%"),
               plotlyOutput("originalPlot"),
               plotlyOutput("transformedPlot")
             ),
             br(),
             splitLayout(
               cellWidths = c("50%", "50%"),
               plotlyOutput("original3DPlot"),
               plotlyOutput("transformed3DPlot")
             ),
             br(),
             h4("Σ' = AΣAᵀ"),
             verbatimTextOutput("transformedSigmaDemo"),
             h4("μ' = Aμ"),
             verbatimTextOutput("transformedMuDemo"),
             fluidRow(
               column(12,
                      h4("Interpretation"),
                      textOutput("interpretation2")
               )
             ),
             br(),
             br(),
             br()
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



server <- function(input, output) {
  
  # Helper Functions
  
  
  
  parseMatrix <- function(input_str) {
    vals <- as.numeric(unlist(strsplit(input_str, ",")))
    if (length(vals) != 4 || any(is.na(vals))) return(NULL)
    matrix(vals, nrow = 2, byrow = TRUE)
  }
  
  parseVector <- function(input_str) {
    vals <- as.numeric(unlist(strsplit(input_str, ",")))
    if (length(vals) != 2 || any(is.na(vals))) return(NULL)
    vals
  }
  
 
  mu <- reactive({
    parseVector(input$mu)
  })
  
  sigma <- reactive({
    sigma <- parseMatrix(input$sigma)
  })
  
  
  
  axisRange <- reactive({
    mu <- c(input$mu1, input$mu2)
    sigma <- sigma()
    
    # Compute max std dev direction
    max_sd <- sqrt(max(eigen(sigma)$values))
    
    lim <- 3 * max_sd + 1
    
    list(
      x = c(mu[1] - lim, mu[1] + lim),
      y = c(mu[2] - lim, mu[2] + lim),
      z = c(0, 0.25) 
    )
  })
  A_demo_matrix <- reactiveVal(matrix(c(1, 0, 0, 1), nrow = 2))  # Default identity
  
  observeEvent(input$A_demo, {
    parsed <- parseMatrix(input$A_demo)
    if (is.null(parsed)) {
      A_demo_matrix(NULL)
    } else {
      A_demo_matrix(parsed)
    }
  })
  
  observeEvent(input$resetA, {
    updateTextInput(inputId = "A_demo", value = "1, 0, 0, 1", session = getDefaultReactiveDomain())
  })
  
  
  output$densityPlot <- renderPlotly({
    mu <- mu()
    if (is.null(mu)) return(NULL)
    
    sigma <- sigma()
    if (is.null(sigma)) return(NULL)
    
    # check if the matrix is positive definite
    if (!all(eigen(sigma)$values > 0)) {
      return(plotly_empty(type = "surface") %>% 
               layout(title = "Covariance matrix must be positive definite"))
    }
    
    x <- seq(mu[1] - 3 * sqrt(sigma[1, 1]), mu[1] + 3 * sqrt(sigma[1, 1]), length.out = 80)
    y <- seq(mu[2] - 3 * sqrt(sigma[2, 2]), mu[2] + 3 * sqrt(sigma[2, 2]), length.out = 80)
    grid <- expand.grid(x = x, y = y)
    z <- matrix(dmvnorm(grid, mean = mu, sigma = sigma), nrow = length(x), byrow = TRUE)
    
    
    ranges <- axisRange()
    plot_ly(x = x, y = y, z = z) %>%
      add_surface(contours = list(
        z = list(show = TRUE, usecolormap = TRUE, highlightcolor = "#ff0000", project = list(z = TRUE))
      )) %>%
      layout(scene = list(
        xaxis = list(title = "X", range = ranges$x),
        yaxis = list(title = "Y", range = ranges$y),
        zaxis = list(title = "Density", range = c(0, max(z, na.rm = TRUE) * 1.1)),
        camera = list(eye = list(x = 1.8, y = 1.8, z = 1))
      ))
  })
  
  output$contourPlot <- renderPlotly({
    mu <- mu()
    sigma <- sigma()
    
    if (sigma[1, 2] != sigma[2, 1]) {
      return(plotly_empty(type = "surface") %>% 
                layout(title = "Covariance matrix must be Symmetric"))
    }
    
    if (is.null(mu) || is.null(sigma)) return(NULL)
    if (!all(eigen(sigma)$values > 0)) {
      return(plotly_empty(type = "surface") %>% 
               layout(title = "Covariance matrix must be positive definite"))
    }
    
    # Sorted x and y sequences
    x_seq <- sort(seq(mu[1] - 3 * sqrt(sigma[1, 1]), mu[1] + 3 * sqrt(sigma[1, 1]), length.out = 100))
    y_seq <- sort(seq(mu[2] - 3 * sqrt(sigma[2, 2]), mu[2] + 3 * sqrt(sigma[2, 2]), length.out = 100))
    
    grid <- expand.grid(x = x_seq, y = y_seq)
    z_vals <- matrix(mahalanobis(grid, center = mu, cov = sigma), nrow = length(x_seq), ncol = length(y_seq))
    
    probs <- c(0.95, 0.90, 0.80, 0.70, 0.60, 0.50)
    labels <- paste0(probs * 100, "%")
    levels_chi2 <- qchisq(probs, df = 2)
    
    contour_data <- do.call(rbind, lapply(seq_along(levels_chi2), function(i) {
      level <- levels_chi2[i]
      clines <- contourLines(x = x_seq, y = y_seq, z = z_vals, levels = level)
      if (length(clines) == 0) return(NULL)
      do.call(rbind, lapply(clines, function(cl) {
        data.frame(x = cl$x, y = cl$y, level = labels[i])
      }))
    }))
    
    p <- ggplot(contour_data, aes(x = x, y = y, color = level)) +
      geom_path(size = 1) +
      scale_color_manual(
        values = c("95%" = "blue4",
                   "90%" = "blue3",
                   "80%" = "dodgerblue3",
                   "70%" = "deepskyblue2",
                   "60%" = "lightskyblue",
                   "50%" = "powderblue"),
        name = "Confidence Level"
      ) +
      coord_fixed() +
      labs(title = "2D Contour Plot",
           x = "X", y = "Y") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  
  output$originalPlot <- renderPlotly({
    mu <- c(5, 5)
    sigma <- matrix(c(1, 0, 0, 1), nrow = 2)
    
    
    x_seq <- sort(seq(mu[1] - 3 * sqrt(sigma[1, 1]), mu[1] + 3 * sqrt(sigma[1, 1]), length.out = 100))
    y_seq <- sort(seq(mu[2] - 3 * sqrt(sigma[2, 2]), mu[2] + 3 * sqrt(sigma[2, 2]), length.out = 100))
    
    grid <- expand.grid(x = x_seq, y = y_seq)
    z_vals <- matrix(mahalanobis(grid, center = mu, cov = sigma), nrow = length(x_seq), ncol = length(y_seq))
    
    probs <- c(0.95, 0.90, 0.80, 0.70, 0.60, 0.50)
    labels <- paste0(probs * 100, "%")
    levels_chi2 <- qchisq(probs, df = 2)
    
    contour_data <- do.call(rbind, lapply(seq_along(levels_chi2), function(i) {
      level <- levels_chi2[i]
      clines <- contourLines(x = x_seq, y = y_seq, z = z_vals, levels = level)
      if (length(clines) == 0) return(NULL)
      do.call(rbind, lapply(clines, function(cl) {
        data.frame(x = cl$x, y = cl$y, level = labels[i])
      }))
    }))
    
    p <- ggplot(contour_data, aes(x = x, y = y, color = level)) +
      geom_path(size = 1) +
      scale_color_manual(
        values = c("95%" = "blue4",
                   "90%" = "blue3",
                   "80%" = "dodgerblue3",
                   "70%" = "deepskyblue2",
                   "60%" = "lightskyblue",
                   "50%" = "powderblue"),
        name = "Confidence Level"
      ) +
      coord_fixed() +
      labs(title = "Original Contour",
           x = "X", y = "Y") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$transformedPlot <- renderPlotly({
    
    A <- A_demo_matrix()
    if (is.null(A)) return(plotly_empty() %>% layout(title = "Invalid A"))
    
    mu <- c(5, 5)
    sigma <- matrix(c(1, 0, 0, 1), nrow = 2)
    
    
    transformed_mu <- A %*% as.matrix(mu)
    
    
    transformed_sigma <- A %*% sigma %*% t(A)
    
    if (!all(eigen(transformed_sigma)$values > 0)) {
      return(plotly_empty() %>% layout(title = "A Σ Aᵀ is not positive definite"))
    }
    
    x_sd <- 3 * sqrt(transformed_sigma[1, 1])
    y_sd <- 3 * sqrt(transformed_sigma[2, 2])
    x_seq <- seq(transformed_mu[1] - x_sd, transformed_mu[1] + x_sd, length.out = 100)
    y_seq <- seq(transformed_mu[2] - y_sd, transformed_mu[2] + y_sd, length.out = 100)
    
    
    grid <- expand.grid(x = x_seq, y = y_seq)
    z_vals <- matrix(mahalanobis(grid, center = transformed_mu, cov = transformed_sigma), nrow = length(x_seq), ncol = length(y_seq))
    
    probs <- c(0.95, 0.90, 0.80, 0.70, 0.60, 0.50)
    labels <- paste0(probs * 100, "%")
    levels_chi2 <- qchisq(probs, df = 2)
    
    contour_data <- do.call(rbind, lapply(seq_along(levels_chi2), function(i) {
      level <- levels_chi2[i]
      clines <- contourLines(x = x_seq, y = y_seq, z = z_vals, levels = level)
      if (length(clines) == 0) return(NULL)
      do.call(rbind, lapply(clines, function(cl) {
        data.frame(x = cl$x, y = cl$y, level = labels[i])
      }))
    }))
    
    p <- ggplot(contour_data, aes(x = x, y = y, color = level)) +
      geom_path(size = 1) +
      scale_color_manual(
        values = c("95%" = "blue4",
                   "90%" = "blue3",
                   "80%" = "dodgerblue3",
                   "70%" = "deepskyblue2",
                   "60%" = "lightskyblue",
                   "50%" = "powderblue"),
        name = "Confidence Level"
      ) +
      coord_fixed(xlim = range(x_seq), ylim = range(y_seq)) +
      labs(title = "Transformed Contour",
           x = "X", y = "Y") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$transformedSigmaDemo <- renderPrint({
    A <- A_demo_matrix()
    if (is.null(A)) return("Invalid A")
    sigma <- matrix(c(1, 0, 0, 1), nrow = 2)
    A %*% sigma %*% t(A)
  })
  
  output$transformedMuDemo <- renderPrint({
    A <- A_demo_matrix()
    if (is.null(A)) return ("Invalid A")
    mu <- as.matrix(c(5, 5))
    A %*% mu
  })
  
  output$original3DPlot <- renderPlotly({
    mu <- c(5, 5)
    sigma <- matrix(c(1, 0, 0, 1), nrow = 2)
    
    x_seq <- seq(mu[1] - 3 * sqrt(sigma[1, 1]), mu[1] + 3 * sqrt(sigma[1, 1]), length.out = 80)
    y_seq <- seq(mu[2] - 3 * sqrt(sigma[2, 2]), mu[2] + 3 * sqrt(sigma[2, 2]), length.out = 80)
    grid <- expand.grid(x = x_seq, y = y_seq)
    z_vals <- matrix(dmvnorm(grid, mean = mu, sigma = sigma), nrow = length(x_seq), byrow = TRUE)
    
    plot_ly(x = x_seq, y = y_seq, z = z_vals) %>%
      add_surface(colorscale = "Viridis") %>%
      layout(title = "Original 3D Density",
             scene = list(
               xaxis = list(title = "X"),
               yaxis = list(title = "Y"),
               zaxis = list(title = "Density")
             ))
  })
  
  output$transformed3DPlot <- renderPlotly({
    A <- A_demo_matrix()
    mu <- c(5, 5)
    
    sigma <- matrix(c(100, 1, 1, 100), nrow = 2)
    transformed_sigma <- A %*% sigma %*% t(A)
    transformed_mu <- A %*% as.matrix(mu)
    
    if (!all(eigen(transformed_sigma)$values > 0)) {
      return(plotly_empty() %>% layout(title = "A Σ Aᵀ is not positive definite"))
    }
    
    x_seq <- seq(transformed_mu[1] - 3 * sqrt(transformed_sigma[1, 1]),
                 transformed_mu[1] + 3 * sqrt(transformed_sigma[1, 1]), length.out = 80)
    y_seq <- seq(transformed_mu[2] - 3 * sqrt(transformed_sigma[2, 2]),
                 transformed_mu[2] + 3 * sqrt(transformed_sigma[2, 2]), length.out = 80)
    grid <- expand.grid(x = x_seq, y = y_seq)
    z_vals <- matrix(dmvnorm(grid, mean = transformed_mu, sigma = transformed_sigma), nrow = length(x_seq), byrow = TRUE)
    
    plot_ly(x = x_seq, y = y_seq, z = z_vals) %>%
      add_surface(colorscale = "Viridis") %>%
      layout(title = "Transformed 3D Density",
             scene = list(
               xaxis = list(title = "X"),
               yaxis = list(title = "Y"),
               zaxis = list(title = "Density")
             ))
  })
  
  # Interpretations
  output$interpretation1 <- renderText({
    "Adding a positive covariance term will tilt the distribution where the two variables tend to increase together creating an upward-sloping shape.
     Likewise, adding a negative covariance term tilts the distribution in the opposite direction, where when one variable increases the other decreases.
    Click on the 'Contour Plot' tab to see the direction of the shape more clearly."
  })
  
  output$interpretation2 <- renderText({
    "Multiplying a Standard bivariate normal distribution by a 2x2 tranformation matrix linearly stretches, rotates, and skews the distribution. Off-diagonal elements
    in the matrix will introduce covariance between the variables."
  })
}

shinyApp(ui = ui, server = server)
