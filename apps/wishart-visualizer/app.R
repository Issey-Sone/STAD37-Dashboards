library(shiny)
library(MASS)
library(shinythemes)
library(ellipse)
library(tidyverse)
library(patchwork)
library(gridExtra)


if (FALSE) library(munsell)

ui <- navbarPage(
  title = "Wishart Distribution and it's properties",
  theme = shinytheme("flatly"),
  
  tabPanel("Wishart Matrix", 
     withMathJax(
       helpText("$$
       W_{m}(\\dot|\\Sigma) = \\text{distribution of } \\Sigma_{j=1}^{m}Z_jZ'_{j}
      $$")
     ),
     sidebarLayout(
       sidebarPanel(
         helpText("In this example, p = 2 and Sigma is the identity matrix"),
         sliderInput("m", "Degrees of Freedom (m)", min = 2, max = 100, value = 5),
         sliderInput("n", "Number of Samples", value = 500, min = 100, max = 5000, step = 100),
         helpText(
           "the value of m can be thought of as the number of bivariate normal with
           0 mean that we are drawing. We draw n samples to get a distribution."
         )
       ),
       mainPanel(
         plotOutput("matrix_densities", height = "600px"),
         withMathJax(
           helpText("$$
            \\sum_{j=1}^m Z_j Z_j^\\top = 
            \\begin{bmatrix}
              \\sum Z_{1j}^2 & \\sum Z_{1j} Z_{2j} \\\\
              \\sum Z_{2j} Z_{1j} & \\sum Z_{2j}^2
            \\end{bmatrix}
            \\sim W_m(\\Sigma)
          $$")
         ),
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
     )
  ),
  
  tabPanel("Wishart Additivity: ", 
    withMathJax(
      helpText("$$
      \\text{If } \\mathbf{A}_1 \\sim W_{m_1}(\\Sigma), \\; \\mathbf{A}_2 \\sim W_{m_2}(\\Sigma), 
      \\text{ and they are independent, then } \\mathbf{A}_1 + \\mathbf{A}_2 \\overset{d}{=} W_{m_1 + m_2}(\\Sigma).
      $$")
    ),
    
    sidebarLayout(
      sidebarPanel(
        sliderInput("m1", "Degrees of Freedom (m1):", min = 2, max = 100, value = 5),
        sliderInput("m2", "Degrees of Freedom (m2):", min = 2, max = 100, value = 5),
        
        h4("Covariance Matrix (Σ):"),
        textInput("sigma", "Σ (row-wise, e.g., 1, 0.5, 0.5, 1):", value = "1, 0.5, 0.5, 1"),
        helpText("Enter 4 numbers separated by commas for a 2x2 matrix."),
        
        sliderInput("n_samples", "Number of Samples:", value = 500, min = 100, max = 5000, step = 100),
      ),
      
      mainPanel(
        plotOutput("histograms", height = "450px"),
        br(),
        verbatimTextOutput("sampleMatrix"),
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
    )
  ),
  
  tabPanel("Wishart with Identity Covariance Matrix",
    withMathJax(
      helpText("$$\\text{If } \\mathbf{A} \\sim W_{m}(A|\\mathbf{I}) \\text{ then } \\text{trace}\\mathbf{(A)}
               \\overset{d}{=} \\chi^2_{mp}$$")
    ),
    sidebarLayout(
      sidebarPanel(
        sliderInput("m_chi", "Degrees of Freedom (m):", min = 1, max = 50, value = 10),
        sliderInput("p_chi", "Number of variables (p)", min = 1, max = 50, value = 2),
        helpText("Make Sure m >= p"),
        sliderInput("n_chi", "Number of Samples:", value = 1000, min = 100, max = 5000, step = 100)
      ),
      mainPanel(
        plotOutput("ChiTracePlot", height = "450px"),
        fluidRow(
          column(12,
                 h4("Interpretation"),
                 textOutput("interpretation3"),
                 withMathJax(
                   helpText(
                     "$$A = X^TX$$ $$\\text{tr}(A) = \\sum_{i=1}^{p}\\sum_{j=1}^{m}X_{ij}^2$$"
                   )
                 )
          )
        ),
        br(),
        br(),
        br()
      )
    )
  ),
  
  tabPanel("Wishart Transformation: CAC",
    withMathJax(
      helpText("$$
        \\text{If } \\mathbf{A} \\sim W_m(\\Sigma), \\text{ then } \\mathbf{C A C'} \\overset{d}{=} W_m(\\mathbf{C \\Sigma C'}).
      $$")
    ),
    
    sidebarLayout(
      sidebarPanel(
        sliderInput("m_transformation", "Degrees of Freedom (m):", min = 2, max = 100, value = 10),
        textInput("sigma_transformation", "Σ (row-wise, e.g., 1, 0.5, 0.5, 1):", value = "1, 0.5, 0.5, 1"),
        textInput("C", "Transformation Matrix C (row-wise, e.g., 1, 0, 0, 1):", value = "1, 0, 0, 1"),
        sliderInput("n_transformation", "Number of Samples:", value = 500, min = 100, max = 5000, step = 100),
        helpText("Careful pushing m and the number of samples may crash the dashboard")
      ),
      
      mainPanel(
        plotOutput("transformation_density", height = "450px"),
        verbatimTextOutput("transformation_output"),
        fluidRow(
          column(12,
                 h4("Interpretation"),
                 textOutput("interpretation4")
          )
        ),
        br(),
        br(),
        br()
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

server <- function(input, output) {
  
  parseMatrix <- function(input_str) {
    vals <- as.numeric(unlist(strsplit(input_str, ",")))
    if (length(vals) != 4 || any(is.na(vals))) return(NULL)
    matrix(vals, nrow = 2, byrow = TRUE)
  }
  
  
  
  generate_wishart_sample <- function(m, Sigma) {
    p <- nrow(Sigma)
    Z_list <- replicate(m, mvrnorm(n = 1, mu = rep(0, p), Sigma = Sigma), simplify = FALSE)
    Reduce(`+`, lapply(Z_list, function(z) z %*% t(z)))
  }
  
  
  # Tab 1: Wishart Matrix
  
  output$matrix_densities <- renderPlot({
    m <- input$m
    n <- input$n
    sigma <- matrix(c(1, 0, 0, 1), nrow = 2)
    direct_samples <- replicate(n, generate_wishart_sample(m, sigma), simplify = FALSE)
    
    s11 <- sapply(direct_samples, function(S) S[1,1])
    s12 <- sapply(direct_samples, function(S) S[1,2])
    s22 <- sapply(direct_samples, function(S) S[2,2])
    
    
    df <- data.frame(
      S11 = s11,
      S12 = s12,
      S22 = s22
    )
    
    
    p1 <- ggplot(df, aes(x = S11)) +
      geom_density(fill = "skyblue", aes(color = "Empirical Density"), alpha = 0.6, linewidth = 1) +
      stat_function(
        aes(color = "Chi-square(m)"), fun = dchisq, args = list(df = m), linewidth = 1, linetype = "dashed") +
      scale_color_manual(
        values = c("Chi-square(m)" = "darkblue", "Empirical Density" = "skyblue")
      ) + 
      labs(title = "Distribution of first diagonal entry", x = "Value", y = "Density") +
      theme_minimal() 
      
    p2 <- ggplot(df, aes(x = S12)) +
      geom_density(fill = "salmon", aes(color = "Empirical Density"), alpha = 0.6, linewidth = 1) +
      scale_color_manual(
        values = c("Empirical Density" = "salmon")) +
      labs(title = "Distribution of off-diagonal entry", x = "Value", y = "Density") + 
      theme_minimal()
    
    
    p3 <- ggplot(df, aes(x = S22)) +
      geom_density(fill = "lightgreen", aes(color = "Empirical Density"), alpha = 0.6, linewidth = 1) + 
      stat_function(
        aes(color = "Chi-square(m)"), fun = dchisq, args = list(df = m), linewidth = 1, linetype = "dashed") +
      scale_color_manual(
        values = c("Chi-square(m)" = "forestgreen", "Empirical Density" = "lightgreen")
      ) +
      labs(title = "Distribution of second diagonal entry", x = "Value", y = "Density") + 
      theme_minimal()
    
    p1 + p2 + p3
    
  })
  
  
  # Tab 2: Additive property
  output$histograms <- renderPlot({
    Sigma <- parseMatrix(input$sigma)
    if (is.null(Sigma)) return("Invalid input matrix")
    
    m1 <- input$m1
    m2 <- input$m2
    m_sum <- m1 + m2
    n <- input$n_samples
    
    # Samples of A1 + A2
    sum_samples <- replicate(n, {
      A1 <- generate_wishart_sample(m1, Sigma)
      A2 <- generate_wishart_sample(m2, Sigma)
      A1 + A2
    }, simplify = FALSE)
    
    # Samples directly from W_{m1 + m2}
    direct_samples <- replicate(n, generate_wishart_sample(m_sum, Sigma), simplify = FALSE)
    

    df <- data.frame(
      W11 = c(sapply(sum_samples, function(S) S[1, 1]),
              sapply(direct_samples, function(S) S[1, 1])),
      W12 = c(sapply(sum_samples, function(S) S[1, 2]),
              sapply(direct_samples, function(S) S[1, 2])),
      W22 = c(sapply(sum_samples, function(S) S[2, 2]),
              sapply(direct_samples, function(S) S[2, 2])),
      group = rep(c("A1 + A2", "W(m1 + m2)"), each = n)
    )
    
    
    p1 <- ggplot(df, aes(x = W11, fill = group, color = group)) +
      geom_density(alpha = 0.4) +
      labs(title = "Density of (1, 1) element", x = "W[1,1]", y = "Density") +
      theme_minimal() +
      scale_fill_manual(values = c("blue", "red")) +
      scale_color_manual(values = c("blue", "red"))
    
    p2 <- ggplot(df, aes(x = W12, fill = group, color = group)) +
      geom_density(alpha = 0.4) +
      labs(title = "Density of (1, 2)/(2, 1) element", x = "W[1, 2]", y = "Density") +
      theme_minimal() +
      scale_fill_manual(values = c("blue", "red")) +
      scale_color_manual(values = c("blue", "red"))
    
    p3 <- ggplot(df, aes(x = W22, fill = group, color = group)) +
      geom_density(alpha = 0.4) + 
      labs(title = "Density of (2, 2) element", x = "W[2, 2]", y = "Density") +
      theme_minimal() + 
      scale_fill_manual(values = c("blue", "red")) +
      scale_color_manual(values = c("blue", "red"))
    
    grid.arrange(p1, p2, p3, nrow = 1)
  })
  
  
  output$sampleMatrix <- renderPrint({
    Sigma <- parseMatrix(input$sigma)
    if (is.null(Sigma)) return("Invalid Sigma matrix.")
    
    m1 <- input$m1
    m2 <- input$m2
    m_sum <- m1 + m2
    
    n_samples <- input$n_samples
    
    p <- 2
    
    sum_samples <- matrix(0, nrow = p, ncol = p)
    direct_samples <- matrix(0, nrow = p, ncol = p)
    
    for (i in 1:n_samples) {
      A1 <- generate_wishart_sample(m1, Sigma)
      A2 <- generate_wishart_sample(m2, Sigma)
      S_sum <- A1 + A2
      sum_samples <- sum_samples + S_sum
      S_direct <- generate_wishart_sample(m_sum, Sigma)
      direct_samples <- direct_samples + S_direct
    }
    
    sum_mean <- sum_samples / n_samples
    direct_mean <- direct_samples / n_samples
    
    
    cat("A1 ~ W(", m1, ", Σ)\n")
    cat("A2 ~ W(", m2, ", Σ)\n")
    cat("Sum: A1 + A2 ~ W(", m1 + m2, ", Σ)\n\n")
    
    cat("Element-wise mean of A1 + A2 samples: \n")
    print(sum_mean)
    cat("\nElement-wise mean of Direct W(m1 + m2, Σ) samples: \n")
    print(direct_mean)
  })
  
  
  # Tab 3: Identity Covariance Matrix
  output$ChiTracePlot <- renderPlot({
    m <- input$m_chi
    n <- input$n_chi
    p <- input$p_chi
    Sigma <- diag(p)
    
    traces <- replicate(n, {
      A <- generate_wishart_sample(m, Sigma)
      sum(diag(A))
    })
    
    df <- data.frame(trace = traces)
    
    ggplot(df, aes(x = trace)) + 
      geom_density(aes(color = "Empirical Density"), fill = "skyblue", alpha = 0.6, linewidth = 1) +
      stat_function(
        aes(color = "Chi-squared PDF"),
        fun = dchisq, args = list(df = m * p), linetype = "dashed", linewidth = 1
      ) +
      labs(
        title = "Trace of Wₘ(I) vs χ²ₘₚ",
        x = "trace of Wishart Sample",
        y = "density",
        color = "Legend"
      ) +
      theme_minimal() +
      scale_color_manual(values = c("Empirical Density" = "skyblue", "Chi-squared PDF" = "darkblue")) +
      annotate(
        "text", 
        x = mean(df$trace), y = max(density(df$trace)$y) * 0.9,
        label = paste("df =", m * p), color = "darkblue", hjust = 0.5, size = 5
      )
  })
  
  
  
  # Tab 4: transfomation
  output$transformation_density <- renderPlot({
    Sigma <- parseMatrix(input$sigma_transformation)
    C <- parseMatrix(input$C)
    if (is.null(Sigma) || is.null(C)) return()
    
    m <- input$m_transformation
    n <- input$n_transformation
    Sigma_new <- C %*% Sigma %*% t(C)
    
    
    
    CAC_samples <- replicate(n, {
      A <- generate_wishart_sample(m, Sigma)
      C %*% A %*% t(C)
    }, simplify = FALSE)
    
    # Direct samples from W_m(CΣC′)
    direct_samples <- replicate(n, generate_wishart_sample(m, Sigma_new), simplify = FALSE)
    
    df <- data.frame(
      W11 = c(sapply(CAC_samples, function(S) S[1, 1]),
              sapply(direct_samples, function(S) S[1, 1])),
      W12 = c(sapply(CAC_samples, function(S) S[1, 2]),
              sapply(direct_samples, function(S) S[1, 2])),
      W22 = c(sapply(CAC_samples, function(S) S[2, 2]),
              sapply(direct_samples, function(S) S[2, 2])),
      group = rep(c("CAC'", "W(CΣC')"), each = n)
    )
    
    
    
    p1 <- ggplot(df, aes(x = W11, fill = group, color = group)) +
      geom_density(alpha = 0.4) +
      labs(title = "Density of (1,1) Element", x = "W[1,1]", y = "Density") +
      theme_minimal() +
      scale_fill_manual(values = c("blue", "red")) +
      scale_color_manual(values = c("blue", "red"))
    
    p2 <- ggplot(df, aes(x = W12, fill = group, color = group)) +
      geom_density(alpha = 0.4) +
      labs(title = "Density of (1,2)/(2,1) Element", x = "W[1,2]", y = "Density") +
      theme_minimal() +
      scale_fill_manual(values = c("blue", "red")) +
      scale_color_manual(values = c("blue", "red"))
    
    p3 <- ggplot(df, aes(x = W22, fill = group, color = group)) +
      geom_density(alpha = 0.4) +
      labs(title = "Density of (2,2) Element", x = "W[2,2]", y = "Density") +
      theme_minimal() +
      scale_fill_manual(values = c("blue", "red")) +
      scale_color_manual(values = c("blue", "red"))
    
    grid.arrange(p1, p2, p3, nrow = 1)
  })
  
  output$transformation_output <- renderPrint({
    Sigma <- parseMatrix(input$sigma_transformation)
    C <- parseMatrix(input$C)
    if (is.null(Sigma) || is.null(C)) return("Invalid input matrix.")
    
    n_samples <- input$n_transformation
    
    m <- input$m_transformation
    A <- generate_wishart_sample(m, Sigma)
    p <- 2
    
    CAC_samples <- matrix(0, nrow = p, ncol = p)
    direct_samples <- matrix(0, nrow = p, ncol = p)
    Sigma_new <- C %*% Sigma %*% t(C)
    
    for (i in 1:n_samples) {
      A <- generate_wishart_sample(m, Sigma)
      CAC_samples <- CAC_samples + C %*% A %*% t(C)
      direct <- generate_wishart_sample(m, Sigma_new)
      direct_samples <- direct_samples + direct
    }
    
    CAC_mean <- CAC_samples / n_samples
    direct_mean <- direct_samples / n_samples
    cat("Element-wise Mean of CAC′ samples:\n")
    print(CAC_mean)
    cat("\nElement-wise Mean of Direct W(m, CΣC′) samples:\n")
    print(direct_mean)
  })
  
  # Interpretations
  output$interpretation1 <- renderText({
    "Notice the diagonal element of the 2x2 Wishart matrix is a sum of squared normal variables which follows a chi-square distribution with m degrees of
    freedom. The off diagonal is a sum of a product of two correlated normal variables. If you are interested it is a 
    variance-gamma distribution (also known as a Bessel function distribution)."
  })
  
  output$interpretation2 <- renderText({
    "The Wishart distribution can't be explicity graphed, so the best way to see this is by looking at the densities of the element of the wishart distribution.
    The close alignment of the densities showcase the additivity property of the Wishart distribution. The element wise mean comparison should also be quite similar since the
    expected matrices should be equal. (Essentailly drawing n number of samples from Wishart, and taking the averages)"
  })
  
  output$interpretation3 <- renderText({
    "If A follows a wishart distribution with identity matrix, we can decompose A into X'X with X being a p x m matrix of iid standard normals.
    The trace is then just a sum of pm independent squared standard normal variables which is a chi squared distribution."
  })
  output$interpretation4 <- renderText({
    "The close alignment of the densities showcase the transformation property of the Wishart distribution. The element wise mean comparison should also be quite similar since
    the expected matrices should be equal. (Essentially drawing n number of samples from Wishart, and taking the averages)"
  })
  
  
  
}

shinyApp(ui = ui, server = server)
