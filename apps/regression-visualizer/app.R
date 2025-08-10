library(shiny)
library(shinythemes)
library(MASS)
library(tidyverse)
library(plotly)
library(gridExtra)
library(ellipse)
library(DT)


if (FALSE) library(munsell)

ui <- navbarPage("Regression",
                 theme = shinytheme("flatly"),
                 tabPanel("Univariate Regression",
                          withMathJax(
                            helpText(
                              "$$\\begin{bmatrix}
                                  \\hat\\beta_0 \\\\
                                  \\hat\\beta_1
                                  \\end{bmatrix}
                                  
                                  \\sim N_2(\\begin{bmatrix} \\beta_0 \\\\ \\beta_1 \\end{bmatrix}, \\sigma^2\\begin{bmatrix}
                                   \\frac{1}{n} + \\frac{\\bar x }{S_{xx}} & -\\frac{\\bar x}{S_{xx}}  \\\\
                                   -\\frac{\\bar x}{S_{xx}} & \\frac{1}{S_{xx}} 
                                  \\end{bmatrix})$$"
                            )
                          ),
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                numericInput("n_draws", "Number of Observations (sample size):", value = 10, min = 100, max = 10000, step = 100),
                                h4("Beta Vector (β):"),
                                textInput("betas", "β (eg. Beta0, Beta1): ", value = "3, 0.5"),
                                helpText("For best results, keep Beta0 between -10 to 12 and Beta1 between -1 to 2"),
                                
                                h4("sigma squared"),
                                numericInput("sigmasq", "σ^2: ", value = 3),
                        
                              
                                actionButton("run_simulation", "Run Simulation", class = "btn-primary"),
                                
                                h4("Expected Properties"),
                                verbatimTextOutput("theoretical_info"),
                                withMathJax(
                                helpText("$$\\epsilon_i \\sim N(0, σ^2) \\\\ S_{xx} = \\sum(x_i - \\bar x)^2$$")
                                ),
                                helpText(HTML("
                                  <b>Legend:</b><br>
                                  <span style='color:red;'>-- True Beta value</span><br>
                                  <span style='color:blue;'>— Empirical Beta value</span><br>
                                  <span style='color:black;'>-- Underlying distribution</span><br>
                                "))
                              ),
                              
                              mainPanel(
                                plotlyOutput("bivariate_plot"),
                                plotOutput("marginal_plots"),
                                fluidRow(
                                  column(12,
                                         h4("Interpretation"),
                                         textOutput("interpretation_uni")
                                  )
                                ),
                                br(),
                                br(),
                                br()
                              )
                            )
                          )
                 ),
                 tabPanel("Multivariate Regression",
                          withMathJax(
                            helpText(
                              "$$\\mathbf{y} = \\mathbf{X}\\boldsymbol{\\beta} + \\boldsymbol{\\epsilon}, \\quad \\boldsymbol{\\epsilon} \\sim N(\\mathbf{0}, \\Sigma)$$"
                            )
                          ),
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                numericInput("n_draws_mvt", "Number of Observations (sample size):", value = 10, min = 100, max = 5000, step = 100),
                                
                                h4("Regression Parameters:"),
                                textInput("betas_mv", "β (β01, β11, β02, β12): ", value = "3, 0.5, 5, 1.5"),
                                textInput("sigmasq_mv", "Σ Variance of epsilon: ", value = "2, 0, 0, 5"),
                                helpText("has to be positive definite"),
                                
                                withMathJax(
                                  helpText(
                                    "$$Y_{j1} = \\beta_{01} + \\beta_{11}z_{j1} + \\epsilon_{j1} \\\\
                                       Y_{j1} = \\beta_{02} + \\beta_{12}z_{j2} + \\epsilon_{j2}$$"
                                  )
                                ), 
                                
                                
                                actionButton("run_simulation_mv", "Run Simulation", class = "btn-primary"),
                                
                                h4("Expected Covariance Matrix"),
                                verbatimTextOutput("cov_matrix_display"),
                                helpText(HTML("
                                  <b>Legend:</b><br>
                                  <span style='color:red;'>-- True Beta value</span><br>
                                  <span style='color:blue;'>— Empirical Beta value</span><br>
                                  <span style='color:black;'>-- Underlying distribution</span><br>
                                "))
                              ),
                              
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Joint Distribution", 
                                           plotlyOutput("multivariate_joint_plot1"),
                                           br(),
                                           plotlyOutput("multivariate_joint_plot2"),
                                           br(),
                                           br(),
                                           br()
                                  ),
                                  tabPanel("Marginal Distributions",
                                           plotOutput("multivariate_marginal_plots")
                                  ),
                                  tabPanel("Simulation Summary",
                                           h4("Coefficient Estimates Summary:"),
                                           DT::dataTableOutput("summary_table"),
                                           br(),
                                           h4("Expected vs. Empirical Comparison:"),
                                           verbatimTextOutput("comparison_output"),
                                           fluidRow(
                                             column(12,
                                                    h4("Interpretation"),
                                                    textOutput("interpretation_mv"),
                                                    withMathJax(
                                                      helpText(
                                                        "$$\\Sigma \\sim \\begin{bmatrix}
                                                          \\sigma_{1} & \\sigma_{12} \\\\
                                                          \\sigma_{12} & \\sigma_{2}
                                                        \\end{bmatrix} \\text{then } \\text{Cov}(\\hat{\\beta^{(1)}}, \\hat{\\beta^{(2)}}) = \\sigma_{12}(X^TX)^{-1}$$"
                                                        
                                                      )
                                                    )
                                             )
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
                 tabPanel("Prediction Ellipse",
                  withMathJax(
                    helpText(
                      "$$\\text{T}^2\\text{ Simultaneous Prediction Intervals: }
                      
                        \\hat{Y}(x_0)_j \\pm \\sqrt{c \\cdot \\hat{\\sigma}_{jj} (1 + \\mathbf{x}_0^T (\\mathbf{X}^T\\mathbf{X})^{-1} \\mathbf{x}_0)}
                       \\text{ where } c = \\frac{p(n - 1)}{n - p} F_{p, n - p}(1 - \\alpha)$$
                       $$\\text{Prediction Ellipse: } \\hat{Y}(x_0) \\pm t_{\\alpha/2, n-2} \\sqrt{\\hat{\\sigma}^2 (1 + \\mathbf{x}_0^T(\\mathbf{X}^T\\mathbf{X})^{-1}\\mathbf{x}_0)}$$"
                    )
                  ),
                  fluidPage(
                    sidebarLayout(
                      sidebarPanel(
                        h4("Data Generation Parameters: "),
                        numericInput("n_pred", "Sample Size:", value = 10, min = 10, max = 200, step= 10),
                        textInput("betas_mv_pred", "β (β01, β11, β02, β12): ", value = "3, 0.5, 5, 1.5"),
                        textInput("sigmasq_mv_pred", "Σ Variance of epsilon: ", value = "2, 0, 0, 5"),
                        helpText("has to be positive definite"),
                        
                        h4("Prediction Settings: "),
                        numericInput("x_new", "New X value for prediction: ", value = 10),
                        sliderInput("conf_level", "Confidence Level: ", value = 0.95, min = 0.9, max = 0.99, step = 0.01),
                        
                        actionButton("generate_pred_data", "Generate Data & Predict", class = "btn-primary"),
                        
                        br(),
                        h4("Prediction Results"),
                        verbatimTextOutput("prediction_results"),
                        br(),
                        helpText(HTML("
                                  <b>Legend:</b><br>
                                  <span style='color:blue;'>- Prediction Interval</span><br>
                                  <span style='color:red;'>● Predicted point</span><br>
                                  <span style='color:skyblue;'>— T^2 interval</span><br>
                                ")
                      )
                      ),
                      mainPanel(
                        plotlyOutput("prediction_plot"),
                        fluidRow(
                          column(12,
                                 h4("Interpretation"),
                                 textOutput("interpretation_pred")
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
  
  
  
  parseVector <- function(input_str) {
    vals <- as.numeric(unlist(strsplit(input_str, ",")))
    if (length(vals) != 2 || any(is.na(vals))) return(NULL)
    vals
  }
  
  parseMatrix <- function(input_str) {
    vals <- as.numeric(unlist(strsplit(input_str, ",")))
    if (length(vals) != 4 || any(is.na(vals))) return (NULL)
    matrix(vals, nrow = 2, byrow = TRUE)
  }
  
  
  simulation_results <- reactiveVal(NULL)

  simulation_results_mvt <- reactiveVal(NULL)
  
  
  
  # single variable generation
  observeEvent(input$run_simulation, {
    
    n_sim <- 1000
    n <- input$n_draws
    x_mean <- 10
    x_sd <- 2
    var_x <- x_sd^2
    
    error_var <- input$sigmasq
    
    beta0hat <- numeric(n_sim)
    beta1hat <- numeric(n_sim)
    
    set.seed(123)
    X <- rnorm(n, mean = x_mean, sd = x_sd)
    
    
    
    
    Sxx <- sum((X - mean(X))^2)
    
    var_beta1 <- error_var / Sxx
    var_beta0 <- error_var * (1/n + mean(X)^2 / Sxx)
    cov_beta01 <- -error_var * mean(X) / (Sxx)
    
    beta_vals <- parseVector(input$betas)
    beta0_true <- beta_vals[1]
    beta1_true <- beta_vals[2]
    
    
    for (i in 1:n_sim) {
      epsilon <- rnorm(n, mean = 0, sd = sqrt(error_var))
      Y <- beta0_true + X * beta1_true + epsilon 
      model <- lm(Y ~ X)
      
      beta0hat[i] <- coef(model)[1]
      beta1hat[i] <- coef(model)[2]
    }
    
    
    results <- data.frame(
      simulation = 1:n_sim,
      beta0hat = beta0hat,
      beta1hat = beta1hat
    )
    
    simulation_results(results)
    
    mu_x <- x_mean
    Sigma <- matrix(c(var_beta1, cov_beta01,
                      cov_beta01, var_beta0), nrow = 2)
    
    ellipse_pts <- as.data.frame(ellipse(
      x = Sigma,
      centre = c(beta1_true, beta0_true),
      level = 0.95,
      npoints = 200
    ))
    
    colnames(ellipse_pts) <- c("beta1hat", "beta0hat")
    
    simulation_results(list(data = results, ellipse = ellipse_pts,
                            beta0_true = beta0_true, beta1_true = beta1_true,
                            var_beta0 = var_beta0, var_beta1 = var_beta1,
                            cov_beta01 = cov_beta01))
  })

  # Multivariable generation
  observeEvent(input$run_simulation_mv, {
    set.seed(123)
    n <- input$n_draws_mvt
    
   
    x1 <- rnorm(n, mean = 10, sd = 2)
    
    
    X <- cbind(1, x1)
    n_sim <- 1000
    beta01hat <- numeric(n_sim)
    beta11hat <- numeric(n_sim)
    beta02hat <- numeric(n_sim)
    beta12hat <- numeric(n_sim)

    epsilon_sigma <- parseMatrix(input$sigmasq_mv)

    epsilon_center <- c(0, 0)
    

    beta_vals <- parseMatrix(input$betas_mv)
    beta01_true <- beta_vals[1, 1]
    beta11_true <- beta_vals[1, 2]
    beta02_true <- beta_vals[2, 1]
    beta12_true <- beta_vals[2, 2]
    B <- matrix(c(beta01_true, beta02_true, beta11_true, beta12_true), nrow = 2, byrow = TRUE)

    for (i in 1:n_sim) {
      epsilon <- mvrnorm(n = n, mu = epsilon_center, Sigma = epsilon_sigma)
      
      Y <- X %*% B + epsilon
      
      Y1 <- Y[,1]
      Y2 <- Y[,2]
      
      model <- lm(cbind(Y1, Y2) ~ x1)
      
      coefs <- coef(model)
      

      beta01hat[i] <- coefs[1, 1]
      beta11hat[i] <- coefs[2, 1]
      beta02hat[i] <- coefs[1, 2]
      beta12hat[i] <- coefs[2, 2]
    }

    results <- data.frame(
      simulation = 1:n_sim,
      beta01hat = beta01hat,
      beta11hat = beta11hat,
      beta02hat = beta02hat,
      beta12hat = beta12hat
    )

    Sxx1 <- sum((x1 - mean(x1))^2)

    cov_beta01 <- epsilon_sigma[1,1] * (1/n + mean(x1)^2/Sxx1)
    var_beta11 <- epsilon_sigma[1,1] / Sxx1
    cov_beta01_beta11 <- -epsilon_sigma[1,1] * mean(x1) / Sxx1
    
    cov_beta02 <- epsilon_sigma[2,2] * (1/n + mean(x1)^2/Sxx1)
    var_beta12 <- epsilon_sigma[2,2] / Sxx1
    cov_beta02_beta12 <- -epsilon_sigma[2,2] * mean(x1) / Sxx1
    
    cov_beta01_beta02 <- epsilon_sigma[1,2] * (1/n + mean(x1)^2/Sxx1)
    cov_beta01_beta12 <- -epsilon_sigma[1,2] * mean(x1) / Sxx1
    cov_beta11_beta02 <- -epsilon_sigma[1,2] * mean(x1) / Sxx1 
    cov_beta11_beta12 <- epsilon_sigma[1,2] / Sxx1
    
    theoretical_cov <- matrix(c(
      cov_beta01, cov_beta01_beta11, cov_beta01_beta02, cov_beta01_beta12,
      cov_beta01_beta11, var_beta11, cov_beta11_beta02, cov_beta11_beta12,
      cov_beta01_beta02, cov_beta11_beta02, cov_beta02, cov_beta02_beta12,
      cov_beta01_beta12, cov_beta11_beta12, cov_beta02_beta12, var_beta12
    ), nrow = 4, ncol = 4)
    
    rownames(theoretical_cov) <- colnames(theoretical_cov) <- c("B01", "B11", "B02", "B12")

    simulation_results_mvt(list(
      data = results,
      theoretical_cov = theoretical_cov,
      true_betas = c(beta01_true, beta11_true, beta02_true, beta12_true),
      epsilon_sigma = epsilon_sigma,
      x1_mean = mean(x1), 
      Sxx1 = Sxx1
    ))
  })
  
  # single variable plots
  output$bivariate_plot <- renderPlotly({
    req(simulation_results())
    
    sim <- simulation_results()
    
    results <- sim$data
    ellipse_pts <- sim$ellipse
    
    beta0_true <- sim$beta0_true
    beta1_true <- sim$beta1_true
    
    p <- ggplot(results, aes(x = beta1hat, y = beta0hat)) +
      geom_point(alpha = 0.6, size = 1, color = "steelblue") +
      geom_vline(xintercept = beta1_true, color = "black", linetype = "solid", size = 0.5) +
      geom_hline(yintercept = beta0_true, color = "black", linetype = "solid", size = 0.5) +
      xlim(-1, 2) + 
      ylim(-10, 12) + 
      labs(
        title = "Joint Distribution of β₀ and β₁ Estimators",
        x = "β₁ (Slope Estimate)",
        y = "β₀ (Intercept Estimate)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "none"
      )
    
    ggplotly(p, tooltip = c("x", "y"))
    
  })
  
  output$theoretical_info <- renderText({
    req(simulation_results())
    sim <- simulation_results()
    paste0(
      "E[β₀] = ", sim$beta0_true, "\n",
      "E[β₁] = ", sim$beta1_true, "\n",
      "Var(β₀) = ", round(sim$var_beta0, 4), "\n",
      "Var(β₁) = ", round(sim$var_beta1, 4), "\n",
      "Cov(β₀,β₁) = ", round(sim$cov_beta01, 4), "\n",
      "Corr(β₀,β₁) = ", round(sim$cov_beta01/sqrt(sim$var_beta0 * sim$var_beta1), 4)
    )
    
  })
  
  output$marginal_plots <- renderPlot({
    req(simulation_results())
    
    sim <- simulation_results()
    
    data <- sim$data
    
    beta0_true <- sim$beta0_true
    beta1_true <- sim$beta1_true
    var_beta0 <- sim$var_beta0
    var_beta1 <- sim$var_beta1
    
    
    
    suppressWarnings({p1 <- ggplot(data, aes(x = beta0hat)) +
      geom_histogram(bins = 50, alpha = 0.7, fill = "lightblue", color = "black", aes(y = ..density..)) +
      geom_vline(xintercept = beta0_true, color = "red", linetype = "solid", size = 1) +
      geom_vline(xintercept = mean(data$beta0hat), color = "blue", linetype = "dashed") +
      stat_function(fun = dnorm, args = list(mean = beta0_true, sd = sqrt(var_beta0)), color = "black", linetype = "dashed", size = 1) +
      labs(title = "β₀ Distribution", x = "β₀", y = "Density") +
      xlim(-15, 15)+
      theme_minimal()
    })
    
    suppressWarnings({p2 <- ggplot(data, aes(x = beta1hat)) +
      geom_histogram(bins = 50, alpha = 0.7, fill = "lightgreen", color = "black", aes(y = ..density..)) +
      geom_vline(xintercept = beta1_true, color = "red", linetype = "solid", size = 1) +
      geom_vline(xintercept = mean(data$beta1hat), color = "blue", linetype = "dashed") +
      stat_function(fun = dnorm,
                    args = list(mean = beta1_true, sd = sqrt(var_beta1)), color = "black", linetype = "dashed", size = 1) + 
      labs(title = "β₁ Distribution", x = "β₁", y = "Density") +
      xlim(-1, 2)
      theme_minimal()
    })
    
    gridExtra::grid.arrange(p1, p2, ncol = 1)
  })

  # multivariate plots

  output$multivariate_joint_plot1 <- renderPlotly({
    req(simulation_results_mvt())

    results <- simulation_results_mvt()$data
    true_betas <- simulation_results_mvt()$true_betas

    p1 <- ggplot(results, aes(x = beta01hat, y = beta11hat)) +
      geom_point(alpha = 0.6, size = 1, color = "steelblue") +
      geom_vline(xintercept = true_betas[1], color = "black", linetype = "dashed") + 
      geom_hline(yintercept = true_betas[2], color = "black", linetype = "dashed") + 
      labs(title = "β01 vs β11", x = "β01", y = "β11") + 
      theme_minimal()

    ggplotly(p1, tooltip = c("x", "y"))

  })
  
  output$multivariate_joint_plot2 <- renderPlotly({
    req(simulation_results_mvt())
    results <- simulation_results_mvt()$data
    true_betas <- simulation_results_mvt()$true_betas

    p1 <- ggplot(results, aes(x = beta02hat, y = beta12hat)) +
      geom_point(alpha = 0.6, size = 1, color = "royalblue") + 
      geom_vline(xintercept = true_betas[3], color = "black", linetype = "dashed") + 
      geom_hline(yintercept = true_betas[4], color = "black", linetype = "dashed") +
      labs(title = "β02 vs β12", x= "B02", y = "B12") + 
      theme_minimal()
    
    ggplotly(p1, tooltip = c("x", "y"))
  })
  
  
  output$summary_table <- DT::renderDT({
    req(simulation_results_mvt())
    
    results <- simulation_results_mvt()$data
    true_betas <- simulation_results_mvt()$true_betas
    
    summary_data <- data.frame(
      Parameter = c("β₀₁", "β₁₁", "β₀₂", "β₁₂"),
      True_Value = true_betas,
      Mean_Estimate = c(mean(results$beta01hat), mean(results$beta11hat), 
                        mean(results$beta02hat), mean(results$beta12hat)),
      SD_Estimate = c(sd(results$beta01hat), sd(results$beta11hat),
                      sd(results$beta02hat), sd(results$beta12hat)),
      Bias = c(mean(results$beta01hat) - true_betas[1], 
               mean(results$beta11hat) - true_betas[2],
               mean(results$beta02hat) - true_betas[3],
               mean(results$beta12hat) - true_betas[4])
    )
    
    datatable(summary_data, options = list(dom = 't')) %>%
      formatRound(columns = c('True_Value', 'Mean_Estimate', 'SD_Estimate', 'Bias'), digits = 4)
  })
  
  output$comparison_output <- renderText({
    req(simulation_results_mvt())
    
    results <- simulation_results_mvt()$data
    theoretical_cov <- simulation_results_mvt()$theoretical_cov
    
    empirical_cov <- cov(results[, -1])  
    
    paste0(
      "Expected Covariance Matrix:\n",
      paste(capture.output(print(round(theoretical_cov, 4))), collapse = "\n"),
      "\n\nEmpirical Covariance Matrix:\n", 
      paste(capture.output(print(round(empirical_cov, 4))), collapse = "\n")
    )
  })
  
  output$cov_matrix_display <- renderText({
    req(simulation_results_mvt())
    
    theoretical_cov <- simulation_results_mvt()$theoretical_cov
    
    paste0(
      "Expected Covariance Matrix of β̂:\n",
      paste(capture.output(print(round(theoretical_cov, 4))), collapse = "\n")
    )
  })
  
  
  output$multivariate_marginal_plots <- renderPlot({
    req(simulation_results_mvt())
    results <- simulation_results_mvt()$data
    true_betas <- simulation_results_mvt()$true_betas
    theoretical_cov <- simulation_results_mvt()$theoretical_cov
    
    p1 <- ggplot(results, aes(x = beta01hat)) +
      geom_histogram(bins = 50, alpha = 0.7, fill = "lightblue", color = "black", aes(y = ..density..)) +
      geom_vline(xintercept = true_betas[1], color = "red", linetype = "solid", size = 1) +
      geom_vline(xintercept = mean(results$beta01hat), color = "blue", linetype = "dashed") +
      stat_function(fun = dnorm, args = list(mean = true_betas[1], sd = sqrt(theoretical_cov[1,1])), 
                    color = "black", linetype = "dashed", size = 1) +
      labs(title = "β₀₁ Distribution", x = "β₀₁", y = "Density") +
      theme_minimal()
    
    p2 <- ggplot(results, aes(x = beta11hat)) +
      geom_histogram(bins = 50, alpha = 0.7, fill = "lightgreen", color = "black", aes(y = ..density..)) +
      geom_vline(xintercept = true_betas[2], color = "red", linetype = "solid", size = 1) +
      geom_vline(xintercept = mean(results$beta11hat), color = "blue", linetype = "dashed") +
      stat_function(fun = dnorm, args = list(mean = true_betas[2], sd = sqrt(theoretical_cov[2,2])), 
                    color = "black", linetype = "dashed", size = 1) +
      labs(title = "β₁₁ Distribution", x = "β₁₁", y = "Density") +
      theme_minimal()
    
    p3 <- ggplot(results, aes(x = beta02hat)) +
      geom_histogram(bins = 50, alpha = 0.7, fill = "lightcoral", color = "black", aes(y = ..density..)) +
      geom_vline(xintercept = true_betas[3], color = "red", linetype = "solid", size = 1) +
      geom_vline(xintercept = mean(results$beta02hat), color = "blue", linetype = "dashed") +
      stat_function(fun = dnorm, args = list(mean = true_betas[3], sd = sqrt(theoretical_cov[3,3])), 
                    color = "black", linetype = "dashed", size = 1) +
      labs(title = "β₀₂ Distribution", x = "β₀₂", y = "Density") +
      theme_minimal()
    
    p4 <- ggplot(results, aes(x = beta12hat)) +
      geom_histogram(bins = 50, alpha = 0.7, fill = "lightyellow", color = "black", aes(y = ..density..)) +
      geom_vline(xintercept = true_betas[4], color = "red", linetype = "solid", size = 1) +
      geom_vline(xintercept = mean(results$beta12hat), color = "blue", linetype = "dashed") +
      stat_function(fun = dnorm, args = list(mean = true_betas[4], sd = sqrt(theoretical_cov[4,4])), 
                    color = "black", linetype = "dashed", size = 1) +
      labs(title = "β₁₂ Distribution", x = "β₁₂", y = "Density") +
      theme_minimal()
    
    gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
    
  })
  
  
  # prediction ellipse
  
  observeEvent(input$generate_pred_data, {
    set.seed(123)
    
    n <- input$n_pred
    beta_vals <- parseMatrix(input$betas_mv_pred)
    beta01 <- beta_vals[1, 1]
    beta11 <- beta_vals[1, 2]
    beta02 <- beta_vals[2, 1]
    beta12 <- beta_vals[2, 2]
    
    B <- matrix(c(beta01, beta02, beta11, beta12), nrow = 2)
    
    
    
    epsilon_sigma <- parseMatrix(input$sigmasq_mv_pred)
    
    epsilon_center <- c(0, 0)
    
    epsilon <- mvrnorm(n = n, mu = epsilon_center, Sigma = epsilon_sigma)
    
    x1 <- rnorm(n, mean = 10, sd = 2)
    
    X <- cbind(1, x1)
    
    Y <- X %*% B + epsilon
    
    Y1 <- Y[,1]
    Y2 <- Y[,2]
    
    model <- lm(cbind(Y1, Y2) ~ x1)
    
    coefs <- coef(model)
    
    new_X <- c(1, input$x_new)
    
    Y_hat <- new_X %*% coefs
    
    XtX_inv <- solve(t(X) %*% X)
    
    quad_form <- t(new_X) %*% XtX_inv %*% new_X
    
    pred_cov <- (1 + as.numeric(quad_form)) * epsilon_sigma
    
    ellipse_df <- as.data.frame(ellipse(pred_cov, centre = Y_hat, level = input$conf_level))
    colnames(ellipse_df) <- c("Y1", "Y2")
    
    
    
    alpha <- 1 - input$conf_level
    
    F_val <- qf(1 - alpha, df1 = 2, df2 = n - 2)
    c_val <- 2 * (n - 1) * F_val / (n - 2)
    
    Y1_lower <- Y_hat[1] - sqrt(c_val * pred_cov[1, 1])
    Y1_upper <- Y_hat[1] + sqrt(c_val * pred_cov[1, 1])
    Y2_lower <- Y_hat[2] - sqrt(c_val * pred_cov[2, 2])
    Y2_upper <- Y_hat[2] + sqrt(c_val * pred_cov[2, 2])
    
    p <-ggplot(ellipse_df, aes(x = Y1, y = Y2)) +
      geom_path(color = "blue") +
      geom_point(aes(x = Y_hat[1], y = Y_hat[2]), color = "red", size = 1) +
      geom_rect(aes(xmin = Y1_lower, xmax = Y1_upper,
                    ymin = Y2_lower, ymax = Y2_upper),
                fill = NA, color = "skyblue", linetype = "dashed") +
      coord_fixed() +
      labs(title = "95% Prediction Ellipse with Simultaneous T² Intervals",
           x = "Predicted Y1", y = "Predicted Y2") +
      theme_minimal()
    
    
    output$prediction_plot <- renderPlotly(
      ggplotly(p)
    )
    
    
  })
  
  
  # interpretations
  output$interpretation_uni <- renderText({
    "Increasing the variance of the error term causes the spread to be higher in both Beta0 and Beta1. Increasing the number of observations will make the spread tighter. Try making sense
    of this by looking at the formula on the top of the screen. "
  })
  
  output$interpretation_mv <- renderText({
    "The same observations from the univarate case extend to the multivaraite case. However, adding a covariance term in the covariance matrix for epsilon
    will cause the joint distribution of the betas will be affected. (See equation below and Result 7.9 in textbook)"
  })
  
  
  output$interpretation_pred <- renderText({
    "Adding a covariance term to sigma will make the ellipse diagonal, making the confidence region larger will make the ellipse larger.
    Increasing n will make the ellipse shrink, this might be hard to visually but pay attention to the values in axis of the graph."
  })




}

shinyApp(ui = ui, server = server)