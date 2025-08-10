library(shiny)
library(bslib)
library(tidyverse)
library(patchwork)
library(mvtnorm)
library(MASS)
library(shinythemes)
library(plotly)
library(ggforce)
library(gganimate)
library(ellipse)
library(av)
library(gifski)

if (FALSE) library(munsell)

ui <- navbarPage("Bonferroni, Simultaneous, Ellipses Visualized",
                 theme = shinytheme("flatly"),
                 
                 tabPanel("Main",
                          helpText("Samples, and shows the Simultaneous (T²) Intervals, Confidence Ellipse, and Bonferroni Intervals and coverage probabilities"),
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                sliderInput("sample_size", "Sample Size:", value = 30, min = 10, max = 1000, step = 10),
                                h4("Mean Vector (μ):"),
                                textInput("mu", "μ (e.g., 0, 0):", value = "0, 0"),
                                
                                h4("Covariance Matrix (Σ):"),
                                textInput("sigma", "Σ (row-wise, e.g., 1, 0, 0, 1):", value = "1, 0, 0, 1"),
                                
                                helpText("Insert values with a , as the delimiter"),
                                helpText(HTML("
                                  <b>Legend:</b><br>
                                  <span style='color:black;'>■ Confidence Ellipse</span><br>
                                  <span style='color:skyblue;'>— T² Simultaneous Intervals</span><br>
                                  <span style='color:darkblue;'>-- Bonferroni Intervals</span><br>
                                  <span style='color:black;'>● Sample Mean</span><br>
                                  <span style='color:blue;'>✱ True Mean</span>
                                ")),
                                br(), 
                                helpText(
                                  "Use the tools at the top to move around the plot"
                                ),
                                
                                sliderInput("confidence", "Confidence Level:", value = 0.95, min = 0.90, max = 0.99, step = 0.01),
                                
                                actionButton("resample", "Generate New Sample", class = "btn-primary"),
                                
                                hr(),
                                
                                h4("Coverage Probabilities:"),
                                tableOutput("coverage_table")
                              ),
                              mainPanel(
                                plotlyOutput("distPlot", height = "800px"),
                                hr(),
                                h4("Interval Comparisons:"),
                                tableOutput("interval_table"),
                                fluidRow(
                                  column(12,
                                         h4("Interpretation"),
                                         textOutput("interpretation1")
                                  )
                                ),
                                br(),
                                br(),
                                br()
                              )
                            )
                          )
                 ),
                 tabPanel("Coverage Probability (ellipses)",
                          withMathJax(
                            helpText("$$
                              \\text{Population distribution: } 
                              N \\left( 
                                \\begin{bmatrix}
                                  2 \\\\
                                  5
                                \\end{bmatrix}, 
                                \\begin{bmatrix}
                                  3 & 1 \\\\
                                  1 & 4
                                \\end{bmatrix}
                              \\right)
                            $$")
                          ),
                          helpText("Drawing 20 ellipses, with a sample size n = 30"),
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                sliderInput("confidence_tab2", "Confidence Level:", value = 0.95, min = 0.90, max = 0.99, step = 0.01),
                                actionButton("generate", "Generate Ellipses"),
                                helpText("Press the button to keep resampling (might take some time to load animation)")
                            
                              ),
                              mainPanel(
                                fluidRow(
                                  column(6, 
                                         plotOutput("ellipsePlot", height = "600px")
                                  ),
                                  column(6,
                                         imageOutput("ellipseGIF", width = "100%", height = "auto")
                                  )
                                ),
                                verbatimTextOutput("coverageCount"), 
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
                      )
                 ),
                 tabPanel("Coverage Probabilities (Bonferroni)",
                          withMathJax(
                            helpText("$$
                              \\text{Population distribution: } 
                              N \\left( 
                                \\begin{bmatrix}
                                  2 \\\\
                                  5
                                \\end{bmatrix}, 
                                \\begin{bmatrix}
                                  3 & 1 \\\\
                                  1 & 4
                                \\end{bmatrix}
                              \\right)
                            $$")
                          ),
                          helpText("Drawing 20 intervals, with a sample size of n = 30"),
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                sliderInput("confidence_tab3", "Confidence Level:", value = 0.95, min = 0.90, max = 0.99, step = 0.01),
                                actionButton("generate2", "Generate Intervals"),
                                helpText("Press the button to keep resampling (might take some time to load animation)")
                                
                              ),
                              mainPanel(
                                fluidRow(
                                  column(6, 
                                         plotOutput("intervalPlot", height = "600px")
                                  ),
                                  column(6,
                                         imageOutput("intervalGIF", width = "100%", height = "auto")
                                  )
                                ),
                                verbatimTextOutput("coverageCount2"),
                                fluidRow(
                                  column(12,
                                         h4("Interpretation"),
                                         textOutput("interpretation3")
                                  )
                                ),
                                br(),
                                br(),
                                br()
                              )
                            )
                          )
                   
                 ),
                 tabPanel("Coverage Probabilities (T^2)",
                          withMathJax(
                            helpText("$$
                              \\text{Population distribution: } 
                              N \\left( 
                                \\begin{bmatrix}
                                  2 \\\\
                                  5
                                \\end{bmatrix}, 
                                \\begin{bmatrix}
                                  3 & 1 \\\\
                                  1 & 4
                                \\end{bmatrix}
                              \\right)
                            $$")
                          ),
                          helpText("Drawing 20 intervals, with a sample size of n = 30"),
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                sliderInput("confidence_tab4", "Confidence Level:", value = 0.95, min = 0.90, max = 0.99, step = 0.01),
                                actionButton("generate3", "Generate Intervals"),
                                helpText("Press the button to keep resampling (might take some time to load animation)")
                                
                              ),
                              mainPanel(
                                fluidRow(
                                  column(6, 
                                         plotOutput("intervalPlot2", height = "600px")
                                  ),
                                  column(6,
                                         imageOutput("intervalGIF2", width = "100%", height = "auto")
                                  )
                                ),
                                verbatimTextOutput("coverageCount3"),
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
  
  
  output$bottom_progress <- renderUI({NULL})

  
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(event_data("plotly_relayout", source = "distPlot"), {
    ed <- event_data("plotly_relayout", source = "distPlot")
    
    if (!is.null(ed[["xaxis.range[0]"]]) && !is.null(ed[["xaxis.range[1]"]])) {
      ranges$x <- c(ed[["xaxis.range[0]"]], ed[["xaxis.range[1]"]])
    }
    
    if (!is.null(ed[["yaxis.range[0]"]]) && !is.null(ed[["yaxis.range[1]"]])) {
      ranges$y <- c(ed[["yaxis.range[0]"]], ed[["yaxis.range[1]"]])
    }
  })
  
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
    result <- parseVector(input$mu)
    if (is.null(result)) c(0, 0) else result
  })
  
  sigma <- reactive({
    result <- parseMatrix(input$sigma)
    if (is.null(result)) matrix(c(1, 0, 0, 1), nrow = 2) else result
  })
  
  sample_data <- reactiveVal(NULL)
  
  observe({
    if (is.null(sample_data())) {
      set.seed(123)
      data <- mvrnorm(n = input$sample_size, mu = mu(), Sigma = sigma())
      sample_data(data)
    }
  })
  
  observeEvent(c(input$resample, input$sample_size, input$mu, input$sigma), {
    data <- mvrnorm(n = input$sample_size, mu = mu(), Sigma = sigma())
    sample_data(data)
  })
  
  stats <- reactive({
    data <- sample_data()
    if (is.null(data)) return(NULL)
    
    n <- nrow(data)
    p <- ncol(data)
    xbar <- colMeans(data)
    S <- cov(data)
    
    alpha <- 1 - input$confidence
    
    t2_crit <- ((n-1) * p / (n-p)) * qf(input$confidence, p, n-p)
    
    bonf_crit <- qt(1 - alpha/(2*p), n-1)
    
    se <- sqrt(diag(S) / n)
    
    list(
      n = n, p = p, xbar = xbar, S = S, se = se,
      t2_crit = t2_crit, bonf_crit = bonf_crit,
      alpha = alpha
    )
  })
  
  output$distPlot <- renderPlotly({
    req(sample_data(), stats())
    
    alpha <- 1 - input$confidence_tab2
    
    data <- sample_data()
    s <- stats()
    
    df <- data.frame(X1 = data[,1], X2 = data[,2])
    
    t2_half_width <- sqrt(s$t2_crit * s$se^2)
    bonf_half_width <- s$bonf_crit * s$se
    
    t2_x_lower <- s$xbar[1] - t2_half_width[1]
    t2_x_upper <- s$xbar[1] + t2_half_width[1]
    t2_y_lower <- s$xbar[2] - t2_half_width[2]
    t2_y_upper <- s$xbar[2] + t2_half_width[2]
    
    bonf_x_lower <- s$xbar[1] - bonf_half_width[1]
    bonf_x_upper <- s$xbar[1] + bonf_half_width[1]
    bonf_y_lower <- s$xbar[2] - bonf_half_width[2]
    bonf_y_upper <- s$xbar[2] + bonf_half_width[2]
    
    x_range <- c(t2_x_lower, t2_x_upper)
    y_range <- c(t2_y_lower, t2_y_upper)
    x_extend <- diff(x_range) * 0.15
    y_extend <- diff(y_range) * 0.15
    
    ellipse_data <- ellipse(s$S/s$n, centre = s$xbar, 
                            level = input$confidence, npoints = 100)
    ellipse_df <- data.frame(X1 = ellipse_data[,1], X2 = ellipse_data[,2])
    
    default_xmin = x_range[1] - x_extend
    default_xmax = x_range[2] + x_extend
    
    default_ymin = y_range[1] - y_extend
    default_ymax = y_range[2] + y_extend
    p1 <- ggplot(df, aes(x = X1, y = X2)) +
      coord_cartesian(
        xlim = if (!is.null(ranges$x)) ranges$x else c(default_xmin, default_xmax),
        ylim = if (!is.null(ranges$y)) ranges$y else c(default_ymin, default_ymax)
      ) + 
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 14)
      ) +
      labs(title = paste("Confidence Regions Comparison (n =", s$n, ")"),
           subtitle = paste("Confidence Level:", 100*input$confidence, "%"),
           x = "μ₁",
           y = "μ₂")
    
    p1 <- p1 + 
      geom_path(data = ellipse_df, aes(x = X1, y = X2), 
                color = "black", size = 1.2, linetype = "solid")
    
    p1 <- p1 + 
      geom_vline(xintercept = t2_x_lower, color = "skyblue", size = 1.2, linetype = "solid") +
      geom_vline(xintercept = t2_x_upper, color = "skyblue", size = 1.2, linetype = "solid")
    
    p1 <- p1 + 
      geom_hline(yintercept = t2_y_lower, color = "skyblue", size = 1.2, linetype = "solid") +
      geom_hline(yintercept = t2_y_upper, color = "skyblue", size = 1.2, linetype = "solid")
    
    p1 <- p1 + 
      geom_vline(xintercept = bonf_x_lower, color = "darkblue", size = 1, linetype = "dashed") +
      geom_vline(xintercept = bonf_x_upper, color = "darkblue", size = 1, linetype = "dashed") +
      geom_hline(yintercept = bonf_y_lower, color = "darkblue", size = 1, linetype = "dashed") +
      geom_hline(yintercept = bonf_y_upper, color = "darkblue", size = 1, linetype = "dashed")
    
    p1 <- p1 + 
      geom_point(aes(x = s$xbar[1], y = s$xbar[2]), 
                 color = "black", size = 3, shape = 19)
    
    true_mu <- mu()
    p1 <- p1 + 
      geom_point(aes(x = true_mu[1], y = true_mu[2]), 
                 color = "blue", size = 3, shape = 8)
    
    legend_x <- x_range[1] + diff(x_range) * 0.02
    legend_y_start <- y_range[2] - diff(y_range) * 0.05
    legend_spacing <- diff(y_range) * 0.08
    
    ggplotly(p1, source = "distPlot")
  })
  
  # Intervals comparison table
  output$interval_table <- renderTable({
    req(stats())
    s <- stats()
    
    t2_half_width <- sqrt(s$t2_crit * s$se^2)
    bonf_half_width <- s$bonf_crit * s$se
    
    intervals_df <- data.frame(
      Method = rep(c("T² Simultaneous", "Bonferroni"), each = 2),
      Variable = rep(c("X₁", "X₂"), 2),
      Lower = c(s$xbar - t2_half_width, s$xbar - bonf_half_width),
      Upper = c(s$xbar + t2_half_width, s$xbar + bonf_half_width),
      Width = c(2 * t2_half_width, 2 * bonf_half_width)
    )
    
    intervals_df$Lower <- round(intervals_df$Lower, 3)
    intervals_df$Upper <- round(intervals_df$Upper, 3)
    intervals_df$Width <- round(intervals_df$Width, 3)
    
    intervals_df
  }, striped = TRUE, hover = TRUE)
  
  output$coverage_table <- renderTable({
    req(stats())
    
    set.seed(42)
    n_sim <- 1000
    coverage_results <- replicate(n_sim, {

      n <- input$sample_size
      sim_data <- mvrnorm(n, mu = mu(), Sigma = sigma())
      sim_xbar <- colMeans(sim_data)
      sim_S <- cov(sim_data)
      sim_se <- sqrt(diag(sim_S) / input$sample_size)
      p <- length(mu())
      

      alpha <- 1 - input$confidence
      t2_crit <- ((n-1) * p / (n-p)) * qf(input$confidence, p, n-p)
      bonf_crit <- qt(1 - alpha/(2*p), n-1)
      
      t2_half_width <- sqrt(t2_crit * sim_se^2)
      bonf_half_width <- bonf_crit * sim_se
      
      true_mu <- mu()
      t2_covers <- all(abs(sim_xbar - true_mu) <= t2_half_width)
      bonf_covers <- all(abs(sim_xbar - true_mu) <= bonf_half_width)
      
      t2_stat <-  as.numeric(input$sample_size * t(sim_xbar - true_mu) %*% solve(sim_S) %*% (sim_xbar - true_mu))
      ellipse_covers <- t2_stat <= t2_crit
      
      c(ellipse_covers, t2_covers, bonf_covers)
    })
    
    coverage_probs <- rowMeans(coverage_results)
    
    data.frame(
      Method = c("Confidence Ellipse", "T² Simultaneous", "Bonferroni"),
      `Theoretical Coverage` = rep(input$confidence, 3),
      `Simulated Coverage` = round(coverage_probs, 3),
      `Difference` = round(coverage_probs - input$confidence, 3),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE)
  
  generate_ellipse_points <- function(x0, y0, a, b, angle_deg, n_points = 100) {
    theta <- seq(0, 2 * pi, length.out = n_points)
    angle_rad <- angle_deg * pi/ 100
    
    x_std <- a * cos(theta)
    y_std <- b * sin(theta)
    
    x <- x0 + x_std * cos(angle_rad) - y_std * sin(angle_rad)
    y <- y0 + x_std * sin(angle_rad) + y_std * cos(angle_rad)
    
    return (data.frame(x = x, y = y))
  }
  # Tab 2
  
  ellipse_gif_path <- reactiveVal(NULL)
  
  observeEvent(input$generate, {
    n <- 100
    num <- 20
    true_mu <- c(2, 5)
    alpha <- 1 - input$confidence_tab2
    true_Sigma <- matrix(c(3, 1, 1, 4), nrow = 2)
    ellipses <- list()
    coverage <- logical(num)
    
    
    for (i in 1:num) {
      sample_data <- mvrnorm(n = n, mu = true_mu, Sigma = true_Sigma)
      xbar <- colMeans(sample_data)
      S <- cov(sample_data)
      eig <- eigen(S/n)
      
      radius <- sqrt(qchisq(1 - alpha, df = 2))
      mdist2 <- mahalanobis(true_mu, center = xbar, cov = S/n)
      covered = ifelse(mdist2 <= qchisq(1 - alpha, 2), "Covered", "Missed")
      
      ellipse_points <- generate_ellipse_points(
        x0 = xbar[1],
        y0 = xbar[2],
        a = radius * sqrt(eigen(S/n)$values[1]),
        b = radius * sqrt(eigen(S/n)$values[2]),
        angle = atan2(eig$vectors[2, 1], eig$vectors[1, 1]) * 180 / pi
      )
      
      ellipse_points$id <- i
      ellipse_points$covered <- covered
      coverage[i] = ifelse(covered == "Covered", TRUE, FALSE)
      ellipse_points$frame <- i
      ellipses[[i]] <- ellipse_points
    }
    

    
    ellipse_df <- do.call(rbind, ellipses)
    
    p <-  ggplot(ellipse_df) +
      geom_point(aes(x = true_mu[1], y = true_mu[2]), color = "darkblue", size = 4, shape = 4) +
      annotate("text", x = true_mu[1] + 0.1, y = true_mu[2] + 0.1, 
               label = "True Mean", color = "darkblue", hjust = 0, size = 4) + 
      geom_path(aes(x = x, y = y, group = id, color = covered),
                   alpha = 1) +
      scale_color_manual(values = c("Covered" = "skyblue", "Missed" = "orange")) +
      coord_fixed() +
      labs(title = "Confidence Ellipses and True Mean",
           x = "X1", y = "X2", color = "Coverage") +
      theme_minimal()
    
    output$ellipsePlot <- renderPlot({
      p
    })
    
    panim <- p + 
      transition_states(frame, transition_length = 2, state_length = 20) +
      shadow_mark(past = TRUE, alpha = 0.2)

    anim <- animate(panim, nframes = num, res = 120, fps = 5, render = gifski_renderer())
    gif_path <- file.path(tempdir(), "ellipse_animation.gif")
    anim_save(gif_path, animation = anim)
    ellipse_gif_path(gif_path)
    
    output$ellipseGIF <- renderImage({
      list(src = gif_path,
           contentType = 'image/gif',
           width = 550, 
           height = 600,
           alt = 'Ellipse Animation')
    }, deleteFile = FALSE)
    
    output$coverageCount <- renderText({
      paste(sum(coverage), "out of", num, "ellipses contain the true mean.")
    })
  })
  # session$onSessionEnded(function() {
  #   for (path in c(ellipse_gif_path(), bonf_gif_path(), t2_gif_path())) {
  #     if (!is.null(path) && file.exists(path)) {
  #       file.remove(path)
  #     }
  #   }
  # })
  
  bonf_gif_path <- reactiveVal(NULL)
  t2_gif_path   <- reactiveVal(NULL)
  
  # tab 3
  observeEvent(input$generate2, {
    num <- 20
    n <- 30
    p <- 2
    alpha <- 1 - input$confidence_tab3
    true_mu <- c(2, 5)
    true_Sigma <- matrix(c(3, 1, 1, 4), nrow = 2)
    bonf_data <- data.frame()
    coverage <- logical(num)
    for (i in 1:num) {
      sample_data <- mvrnorm(n, mu = true_mu, Sigma = true_Sigma)
      xbar <- colMeans(sample_data)
      S <- cov(sample_data)
      se <- sqrt(diag(S) / n)

      tval <- qt(1 - alpha/(2*p), n - 1)

      lower <- xbar - tval * se
      upper <- xbar + tval * se
      cover_x1 <- lower[1] <= true_mu[1] && true_mu[1] <= upper[1]
      cover_x2 <- lower[2] <= true_mu[2] && true_mu[2] <= upper[2]
      coverage[i] <- cover_x1 & cover_x2
      bonf_data <- rbind(bonf_data,
        data.frame(
          Sample = i,
          Variable = "X1",
          Est = xbar[1],
          Lower = lower[1],
          Upper = upper[1],
          Missed = !cover_x1,
          Frame = i
        ),
        data.frame(
          Sample = i,
          Variable = "X2",
          Est = xbar[2],
          Lower = lower[2],
          Upper = upper[2],
          Missed = !cover_x2,
          Frame = i
        )
      )
    }
    bonf_data$Missed <- factor(bonf_data$Missed, levels = c(TRUE, FALSE), labels = c("Missed", "Covered"))
    
    p <- ggplot(bonf_data, aes(x = Sample, y = Est, color = Missed)) +
      geom_point(size = 2) +
      geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
      facet_wrap(~Variable, ncol = 1) + 
      geom_hline(data = data.frame(Variable = c("X1", "X2"), 
                                   mu = true_mu), 
                 aes(yintercept = mu), color = "black", linetype = "dashed") +
      scale_color_manual(values = c("Covered" = "skyblue", "Missed" = "orange")) +
      labs(title = "Bonferroni Intervals for Means",
           x = "Sample",
           y = "CI", color = "Coverage") +
      theme_minimal() +
      theme(strip.text = element_text(size = 14),
            plot.title = element_text(hjust = 0.5))

    output$intervalPlot <- renderPlot({
     p
    })
    
    panim_bonf <- p + transition_states(Frame, transition_length = 1, state_length = 1) + 
      shadow_mark(past = TRUE, alpha = 0.3)
    anim_bonf <- animate(panim_bonf, nframes = n, fps = 5, res = 120, width = 500, height = 600)
    gif_bonf_path <- file.path(tempdir(), "bonferroni_animation.gif")
    
    bonf_gif_path(gif_bonf_path)
    anim_save(gif_bonf_path, animation = anim_bonf)
    
    output$intervalGIF <- renderImage({
      list(src = gif_bonf_path,
           contentType = 'image/gif',
           width = 500,
           height = 600,
           alt = 'Bonferroni Animation')
    }, deleteFile = FALSE)
    
    output$coverageCount2 <- renderText({
      paste(sum(coverage), "out of", num, "Bonferroni intervals contain the true mean.")
    })
  })
  
  
  observeEvent(input$generate3, {
    num <- 20
    n <- 30
    p <- 2 
    alpha <- 1 - input$confidence_tab4

    true_mu <- c(2, 5)
    true_Sigma <- matrix(c(3, 1, 1, 4), nrow = 2)
    t2_data <- data.frame()
    coverage <- logical(num) 

    for (i in 1:num) {
      sample_data <- mvrnorm(n, mu = true_mu, Sigma = true_Sigma)
      xbar <- colMeans(sample_data)
      S <- cov(sample_data)
      se <- sqrt(diag(S) / n)

      f_crit <- (p * (n - 1) / (n - p)) * qf(1 - alpha, df1 = p, df2 = n - p)
      me <- sqrt(f_crit * se^2)

      lower <- xbar - me
      upper <- xbar + me

      cover_x1 <- lower[1] <= true_mu[1] && true_mu[1] <= upper[1]
      cover_x2 <- lower[2] <= true_mu[2] && true_mu[2] <= upper[2]
      coverage[i] <- cover_x1 && cover_x2

      t2_data <- rbind(
        t2_data,
        data.frame(Sample = i, Variable = "X1", Est = xbar[1], Lower = lower[1], Upper = upper[1], Missed = !cover_x1, Frame = i),
        data.frame(Sample = i, Variable = "X2", Est = xbar[2], Lower = lower[2], Upper = upper[2], Missed = !cover_x2, Frame = i)
      )
    }

    t2_data$Missed <- factor(t2_data$Missed, levels = c(TRUE, FALSE), labels = c("Missed", "Covered"))

    p <- ggplot(t2_data, aes(x = Sample, y = Est, color = Missed)) +
      geom_point(size = 2) +
      geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
      facet_wrap(~Variable, ncol = 1) + 
      geom_hline(data = data.frame(Variable = c("X1", "X2"), 
                                   mu = true_mu), 
                 aes(yintercept = mu), color = "black", linetype = "dashed") +
      scale_color_manual(values = c("Covered" = "skyblue", "Missed" = "orange")) +
      labs(title = "T² Simultaneous Intervals for Means",
           x = "Sample",
           y = "CI", color = "Coverage") +
      theme_minimal() +
      theme(strip.text = element_text(size = 14),
            plot.title = element_text(hjust = 0.5))
    output$intervalPlot2 <- renderPlot({
      p
    })
    panim_t2 <- p + transition_states(Frame, transition_length = 1, state_length = 1) + 
      shadow_mark(past = TRUE, alpha = 0.3)
    anim_t2 <- animate(panim_t2, nframes = n, fps = 5, res = 120, width = 500, height = 600)
    gif_t2_path <- file.path(tempdir(), "t2_animation.gif")
    anim_save(gif_t2_path, animation = anim_t2)
    
    t2_gif_path(gif_t2_path)
    
    output$intervalGIF2 <- renderImage({
      list(src = gif_t2_path,
           contentType = 'image/gif',
           width = 500,
           height = 600,
           alt = 'T² Animation')
    }, deleteFile = FALSE)
    output$coverageCount3 <- renderText({
      paste(sum(coverage), "out of", num, "T² intervals contain the true mean.")
    })
    
    # Interpretations
    
  })
  output$interpretation1 <- renderText({
    "Increasing the sample size will make the ellipse and intervals narrower and closer together. If you zoom out of the plot, and keep generating new samples
      you will notice some intervals may not capture the true mean. Increasing the confidence level will make the ellipses/intervals wider as well."
  })
  
  output$interpretation2 <- renderText({
    conf_level <- input$confidence_tab2 * 100
    paste0(
    "Just like generating confidence intervals in the univariate case, not every sample-based confidence ellipse will contain the true mean. If we keep drawing samples an infinite amount of times,
    the ", round(conf_level, 1),  "% confidence ellipse will cover the true mean ", round(conf_level, 1), "% of the time.")
  })
  
  output$interpretation3 <- renderText({
    conf_level <- input$confidence_tab3 * 100
    paste0(
    "Bonferroni intervals are narrower than T^2 ones. When drawing an infinite amount of samples, about ", round(conf_level, 1), "% of the constructed Bonferroni intervals for each variable
    will individually contain their true means. However, the joint probability that both intervals simultaneously cover the true means is slightly less than ", round(conf_level, 1), "%.")
  })
  output$interpretation4 <- renderText({
    conf_level <- input$confidence_tab4 * 100
    paste0(
    "Simultaneous (T^2) Intervals are wider than the Bonferroni ones and are more likely to cover the true mean. These intervals are constructed so that if we sample
    an infinite amount of times, approximately ", round(conf_level, 1), "% of the ellipses will jointly enclose the true mean vector. Unlike Bonferroni, this method explicity controls the
    overall coverage probability for both variables at once.")
  })
}

shinyApp(ui = ui, server = server)