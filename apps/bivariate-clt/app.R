library(shiny)
library(MASS)
library(ggplot2)
library(shinythemes)
library(gridExtra)
library(plotly)

if (FALSE) library(munsell)


ui <- navbarPage("Bivariate CLT Visualizer",
  theme = shinytheme("flatly"),
  tabPanel("Main",
    withMathJax(
    helpText("$$
      \\text{If } X_1, X_2, \\dots, X_n \\text{ are independent observations from any population} 
      \\text{with mean } \\boldsymbol{\\mu} \\text{ and finite covariance } \\boldsymbol{\\Sigma}, \\\\
      \\text{then } \\sqrt{n}(\\bar{X} - \\boldsymbol{\\mu}) \\text{ has an approximate }
      N_p(\\mathbf{0}, \\boldsymbol{\\Sigma}) \\text{ distribution.}
      $$")
    ),
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          sliderInput("m", "Sample size (n):", min = 2, max = 100, value = 2),
          radioButtons("dist", "Original Data Distribution:",
                       choices = list(
                         "Bivariate Normal (μ=[0,0], ρ=0.5)" = "bvnorm_pos",
                         "Bivariate Normal (μ=[2,1], ρ=-0.8)" = "bvnorm_neg", 
                         "Independent Standard Normal" = "indep_norm",
                         "Bivariate Uniform ([0,1]×[0,1])" = "bv_unif",
                         "Bivariate Poisson λ = (2, 2)" = "bv_pois"
                       ),
                       selected = "bvnorm_pos"),
          hr(),
          h4("Theoretical Properties:"),
          verbatimTextOutput("theoretical")
        ),
        mainPanel(
          plotOutput("distPlot", height = "1000px"),
          fluidRow(
            column(12,
                   h4("Interpretation"),
                   textOutput("interpretation")
            )
          ),
          br(),
          br(),
          br()
        )
      )
    )
  ), 
  tabPanel("3D Density Surface",
    fluidPage(
      mainPanel(
        plotlyOutput("density3D", height = "600px")
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
  
  
  generate_sample_means <- function() {
    n_simulations <- 5000
    
    switch(input$dist,
           "bvnorm_pos" = {
             mu <- c(0, 0)
             Sigma <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
             t(replicate(n_simulations, colMeans(mvrnorm(input$m, mu, Sigma))))
           },
           "bvnorm_neg" = {
             mu <- c(2, 1)
             Sigma <- matrix(c(1, -0.8, -0.8, 1), nrow = 2)
             t(replicate(n_simulations, colMeans(mvrnorm(input$m, mu, Sigma))))
           },
           "indep_norm" = {
             mu <- c(0, 0)
             Sigma <- diag(2)
             t(replicate(n_simulations, colMeans(mvrnorm(input$m, mu, Sigma))))
           },
           "bv_unif" = {
             t(replicate(n_simulations, colMeans(cbind(runif(input$m, 0, 1), runif(input$m, 0, 1)))))
           },
           "bv_pois" = {
             t(replicate(n_simulations, colMeans(cbind(rpois(input$m, 2), rpois(input$m, 2)))))
           })
  }
  
  sample_means <- reactive({
    generate_sample_means()
  })
  
  output$distPlot <- renderPlot({
    
    df <- data.frame(X1 = sample_means()[,1], X2 = sample_means()[,2])
    
    
    p1 <- ggplot(df, aes(x = X1, y = X2)) +
      geom_point(alpha = 0.6, color = "steelblue") +
      geom_density_2d(color = "red", alpha = 0.7) +
      xlim(-4, 4) + 
      ylim(-4, 4) + 
      labs(title = paste("Sample Mean Vectors (n =", input$m, ")"),
           x = "Sample Mean of X₁", 
           y = "Sample Mean of X₂") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    p2 <- ggplot(df, aes(x = X1)) +
      geom_histogram(bins = 30, fill = "lightcoral", alpha = 0.7, color = "white") +
      labs(title = "Marginal: Sample Means of X₁", x = "Sample Mean of X₁") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    
    p3 <- ggplot(df, aes(x = X2)) +
      geom_histogram(bins = 30, fill = "lightgreen", alpha = 0.7, color = "white") +
      labs(title = "Marginal: Sample Means of X₂", x = "Sample Mean of X₂") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    grid.arrange(p1, p2, p3, layout_matrix = rbind(c(1,1), c(2,3)))
    
    
  })
  
  
  output$theoretical <- renderText({
    switch(input$dist,
           "bvnorm_pos" = paste0("Population μ = [0, 0]\n",
                                "Population Σ = [[1, 0.5], [0.5, 1]]\n",
                                "CLT:̄ Xbar ~ N([0,0], Σ/", input$m, ")\n",
                                "Expected correlation: 0.5"),
           "bvnorm_neg" = paste0("Population μ = [2, 1]\n",
                                "Population Σ = [[1, -0.3], [-0.3, 1]]\n", 
                                "CLT: Xbar ~ N([2,1], Σ/", input$m, ")\n",
                                "Expected correlation: -0.8"),
           "indep_norm" = paste0("Population μ = [0, 0]\n",
                                "Population Σ = [[1, 0], [0, 1]]\n",
                                "CLT:̄ Xbar ~ N([0,0], I/", input$m, ")\n",
                                "Expected correlation: 0"),
           "bv_unif" = paste0("Population μ = [0.5, 0.5]\n",
                             "Population Σ = [[1/12, 0], [0, 1/12]]\n",
                             "CLT: Xbar ~ N([0.5,0.5], Σ/", input$m, ")\n",
                             "Expected correlation: 0"),
           "bv_pois" = paste0("Population μ = [2, 2]\n",
                              "Population Σ = [[2, 0], [0, 2]]\n",
                              "CLT: Xbar ~ N([2, 2], Σ/", input$m, ")\n",
                              "Expected correlation: 0")
           )
  })
  
  output$density3D <- renderPlotly({
  df <- as.data.frame(sample_means())
  colnames(df) <- c("X1", "X2")
  
  dens <- kde2d(df$X1, df$X2, n = 75)
  
  plot_ly() %>%
    add_surface(x = ~dens$x, y = ~dens$y, z = ~dens$z,
                showscale = TRUE,
                opacity = 0.8,
                colorscale = "Viridis") %>%
    
    add_markers(x = df$X1, y = df$X2, z = rep(0, nrow(df)),
                marker = list(size = 2, color = 'black'),
                name = "Sample Means") %>%
    
    layout(
      title = paste("3D Density of Sample Means (n =", input$m, ")"),
      scene = list(
        xaxis = list(title = "Mean X₁"),
        yaxis = list(title = "Mean X₂"),
        zaxis = list(title = "Density"),
        camera = list(eye = list(x = 1.2, y = 1.2, z = 0.8)) 
      )
    )
  })
  
  output$interpretation <- renderText({
    "Similar to the univariate Central Limit Theorem, as you increase the sample size the joint distribution will look like 
    a bivariate normal shape. The marginal distributions will also look like a normal distribution. In the '3D Density Surface Tab',
    you can observe the 3D surface which is representative of the bivariate normal density."
  })

}

shinyApp(ui, server)