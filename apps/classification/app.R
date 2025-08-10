library(shiny)
library(tidyverse)
library(MASS)
library(shinythemes)


if (FALSE) library(munsell)

ui <- navbarPage("Classification Dashboard",
                 theme = shinytheme("flatly"),
                 tabPanel("Classification of Normal Population",
                    withMathJax(
                      helpText("$$
                        a = \\Sigma^{-1}(\\mu_1 - \\mu2) \\hspace{2em} \\text{$\\Sigma = $ pooled covariance matrix} \\\\
                        y = a^{T}x
                        $$")
                    ),
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          h3("Data Generation Parameters"),
                          numericInput("n1", "Sample Size for Group 1", value = 100, min = 10, max = 1000),
                          numericInput("n2", "Sample Size for Group 2", value = 100, min = 10, max = 1000),
                          
                          h4("Group 1"),
                          textInput("mu_g1", "Mean of Group 1: eg(4, 3)", value = "4, 3"),
                          numericInput("cov_g1","Covariance for Group 1 (has to be less than 3)", value = 0, step = 0.1, max = 3),
                          
                          h4("Group 2"),
                          textInput("mu_g2", "Mean of Group 2: eg(9, 5)", value = "9, 5"),
                          numericInput("cov_g2", "Covariance for Group 2 (has to be less than 3)", value = 0, step = 0.1, max = 3),
                          
                          br(),
                          actionButton("generate", "Generate New Data", class = "btn-primary"),
                          actionButton("classify", "Apply Linear Discriminant", class = "btn-success"),
                          h4("Classify New Point"),
                          textInput("new_point", "New Point (x, y)", value = "5, 5"),
                          actionButton("classify_new", "Classify New Point", class = "btn-warning"),
                          verbatimTextOutput("new_point_result"),
                          br(),
                          br(),
                          br()
                          
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Scatter Plot",
                                     plotOutput("scatterplot", height = '600px'),
                                     h4("Classification Results"),
                                     verbatimTextOutput("accuracy"),
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
                            tabPanel("Fisher's Approach Details",
                                     h4("Linear Discriminant Function"),
                                     verbatimTextOutput("lda_function"),
                                     h4("Discriminant Coefficients"),
                                     verbatimTextOutput("lda_coefficients"),
                                     h4("Group Means"),
                                     verbatimTextOutput("group_means"),
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
                            tabPanel("Projected Points", 
                                     plotOutput("projection_plot", height = "600px"),
                                     plotOutput("histogram_plot", height = "600px"),
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
  values <- reactiveValues(
    data = NULL,
    lda_model = NULL,
    lda_applied = FALSE
  )
  
  observeEvent(input$generate, {
    generate_data()
  })
  
  
  observe({
    req(input$n1, input$n2)             
    req(input$mu_g1, input$mu_g2)        
    req(input$cov_g1, input$cov_g2)     
    
    mu1_vals <- as.numeric(unlist(strsplit(input$mu_g1, ",")))
    mu2_vals <- as.numeric(unlist(strsplit(input$mu_g2, ",")))
    
    validate(
      need(length(mu1_vals) == 2 && all(!is.na(mu1_vals)), "Enter 2 numbers for Group 1 mean."),
      need(length(mu2_vals) == 2 && all(!is.na(mu2_vals)), "Enter 2 numbers for Group 2 mean.")
    )
    
    generate_data()
    
    generate_data()
  })
  
  observe({
    if (is.null(values$data)) {
      generate_data()
    }
  })
  
  parseVector <- function(input_str) {
    vals <- as.numeric(unlist(strsplit(input_str, ",")))
    if (length(vals) != 2 || any(is.na(vals))) return(NULL)
    vals
  }
  
  generate_data <- function() {
    cov1 <- matrix(c(3, input$cov_g1, input$cov_g1, 3), nrow = 2)
    cov2 <- matrix(c(3, input$cov_g2, input$cov_g2, 3), nrow = 2)
    
    mu1 <- parseVector(input$mu_g1)
    mu2 <- parseVector(input$mu_g2)
    
    group1 <- mvrnorm(input$n1, mu = mu1, Sigma = cov1)
    group2 <- mvrnorm(input$n2, mu = mu2, Sigma = cov2)
    
    data <- data.frame(
      x = c(group1[, 1], group2[, 1]),
      y = c(group1[, 2], group2[, 2]),
      group = factor(c(rep("Group 1", input$n1), rep("Group 2", input$n2)))
    )
    
    values$data <- data
    values$lda_applied <- FALSE
    values$lda_model <- NULL
  }
  
  observeEvent(input$classify_new, {
    req(values$lda_applied, values$lda_model)
    
    value <- parseVector(input$new_point)
    
    new_point <- data.frame(
      x = value[1],
      y = value[2]
    )
    
    pred <- predict(values$lda_model, new_point)
    
    output$new_point_result <- renderText({
      paste0("New point classified as: ", pred$class)
    })
    values$new_point <- new_point
    values$new_point$predicted_class <- pred$class
    
  })
    
  
  observeEvent(input$classify, {
    if(!is.null(values$data)) {
      lda_model <- lda(group ~ x + y, data = values$data)
      
      predictions <- predict(lda_model, values$data)
      
      values$data$predicted <- predictions$class
      values$data$ld1 <- predictions$x[, 1]
      
      values$lda_model <- lda_model
      values$lda_applied <- TRUE
    }
  })
  output$scatterplot <- renderPlot({
    if (is.null(values$data)) return()
    
    p <- ggplot(values$data, aes(x=x, y=y)) +
    	geom_point(aes(color = group, shape = group), size = 3, alpha = 0.9) +
    	scale_color_manual(values = c("Group 1" = "darkorchid1", "Group 2" = "aquamarine2")) +
    	scale_shape_manual(values = c("Group 1" = 19, "Group 2" = 17)) +
     	labs(
        x = "x",
    	  y = "y",
    	  title = "Bivariate Normal Classification",
    	  color = "Group",
    	  shape = "Group"
    	) +
    	theme_minimal() + 
    	theme(
    	  legend.position = "bottom",
    	  plot.title = element_text(hjust = 0.5, size = 16),
    	  axis.title = element_text(size = 12),
    	  legend.title = element_text(size = 12)
    	)
    if (nrow(values$data) > 0) {
      p <- p + stat_ellipse(aes(color = group), level = 0.9, type = "norm", linetype = "dashed")
    }
    
    
    if(values$lda_applied && !is.null(values$lda_model)) {
      
      mu1 <- values$lda_model$means[1, ]
      mu2 <- values$lda_model$means[2, ]
      
      pooled_cov <- cov(values$data[values$data$group == "Group 1", 1:2]) * (input$n1 - 1) +
        cov(values$data[values$data$group == "Group 2", 1:2]) * (input$n2 - 1)
      pooled_cov <- pooled_cov / (input$n1 + input$n2 - 2)
      
      inv_S <- solve(pooled_cov)
      a_vec <- inv_S %*% (mu1 - mu2)
      
      prior1 <- input$n1 / (input$n1 + input$n2)
      prior2 <- input$n2 / (input$n1 + input$n2)
      
      intercept_val <- 0.5 * (t(mu1) %*% inv_S %*% mu1 - 
                                t(mu2) %*% inv_S %*% mu2) - log(prior1 / prior2)
      
      slope <- -a_vec[1] / a_vec[2]
      intercept <- intercept_val / a_vec[2]
      
      p <- p + geom_abline(slope = slope, intercept = intercept,
                           color = "red", size = 1.2, linetype = "solid") + 
        annotate("text", x = Inf, y = Inf, label = "Fisher's Discriminant Line",
                 hjust = 1.1, vjust = 2, color = "red", size = 4)
      
      if ("predicted" %in% names(values$data)) {
        misclassified <- values$data[values$data$group != values$data$predicted, ]
        if (nrow(misclassified) > 0) {
          p <- p + geom_point(data = misclassified, aes(x=x, y=y),
                              color = "red", size = 3, shape = 4, stroke = 1)
        }
      }
    }
    
    
    if (!is.null(values$new_point)) {
      p <- p + geom_point(data = values$new_point, aes(x = x, y = y),
                          fill = ifelse(values$new_point$predicted_class == "Group 1", "darkorchid1", "aquamarine2"),
                          color = "black",
                          size = 3, shape = ifelse(values$new_point$predicted_class == "Group 1", 21, 24), stroke = 1.5) 
    }
    p
    
	})	
  output$lda_function <- renderText({
    if (!values$lda_applied || is.null(values$lda_model)) {
      return("Apply Fisher's LDA to see the discriminant function")
    }
    coef <- values$lda_model$scaling
    paste0("Linear Discriminant Function: \n",
           "LD = ", round(coef[1], 4), "x + ", round(coef[2], 4), "y")
  })
  
  output$accuracy <- renderText({
    if(!values$lda_applied || !("predicted" %in% names(values$data))) {
      return("Apply Fisher's LDA to see classification accuracy.")
    }
    
    accuracy <- mean(values$data$group == values$data$predicted)
    confusion <- table(Predicted = values$data$predicted, Actual = values$data$group)
    
    confusion_str <- apply(confusion, 1, function(row) paste(row, collapse = "\t"))
    confusion_output <- paste(
      "           \tGroup 1 Group 2",
      paste("Predicted Group 1:", confusion_str[1]),
      paste("Predicted Group 2:", confusion_str[2]),
      sep = "\n"
    )
    
    paste0(
      "Classification Accuracy: ", round(accuracy * 100, 2), "%\n\n",
      "Confusion Matrix:\n", confusion_output
    )
  })
  
  output$lda_coefficients <- renderText({
    if(!values$lda_applied || is.null(values$lda_model)) {
      return("Apply Fisher's LDA to see coefficients.")
    }
    
    coef <- values$lda_model$scaling
    paste0("Coefficient for X: ", round(coef[1], 4), "\n",
           "Coefficient for Y: ", round(coef[2], 4))
  })
  
  output$group_means <- renderText({
    if(!values$lda_applied || is.null(values$lda_model)) {
      return("Apply Fisher's LDA to see group means.")
    }
    
    means <- values$lda_model$means
    paste0("Group Means in discriminant Space: \n",
           "X: ", round(means[1, 1], 4), ", ", round(means[1, 2], 4), "\n",
           "Y: ", round(means[2, 1], 4), ", ", round(means[2, 2], 4))
  }) 
  
  observeEvent(input$classify, {
    if (!is.null(values$data)) {
      lda_model <- lda(group ~ x + y, data = values$data)
      
      predictions <- predict(lda_model, values$data)
      
      S_pooled <- lda_model$scaling %*% t(lda_model$scaling) 
      mean_diff <- colMeans(values$data[values$data$group == "Group 1", 1:2]) - 
        colMeans(values$data[values$data$group == "Group 2", 1:2])
      a_vec <- lda_model$scaling  
      
      fisher_projection <- as.matrix(values$data[, 1:2]) %*% a_vec  
      values$data$projected <- fisher_projection  
      
      values$data$predicted <- predictions$class
      values$data$ld1 <- predictions$x[, 1]  
      
      values$lda_model <- lda_model
      values$lda_applied <- TRUE
    }
  })
  
  
  output$projection_plot <- renderPlot({
    validate(
      need(values$lda_applied, "Apply Linear Discriminant to see the projection plot."),
      need(!is.null(values$data$projected), "Projected data isn't available")
    )
    
    projected_data <- data.frame(
      LD1 = values$data$projected,
      group = values$data$group
    )
    
    means <- projected_data %>%
      group_by(group) %>%
      summarise(mean_LD1 = mean(LD1), .groups = "drop")
    
    y1bar <- means$mean_LD1[means$group == "Group 1"]
    y2bar <- means$mean_LD1[means$group == "Group 2"]
    cutoff <- 0.5 * (y1bar + y2bar)
    
    ggplot(projected_data, aes(x = LD1, y = 0, color = group)) +
      geom_jitter(aes(y=0), height = 0.05, width = 0, size = 2, alpha = 0.7) +
      geom_vline(xintercept = cutoff, color = "red", linetype = "dashed", size = 1) +
      annotate("text", x = cutoff, y = 0.2,
               label = paste0("Cutoff = ", round(cutoff, 2)),
               color = "red", angle = 90, vjust = -0.5) +
      labs(
        title = "Fisher’s Transformation – Points Projected on Discriminant Line",
        x = "Projection y = aᵀx",
        y = NULL
      ) +
      scale_color_manual(values = c("Group 1" = "darkorchid3", "Group 2" = "aquamarine2")) +
      theme_minimal() +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  })
  
  
  output$histogram_plot <- renderPlot({
    validate(
      need(values$lda_applied, "Apply Linear Discriminant to see the projection plot."),
      need(!is.null(values$data$projected), "Projected data isn't available")
    )
    
    projected_data <- data.frame(
      LD1 = values$data$projected,
      group = values$data$group
    )
    
    means <- projected_data %>%
      group_by(group) %>%
      summarise(mean_LD1 = mean(LD1), .groups = "drop")
    
    y1bar <- means$mean_LD1[means$group == "Group 1"]
    y2bar <- means$mean_LD1[means$group == "Group 2"]
    cutoff <- 0.5 * (y1bar + y2bar)
    
    ggplot(projected_data, aes(x = LD1, fill = group)) +
      geom_histogram(
        aes(y = ..density..), 
        position = "identity", 
        bins = 30, 
        alpha = 0.6, 
        color = "black"
      ) +
      geom_vline(xintercept = cutoff, color = "red", linetype = "dashed", size = 1) +
      annotate("text", x = cutoff, y = 0.25,
               label = paste0("Cutoff = ", round(cutoff, 2)),
               color = "red", angle = 90, vjust = -0.5) +
      labs(
        title = "Fisher’s Transformation – Histogram of Projected Points",
        x = "Projection y = aᵀx",
        y = "Density"
      ) +
      scale_fill_manual(values = c("Group 1" = "darkorchid3", "Group 2" = "aquamarine2")) +
      theme_minimal()
  })
  
  # Interepretations
  
  output$interpretation1 <- renderText({
    "The plot shows two bivariate normal populations with different means each with a value of 3 as the diagonal element in the covariance matrix.
    If the seperability isn't good the two centers are likely too close to each other. Remember
    Fisher's approach doesn't assume normality of data, but assumes population level covariance matrices are the same. If the covariance term is different then the
    optimal boundary should be quadratic. See Fisher's approach details for an interpretation of the boundary."
  })
  
  output$interpretation2 <- renderText({
    "The red line is Fisher's linear discriminant boundary, which is a straight-line perpendicular to the direction 'a' that seperates the two class means. If
    both groups had the same covariance and equal sample size, this line would be the perpendicular bisector of the line segment connecting two class means. 
    If sample sizes differ, the line shifts toward the larger group which would be reflective of having
    a higher prior probability for that group. "
  })
  
  output$interpretation3 <- renderText({
    "This plot shows the points in the projected space which is only 1D. The center is the average between the two transformed centers."
  })
}

shinyApp(ui = ui, server = server)
