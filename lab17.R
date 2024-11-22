library(shiny)
library(MASS)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Multiple Linear Regression with Model Selection"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload a CSV file", accept = c(".csv")),
      uiOutput("var_selector"),
      numericInput("max_predictors", "Maximum Number of Predictors", value = 3, min = 1),
      actionButton("run_regression", "Run Regression"),
      hr()
    ),
    mainPanel(
      h3("Regression Summary"),
      verbatimTextOutput("regression_summary"),
      h3("Plots"),
      plotOutput("residual_plot"),
      plotOutput("regression_lines")
    )
  )
)

server <- function(input, output, session) {
  # Reactive dataset with NA removal
  data <- reactive({
    req(input$file)
    dataset <- read.csv(input$file$datapath)
    dataset <- na.omit(dataset)  # Remove rows with missing values
    dataset
  })
  
  # Filter for numerical variables
  numerical_vars <- reactive({
    req(data())
    vars <- names(data())
    num_vars <- vars[sapply(data(), is.numeric)]  # Keep only numeric columns
    num_vars
  })
  
  # Render variable selectors
  output$var_selector <- renderUI({
    req(numerical_vars())
    tagList(
      selectInput("dependent_var", "Select Dependent Variable", choices = numerical_vars()),
      selectInput("independent_vars", "Select Independent Variables", choices = numerical_vars(), multiple = TRUE)
    )
  })
  
  # Limit the number of predictors based on user input
  limited_predictors <- reactive({
    req(input$independent_vars, input$max_predictors)
    predictors <- input$independent_vars
    if (length(predictors) > input$max_predictors) {
      predictors <- predictors[1:input$max_predictors]  # Limit to max_predictors
    }
    predictors
  })
  
  regression_result <- eventReactive(input$run_regression, {
    req(input$dependent_var, limited_predictors())
    formula <- as.formula(
      paste(input$dependent_var, "~", paste(limited_predictors(), collapse = "+"))
    )
    lm(formula, data = data())
  })
  
  # Enhanced regression summary
  output$regression_summary <- renderPrint({
    req(regression_result())
    model <- regression_result()
    summary_model <- summary(model)
    cat("Coefficients:\n")
    print(coef(summary_model))  # Coefficients and p-values
    cat("\nR-squared:", summary_model$r.squared, "\n")
  })
  
  # Residual plot
  output$residual_plot <- renderPlot({
    req(regression_result())
    plot_data <- data.frame(
      Residuals = residuals(regression_result()),
      Fitted = fitted(regression_result())
    )
    ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      theme_minimal() +
      labs(title = "Residual Plot", x = "Fitted Values", y = "Residuals")
  })
  
  # Regression line plot
  output$regression_lines <- renderPlot({
    req(input$dependent_var, limited_predictors())
    plot_data <- data()
    ggplot(plot_data, aes_string(x = limited_predictors()[1], y = input$dependent_var)) +
      geom_point() +
      geom_smooth(method = "lm", formula = regression_result()$call$formula, color = "blue") +
      theme_minimal() +
      labs(title = "Regression Line", x = limited_predictors()[1], y = input$dependent_var)
  })
}

shinyApp(ui, server)
