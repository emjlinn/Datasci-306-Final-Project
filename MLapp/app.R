#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


install.packages("fastmap")

install.packages(shiny)

# make sure to install: install.packages("shiny")

library(shiny)
library(dplyr)

### ---- PREPARE DATA & MODEL ----
# Assuming smoking_all is already loaded in the environment
# Model: Predict prevalence (%) by Year + Age reference group

model_data <- smoking_all %>%
  filter(disparity_category == "Age") %>%
  select(reference_group, Year, reference_prev_pct) %>%
  drop_na()

# Fit linear regression
lm_model <- lm(reference_prev_pct ~ Year + reference_group, data = model_data)

age_groups <- sort(unique(model_data$reference_group))
year_range <- range(model_data$Year)

### ---- UI ----
ui <- fluidPage(
  
  titlePanel("Group 28: Smoking Prevalence Prediction (Linear Regression)"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput(
        "age_group",
        "Select Age Group:",
        choices = age_groups
      ),
      
      numericInput(
        "future_year",
        "Enter Year to Predict (e.g. 2030):",
        value = max(year_range) + 5,
        min = year_range[1],
        max = year_range[2] + 50
      ),
      
      actionButton("predict_btn", "Predict Smoking Prevalence")
    ),
    
    mainPanel(
      h3("Predicted Prevalence"),
      verbatimTextOutput("prediction_text"),
      hr(),
      plotOutput("prediction_plot")
    )
  )
)

### ---- SERVER ----
server <- function(input, output) {
  
  # Reactive prediction once button is pressed
  prediction <- eventReactive(input$predict_btn, {
    
    new_data <- data.frame(
      Year = input$future_year,
      reference_group = input$age_group
    )
    
    predict(lm_model, newdata = new_data)
  })
  
  output$prediction_text <- renderText({
    req(prediction())
    paste0("Predicted smoking prevalence: ",
           round(prediction(), 2), "%")
  })
  
  # Plot the linear prediction line for the selected age group
  output$prediction_plot <- renderPlot({
    req(prediction())
    
    # Create a sequence of years to visualize the regression line
    plot_years <- seq(min(model_data$Year), input$future_year, by = 1)
    new_dat <- data.frame(
      Year = plot_years,
      reference_group = input$age_group
    )
    preds <- predict(lm_model, newdata = new_dat)
    
    plot(
      plot_years, preds, type = "l", lwd = 2,
      xlab = "Year", ylab = "Predicted Prevalence (%)",
      main = paste("Predicted Trend for Age Group:", input$age_group)
    )
    
    points(input$future_year, prediction(), col = "red", pch = 19, cex = 1.4)
    text(input$future_year, prediction(),
         labels = paste0(round(prediction(), 2), "%"),
         pos = 4, col = "red")
  })
}

### ---- Run App ----
shinyApp(ui = ui, server = server)
