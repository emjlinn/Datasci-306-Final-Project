library(shiny)
library(dplyr)

### ---- PREPARE DATA & MODEL ----

# Model: Predict prevalence (%) by Year + Age reference group

# Assuming smoking_all is already loaded in the environment

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
        "Enter Year to Predict (e.g., 2030):",
        value = max(year_range) + 5,
        min = year_range[1],
        max = year_range[2] + 50
      ),
      
      actionButton("predict_btn", "Predict")
    ),
    
    mainPanel(
      h3("Predicted Smoking Prevalence (%)"),
      verbatimTextOutput("prediction_text")
    )
  )
)

### ---- SERVER ----
server <- function(input, output) {
  
  prediction <- eventReactive(input$predict_btn, {
    new_data <- data.frame(
      Year = input$future_year,
      reference_group = input$age_group
    )
    predict(lm_model, newdata = new_data)
  })
  
  output$prediction_text <- renderText({
    req(prediction())
    paste0(round(prediction(), 2), "%")
  })
}

### ---- Run App ----
shinyApp(ui = ui, server = server)

#We chose to use Age and Year for our predictors. Looking at the visualizations we made,
#we noticed a strong trend between each age group, year, and average cigarette use prevalence.
#Specifically, we noticed that each group appeared to decrease at a different rate, with younger 
#groups decreasing at a steeper rate. We wanted to create a model to predict cigarette use 
#prevalence for a given age group, which will be chosen by the user. Our model allows the user to
#choose an age range and year, and it will predict the given cigarette use prevalence for that
#group in that year.
