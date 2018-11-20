# Load the shiny library
# Load ggplot@

library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)


project_data <- read_rds("./shiny_data.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Election Predictions Expected Larger Republican Wins and Losses"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Election Size (# of Votes):",
                  min = 220000,
                  max = 500000,
                  value = 250000)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  datareact <- reactive({
    project_data %>% 
      filter(total_votes < input$bins)      
  })
  
  output$distPlot <- renderPlot({
    
    
    datareact() %>% 
      ggplot(aes(x = rep_advantage.predict, y = rep_advantage.actual)) + 
      geom_smooth(method = lm) +
      geom_point(size = 6, alpha = .3) +
      geom_abline(intercept = 0, slope = 1) +
      labs(x = "Predicted Republican Advantage",
           y = "Actual Republican Advantage",
           caption = "Prediction Data from Upshot/Siena polling data")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

