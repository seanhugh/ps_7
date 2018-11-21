# Load the shiny library
# Load ggplot@

library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)

myArray <-  c("vote_change" = "Raw Votes vs. 2016",
              "kav_percentage" = "Kavanaugh Approval",
              "feminism_percentage" = "Feminism Support",
              "clint_margin" = "Clinton Margin",
              "other_percentage" = "Other vote Percentage",
              "dem_percentage" = "Democrat vote Percentage",
              "cell_percentage" = "Cell Usage")

project_data <- read_rds("./shiny_data.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Analyzing Prediction Accuracy by Poll Responses"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("data", "Data to Analyze:",
      
                  c("Raw Votes vs. 2016" = "vote_change",
                    "Kavanaugh Approval" = "kav_percentage",
                    "Feminism Support" = "feminism_percentage",
                    "Clinton Margin" = "clint_margin",
                    "Other vote Percentage" = "other_percentage",
                    "Democrat vote Percentage" = "dem_percentage",
                    "Cell Usage" = "cell_percentage")),
      
      "Above is a selection of interesting comparisons that we noticed when analyzing the dataset.
      One main take away is the Raw Votes vs. 2016 tab. It is interesting that this variable almost
      directly correlates with prediction accuracy. There is a chance that we could do a much better job
      predicting the changes in raw votes, and that this could increase our accuracy in the future."
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
    project_data
  })
  
  output$distPlot <- renderPlot({
    
#     
    datareact() %>% 
      filter(UQ(as.name(input$data)) != 0) %>% 
      ggplot(aes_string(x = input$data, y = "poll_accuracy", color = "Party")) + 
      geom_smooth(method = lm) +
      geom_point(size = 6, alpha = .3) +
      labs(caption = "Prediction Data from Upshot/Siena polling data", 
           x = myArray[input$data],
           y = "Accuracy of Prediction")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

