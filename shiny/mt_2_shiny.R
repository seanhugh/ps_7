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
  titlePanel("Analyzing Prediction Accuracy by Poll Responses"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("data", "Data to Analyze:",
                  c("Kavanaugh Approval" = "kav_percentage",
                    "Feminism Support" = "feminism_percentage",
                    "Clinton Margin" = "clint_margin",
                    "Cell Usage" = "cell_percentage"))
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
    
    
    datareact() %>% 
      filter(UQ(as.name(input$data)) > 0) %>% 
      ggplot(aes_string(x = input$data, y = "poll_accuracy", color = "Party")) + 
      geom_smooth(method = lm) +
      geom_point(size = 6, alpha = .3) +
      labs(caption = "Prediction Data from Upshot/Siena polling data")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

