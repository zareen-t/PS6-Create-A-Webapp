library(shiny)
library(tidyverse)

data <- read_delim("WhatsgoodlyData-6.csv")

#data cleaning
shopping <- data %>% 
  group_by('Segment Type', 'Segment Description')


#UI Coding
ui <- fluidPage(
  titlePanel("Social Media Influence"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "Gender",
                  label = "Select Gender",
                  choices = c("Both", "Male", "Female"),
                  selected = "Both")
    ),
    mainPanel(
      plotOutput( "plot")
      #tabsetPanel(type = "tabs",
                  #tabPanel("Plot", plotOutput("plot")),
                  #tabPanel("Summary", verbatimTextOutput("summary"))
      #)
    )
  )
)

#Server Coding
server <- function(input, output) {
  #scatter plot
  filtered_data <- reactive({
    data %>%
      group_by(Gender) %>%
      summarize(count = n())
  })
  
  output$plot <- renderPlot({
    ggplot(data = filtered_data(), aes(x = Gender, y = count)) +
      geom_bar(stat = "identity") +
      labs(title = "Bar plot of gender", x = "Gender", y = "Count")
  })
  
  
  
  
  output$summary <- renderPrint({
    summary(data)
  })
}



shinyApp(ui = ui, server = server)
