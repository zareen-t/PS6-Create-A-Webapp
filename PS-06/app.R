library(shiny)
library(tidyverse)

orange <- read_delim("orange-trees.csv") %>% 
  mutate(tree_factor = factor(Tree))

ui <- fluidPage(
  titlePanel("Orange Data"), 
    mainPanel(
     tabsetPanel(type = "tabs",
                ## Introduction
                tabPanel("Introduction",
                         h1("Oranges Data Set"),
                         p("This data set has information on the growth of trees in California. The data set contains", strong("35"), "observations of", strong("4"), "variables."),
                         p("Variables Measured In the Data Set:"),
                         em("Tree Age"),
                         p(),
                         em("Tree Circumference"),
                         p(),
                         em("Tree Growth"),
                         p(),
                         p(),
                         p("Here is a random sample of data:"),
                         tableOutput("sample")),
                ## Plot
                tabPanel("Plot",
                         sidebarLayout(
                           sidebarPanel(
                             checkboxGroupInput("trees", 
                                                "Which tree would you like to plot: ", 
                                                choices = unique(orange$Tree), 
                                                selected = unique(orange$Tree)), 
                             sliderInput("range", 
                                         "What range of Age would you like to plot: ", 
                                         min = 0, 
                                         max = max(orange$age), 
                                         value = c(0, max(orange$age))),
                             selectInput("color", 
                                         "Choose a color to highlight:", 
                                         choices = c("None", "Red", "Blue", "Green", "Yellow")),
                             checkboxInput("labels", 
                                           "Show tree labels", 
                                           value = FALSE)
                           ),
                           mainPanel(
                             plotOutput("plot"),
                             p(),
                             textOutput(("plot_description"))
                            )
                          )
                        ),
                ## Table
                tabPanel("Table",
                         sidebarLayout(
                           sidebarPanel(
                             selectInput("age", 
                                         "Select an age:", 
                                         choices = c("All Ages", as.character(unique(orange$age))))
                           ),
                           mainPanel(
                             tableOutput("table"),
                             p(),
                             textOutput("table_description")
                            )
                          )
                        )       
              )
          )
      )



server <- function(input, output) {
  
  # Sample plot
  output$sample <- renderTable(
    orange %>% 
      sample_n(5)
  )
  
  
  ## Plot
  output$plot <- renderPlot({
    orange_filter <- orange %>% 
      filter(Tree %in% input$trees) %>% 
      filter(age >= input$range[1], 
             age <= input$range[2])
    
    color <- if(input$color != "None") {
      input$color
    } else {
      "grey"
    }
    ggplot(orange_filter, aes(x = age, y = circumference)) + 
      geom_point(aes(col=tree_factor), color=color) + 
      labs(title = "Age vs Circumference", x = "Age (years)", y = "Circumference (inches)") +
      scale_color_manual(values = rep("grey", length(levels(orange$tree_factor))-1)) +
      if(input$labels) geom_text(aes(label = Tree))
  })
  ## Plot Description
  output$plot_description <- renderText({
    age_range <- paste(input$range, collapse = " to ")
    paste("The plot shows trees from age", age_range, "years old above.")
  })
  
  
  
  ## Table
  output$table <- renderTable({
    if(input$age == "All Ages"){
      orange %>% 
        group_by(age) %>% 
        summarise(avg_cir = mean(circumference)) %>% 
        mutate("Age" = age, "Average Circumference" = avg_cir) %>% 
        select("Age", "Average Circumference")
    } else {
      orange %>% 
        filter(age == input$age) %>% 
        summarise(avg_cir = mean(circumference)) %>% 
        mutate("Average Circumference" = avg_cir) %>% 
        select("Average Circumference")
    
    }
  })
  ## Statement
  output$table_description <- renderText({
    if(input$age == "All Ages") {
      paste("Average circumference for all ages is", round(mean(orange$circumference), 2))
    } else {
      avg <- orange %>% 
        filter(age == input$age) %>% 
        summarise(avg_cir = mean(circumference)) %>% 
        pull(avg_cir)
      
      paste("The average circumference of trees at age", input$age, "is", round(avg, 2), "!")
    
    }
  })
  
}

shinyApp(ui = ui, server = server)
