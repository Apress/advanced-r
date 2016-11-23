#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# user interface drawing a pie chart
ui <- shinyUI(fluidPage(
  
  # Sidebar with numeric, text, and radio box inputs controlled by user
  sidebarLayout(
    sidebarPanel(
      numericInput("pie",
                   "Percent of Pie Chart",
                   min = 0,
                   max = 100, 
                   value = 50),
      
      textInput("pietext", "Text Input", value = "Default Title",
                placeholder = "Enter Your Title Here"),
      
      checkboxInput("pieChoice",
                    "  I want a Pie Chart instead.", value = FALSE)
    ),
    
    mainPanel()
)
))

# Define server 
server <- shinyServer(function(input, output) {})

# Run the application 
shinyApp(ui = ui, server = server)
