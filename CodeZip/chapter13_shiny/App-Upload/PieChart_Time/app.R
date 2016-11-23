library(shiny)

# user interface drawing a pie chart
ui <- shinyUI(fluidPage(
   
   # Page title
   titlePanel("User Controlled Chart"),
   
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
      
      # Show the plot(s)
      mainPanel(
         plotOutput("piePlot")
      )
   )
))

# server side R code creating Pie Chart or barplot hidden from user
server <- shinyServer(function(input, output) {
   
   output$piePlot <- renderPlot({
      # generate Pie chart ratios based on input$pie from user
      y <- c(input$pie, 100-input$pie)

      # draw the pie chart or barplot with the specified ratio and label
      
    if(input$pieChoice == FALSE){
      barplot(y, ylim = c(0,100),
              names.arg = c(input$pietext, paste0("Complement of ", input$pietext))) 
      
    }else{
      pie(y, labels = c(input$pietext, paste0("Complement of ", input$pietext)))}
      
   })
})

# Run the application 
shinyApp(ui = ui, server = server)
