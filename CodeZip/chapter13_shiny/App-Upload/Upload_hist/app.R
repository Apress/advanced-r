

library(shiny)

# Define UI for application that draws a histogram based on file uplaod
ui <- shinyUI(fluidPage(
   
      # Copy the line below to make a file upload manager
      fileInput("file", label = h3("Histogram Data File input"),
                multiple = FALSE),
      tableOutput("tabledf"),
      tableOutput("tabledf2"),
      plotOutput("histPlot1")

))

# Define server logic required to draw a
server <- shinyServer(function(input, output) {
   
  output$tabledf<-renderTable({
    input$file
  })
  
histData<-reactive({
  file1<-input$file
  read.csv(file1$datapath,header=TRUE ,sep = ",")
  
})

output$tabledf2<-renderTable({
  histData()
  
})

output$histPlot1<-renderPlot({
  hist(as.numeric(histData()$X1))
  
})
  
})

# Run the application 
shinyApp(ui = ui, server = server)