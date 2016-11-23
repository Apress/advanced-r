#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# sudo cp -R ~/slider_time /srv/shiny-server/       

library(shiny)

slider_data<-read.csv("slider_data.csv", header = TRUE, sep = ",")
Phase1    <- slider_data[,2]
Phase2    <- slider_data[,3]
Phase3    <- slider_data[,4]
Phase4    <- slider_data[,5]

# Define UI for application that draws Bar Plot
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Training Programme Results"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("ayear",
                     "Academic Year:",
                     min = 2014,
                     max = 2017,
                     value = 2014,
                     step = 1,
                     animate = animationOptions(interval=600, loop=T))
      ),
      
      # Show the bar plot
      mainPanel(
         plotOutput("barPlot")
      )
   )
))

# Define server logic required to draw a barplot
server <- shinyServer(function(input, output) {
   
   output$barPlot <- renderPlot({
      # Count values in each phase which match the correct date.
     cap<-input$ayear*100
     x <- c(sum(Phase1<cap),
            sum(Phase2<cap),
            sum(Phase3<cap),
            sum(Phase4<cap))
      
      
      # draw the barplot for the correct year.
      barplot(x,
              names.arg = c("Phase I", "Phase II", "Phase III", "Fellows"),
              col = c("deeppink1","deeppink2","deeppink3","deeppink4"),
              ylim=c(0,50))
   })
})

# Run the application 
shinyApp(ui = ui, server = server)

