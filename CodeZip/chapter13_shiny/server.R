#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
CollegeData <- read.delim("Ch22_Data.txt", header = FALSE, sep = "\t")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    Enr_Race <- as.numeric(input$radio_Enr)+1
    y <- unlist(CollegeData[Enr_Race,2:6], use.names = FALSE)
    yearNames <- unlist(CollegeData[1,2:6], use.names=FALSE)

        # draw the barplot
    barplot(y, col = c("red", "green", "blue", "violet", "cornsilk"),
            names.arg=yearNames,
            ylim=c(0,5000))

  })
  
  output$linePlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    Fnd_ABC <- as.numeric(input$select_abc)+1
    ABC <- unlist(CollegeData[Fnd_ABC,2:6], use.names = FALSE)
    FyearNames <- unlist(CollegeData[1,2:6], use.names=FALSE)
    
    # draw the piechart
    plot(FyearNames, ABC, type = "l", col="dark red")
    
  })
  
  
# You can access the value of the widget with input$file, e.g.
 # output$tablePBI <- renderPlot({ hist(input$file_pbi[ , 1]) })
  
})
