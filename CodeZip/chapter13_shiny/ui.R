#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Advanced R Shiny Dashboard"),
  
  
  fluidRow(
    
    column(6,
           wellPanel(
             h2("Academic Enrollment"),
             radioButtons("radio_Enr", label = h3("Select Race"),
                          choices = list( "African American" = 1, "Hispanic" = 2,
                                          "White" = 3, "All" = 4),selected = 1),
             br(),
             plotOutput("distPlot")

           )),
    
    column(6,
           wellPanel(
             h2("Foundation ABC Rates"),
             selectInput("select_abc", label = h3("Select Foundation Area"), 
                         choices = list("All Foundations" = 10, "Developmental Math" = 11,
                                        "Developmental Reading" = 12), 
                         selected = 1),
             br(),
             plotOutput("linePlot")
             
           )),
    
    column(6,
           wellPanel(
             h2("Point Biseriel Calculator"),
             # Copy the line below to make a file upload manager
             fileInput("file_pbi", label = h3("File input of PBI Data")),
             
             hr(),
             fluidRow(column(4, verbatimTextOutput("value"))),
             br(),
             tableOutput("tablePBI")
             
           ))
    
    
  )
  )
)
