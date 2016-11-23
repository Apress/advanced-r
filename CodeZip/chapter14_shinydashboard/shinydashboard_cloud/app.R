#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages("XML")
#install.packages("data.table")
#install.packages("zoo")

########################################################################
##                                                                    ##
##            Shiny Dashboard Sampler                                 ##
##                                                                    ##
########################################################################



library(shiny)
library(shinydashboard)
library(XML)
library(zoo)
library(data.table)
library(ggplot2)


#run-once code

slider_data<-read.csv("slider_data.csv", header = TRUE, sep = ",")
Phase1    <- slider_data[,2]
Phase2    <- slider_data[,3]
Phase3    <- slider_data[,4]
Phase4    <- slider_data[,5]

bls <-readHTMLTable("http://data.bls.gov/timeseries/LAUMT484702000000006?data_tool=XGtable", stringsAsFactors = FALSE)
bls <- as.data.table(bls[[2]])
bls[, names(bls):= lapply(.SD, function(x) {
  x <- gsub("\\(.*\\)", "", x)
  type.convert(x)
  }
  )]
#str(bls)
setnames(bls,names(bls), gsub("\\s","",names(bls)))

bls[, Date := as.Date(paste0(Year, "-", Period, "-01"), format="%Y-%b-%d")]

startYear<-min(bls$Date)
endYear<-max(bls$Date)

# Define UI for dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Advanced R"), 
  dashboardSidebar(
    img(src = "egl_logo_full_3448x678.png", height = 43, width = 216),
    sidebarMenu(
      menuItem("Pie Charts!", tabName = "PieChart_Time", icon = icon("pie-chart")),
      menuItem("Use a Slider", tabName = "slider_time", icon = icon("sliders"), badgeLabel = "New"),
      menuItem("Upload a Histogram File", tabName = "Upload_hist", icon = icon("bar-chart")),
      menuItem("Labour Data Dates", tabName = "labour", icon = icon("cloud-download"), badgeLabel = "Live")
    )
    
    
  ),
  dashboardBody(
    tabItems(
      # PieChart_Time Content
      tabItem(tabName = "PieChart_Time",
              fluidRow(
                box( title = "PieChart_Time", status = "warning",
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
                
                box( title = "Graphical Output", solidHeader = TRUE, status = "warning",
                  plotOutput("piePlot")
                )
              )
      ),
      
      # Slider Tab Content
      tabItem(tabName = "slider_time",
              h2("Training Programme Results"),
              box( title = "Control the Academic Year", status = "primary", solidHeader = TRUE,
                        sliderInput("ayear",
                                       "Academic Year:",
                                       min = 2014,
                                       max = 2017,
                                       value = 2014,
                                       step = 1,
                                       animate = animationOptions(interval=600, loop=T))),
              box(plotOutput("barPlot"))
              
      ),
      
      #Histogram from an Uploaded CSV
      tabItem( tabName = "Upload_hist",
               
               fluidRow(
                        box(   title = "File Input",
                          # Copy the line below to make a file upload manager
                          fileInput("file", label = h3("Histogram Data File input"),
                                    multiple = FALSE)
                        ),
                        box( title = "Data from file input", collapsible = TRUE,
                          tableOutput("tabledf"))
                 
               ),
               
               fluidRow(
                 box(tableOutput("tabledf2")),
                 box( background = "blue",
                   plotOutput("histPlot1"))
               )
               
            ),
      
######################## Labour plot Tab#################################################
      tabItem(tabName = "labour",
              
              fluidRow(
                box( dateRangeInput("labourDates", min = startYear,max = endYear,
                                    start = startYear, end = endYear,
                                    label = h3("Date range")),
                     selectInput("labourYvalue", label = h3("Select box"), 
                                 choices = list("LaborForce" = 3,
                                                "Employement" = 4,
                                                "Unemployement" = 5), 
                                 selected = 3),
                     radioButtons("labourRadio", "Rolling Average", selected = FALSE,
                                  choices = list("Yes"=TRUE, "No"=FALSE))
                     ),
                box(title = "Test",
                    plotOutput("labourPlot"))
                #put one of those fancy boxes here
                
              ),
              fluidRow(
                valueBox(endYear-startYear, "Days of Data", icon = icon("calendar"))
              )
        
      )
    )
    
  ),
  title = "Dashboard Sampler",
  skin = "yellow"
  
  )

######################################### Define server logic required to draw a histogram
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
  
  ####Here is where the input of a file happens
  
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
  
  ##############labour plot####################
  
  output$labourPlot<-renderPlot({
    xlow<-input$labourDates[1]
    xhigh<-input$labourDates[2]
    blsSub <- bls[Date<=xhigh&Date>=xlow]
    yValue <- names(blsSub)[as.numeric(input$labourYvalue)]
    
    
    if(input$labourRadio == TRUE && nrow(blsSub)>=4){
      blsSub[,(yValue):=rollmean(get(yValue), k=3, na.pad = TRUE)]
    }
    
#   plot(1, xlab = class(input$labourYvalue)) 
    ggplot(data = blsSub, mapping = aes_string("Date", yValue)) +
      geom_line()+
     xlab("Years & Months")+
     ylab("People")+
     xlim(xlow, xhigh)+
      ggtitle(yValue)+
      scale_x_date(date_labels = "%b %y")
  })
})

# Run the application 
shinyApp(ui = ui, server = server)