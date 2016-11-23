
##Dynamic Reports and the Cloud

library(shiny)
library(ltm)
library(data.table)
library(rmarkdown)
library(tufte)
library(formatR)
library(knitr)
library(evaluate)

  function(input, output) {

    output$contents <- renderTable({

      inFile <- input$file1

      if (is.null(inFile))
        return(NULL)

      read.csv(inFile$datapath, header = input$header,
               sep = input$sep, quote = input$quote)
    })

    scores <- reactive({
      inFile <- input$file1

      if (is.null(inFile))
        return(NULL)

      read.csv(inFile$datapath, header = input$header,
               sep = input$sep, quote = input$quote)
    })

    output$downloadReport <- downloadHandler(
      filename = function() {
        paste('quiz-report', sep = '.', 'pdf')
      },

      content = function(file) {
        src <- normalizePath('report.Rmd')


        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'report.Rmd', overwrite = TRUE)

       
        knitr::opts_chunk$set(tidy = FALSE, cache.extra = packageVersion('tufte'))
        options(htmltools.dir.version = FALSE)

        out <- render('report.Rmd')
        file.rename(out, file)
      }    )
    
    }