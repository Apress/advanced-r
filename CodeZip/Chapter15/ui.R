#Dynamic Reports and the Cloud User Interface
shinyUI(

  fluidPage(
    title = 'Score Control Panel',
    sidebarLayout(
      sidebarPanel(
        helpText("Upload a wide CSV file where
                 each column is 0 or 1 based on whether the student
                 got that question incorrect or correct."),

        fileInput('file1', 'Choose file to upload',
                  accept = c(
                    'text/csv',
                    'text/comma-separated-values',
                    'text/tab-separated-values',
                    'text/plain',
                    '.csv',
                    '.tsv'
                  )
        ),
        tags$hr(),
        checkboxInput('header', 'Header', TRUE),
        radioButtons('sep', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ','),
        radioButtons('quote', 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     '"'),
        tags$hr(),
        helpText("This is where helpertext goes"),
        downloadButton('downloadReport')
      ),
      mainPanel(
        tableOutput('contents')
      ))))