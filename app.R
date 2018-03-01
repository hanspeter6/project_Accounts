# libraries
library(tm)
library(pdftools)
library(stringr)
library(dplyr)

## Only run examples in interactive R sessions
if (interactive()) {
        
        ui <- fluidPage(
                sidebarLayout(
                        sidebarPanel(
                                fileInput("file1", "Choose PDF File",
                                          accept = c(
                                                  "text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv",
                                                  ".pdf")
                                )
                                # checkboxInput("header", "Header", TRUE)
                        ),
                        mainPanel(
                                tableOutput("contents")
                        )
                )
        )
        
        server <- function(input, output) {
                output$contents <- renderText({
                        
                        
                        # date pattern for use elsewhere
                        date_pattern <- "\\d{1,2}/\\d{2}/\\d{4}"
                        
                        # input$file1 will be NULL initially. After the user selects
                        # and uploads a file, it will be a data frame with 'name',
                        # 'size', 'type', and 'datapath' columns. The 'datapath'
                        # column will contain the local filenames where the data can
                        # be found.
                        inFile <- input$file1
                        
                        if (is.null(inFile))
                                return(NULL)
                        
                        temp1 <- pdf_text(input$file1$datapath) # list of single string per pdf page
                        temp2 <- unlist(strsplit(temp1,"\n"))  # by line
                        
                        return(temp2[3])
                        # read.csv(inFile$datapath)
                })
        }
        
        shinyApp(ui, server)
}