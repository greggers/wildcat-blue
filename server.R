library(shiny)

generateValuation <- function(session)
{
  return(paste(titlePanel("Uploading Files"),
         sidebarLayout(
           sidebarPanel(
             fileInput('file1', 'Choose CSV File',
                       accept=c('text/csv', 
                                'text/comma-separated-values,text/plain', 
                                '.csv')),
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
                          '"')
           ),
           mainPanel(
             tableOutput('fileContent')
           )
         )))
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  # check which page to render
  
  output$content <- renderText({
    query <- parseQueryString(session$clientData$url_search)
    # Generate an HTML table view of the data
    
    if(is.null(query$page))
    {
      paste(sep = "",
            "protocol: ", session$clientData$url_protocol, "\n",
            "hostname: ", session$clientData$url_hostname, "\n",
            "pathname: ", session$clientData$url_pathname, "\n",
            "port: ",     session$clientData$url_port,     "\n",
            "search: ",   session$clientData$url_search,   "\n"
      )
    }
    else if(tolower(query$page) == tolower("valuation"))
    {
      generateValuation(session)
    }
    
  })
  
  output$fileContent <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)
  })
  
})
