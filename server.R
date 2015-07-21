library(shiny)
library(MCMCpack)
source('valuation.R')

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

  
  output$simParams <- 
    renderText({
     
    
    
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      
      inFile <- input$file1
      
      if (is.null(inFile))
        return("")
      
      val.data <- processFiles(inFile,input)
      assign('mainData',val.data,envir=.GlobalEnv)      
      headerSelVec <- c(colnames(val.data))
      paste(
        h4("Simulation Parameters"),
        selectInput('depVar',
                    'Dependent Variable',
                    headerSelVec
                  ),
        selectInput('indVar',
                    'Independent Variable',
                    headerSelVec,
                    multiple=TRUE
        ),
        actionButton("getSimParms","Submit")
      )
  })
  
  selDepVar <- eventReactive(input$getSimParms, {
    input$depVar
  })
  
  selIndVar <- eventReactive(input$getSimParms, {
    input$indVar
  })
  
  output$fileContent <- renderTable({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
  
    mainData
  })

  output$summaryContent <- renderText({
    inFile <- input$file1
    
    if(is.null(inFile))
      return("")
    paste(h3('Simuation Summary'),verbatimTextOutput('summary'))
  })
  
  output$summary <- renderPrint({
      indVars <- selIndVar()
      depVar <- selDepVar()
      regressForm <- {paste(depVar," ~ ",paste(indVars,collapse='+'))}
      val.mcmc <- MCMCregress(regressForm, data=data.frame(mainData), seed=112005)
      summary(val.mcmc)
  })
  
})
