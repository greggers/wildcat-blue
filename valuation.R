library(tools)
library(gdata)
library(rJava)

generateValuation <- function(session)
{
    return(
      paste(
        titlePanel("Valuation Simuation"),
        
        fluidRow(
          column(3,
            h4("Input File"),
            fileInput('file1', 'Choose CSV or Excel File',
                       accept=c('text/csv', 
                                'text/comma-separated-values,text/plain', 
                                '.csv','application/vnd.ms-excel')),
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
          column(4,
            htmlOutput('simParams')
          )
          
        ),
        hr(),
        htmlOutput('summaryContent')
        
      )  
    )
}

processFiles <- function(inFile,input)
{
  ext <- file_ext(inFile$name)
  
  if(ext == "csv")
  {
    
    val.data <- read.csv(inFile$datapath, stringsAsFactors=FALSE, header=input$header, sep=input$sep, 
                              quote=input$quote)
  }
  else if((ext == "xls") ||(ext == "xlsx"))
  {
    val.data <- read.xls(inFile$datapath)
  }
  for(i in val.data)
  {
    if((sapply(i,class) == 'character') 
     && (substring(i,1,1)=='$'))
     {
       i <- as.numeric(gsub('[$,]', '', i))
     }
  }
  val.data <- sapply(val.data, clean)
#   headerSelVec <- c(colnames(val.data))
#   for(i in 1:length(val.data))
#   {
#     if((sapply(val.data[1,i],class) == 'factor') 
#     && (substring(val.data[1,i],1,1)=='$'))
#     {
#       vecName <- {paste('val.data$',headerSelVec[i],sep='')}
#       vecName <- as.numeric(gsub('[$,]', '', vecName))
#     }
#   }
  
  return(val.data)
}

clean <- function(i)
{
  if((sapply(i,class) == 'character') 
     && (substring(i,1,1)=='$'))
  {
    i <- as.numeric(gsub('[$,]', '', i))
  }
  return(i)
}