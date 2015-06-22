library(shiny)
library(XML)
library(googleVis)
library(ggplot2)
library(rCharts)


shinyUI(
  pageWithSidebar(
    #Application title
  headerPanel("NSF 2014 Award Search Tool"),
  sidebarPanel(
    selectInput('searchMode', 'Select a search type', choices = c('Institution', 'Principal Investigator(s)', 'Award Title')),
    conditionalPanel(
      condition = "input.searchMode == 'Institution'",
      selectizeInput(
        'institutionSearch', 'Start typing your search query here', choices = NULL, multiple = TRUE
      )
      ),
    conditionalPanel(
      condition = "input.searchMode == 'Principal Investigator(s)'",
      selectizeInput(
        'PIsearch', 'Start typing your search query here', choices = NULL, multiple = TRUE
      )
    ),
    conditionalPanel(
      condition = "input.searchMode == 'Award Title'",
      selectizeInput(
        'awardSearch', 'Start typing your search query here', choices = NULL, multiple = TRUE
      )
    ),
    checkboxInput('displayGraph', 'Display Scatter Plot of All Awards', value=FALSE)
    ),
  
  mainPanel(
    h3('Your Search Results'),
    verbatimTextOutput("totalAward"),
    #textOutput("names"),
    verbatimTextOutput("percent"),
    
    h3('All NSF Data'),
    h4('Total NSF Funding for 2014'),
    verbatimTextOutput("Funding"),
    h4('Award Stats'),
    verbatimTextOutput("avgAward"),
    
    conditionalPanel(
      condition = "input.displayGraph",
      htmlOutput("view")
      )
    )
  )
)