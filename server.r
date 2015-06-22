library(shiny)
library(data.table)
library(scales)
library(googleVis)
#library("reshape2")

dset = 'data.csv'
inputFile <- fread(dset, header=T, sep=",", stringsAsFactors=F)
plotX <- data.frame(1:10904)
plotY <- as.data.frame(as.numeric(as.character(inputFile$AwardAmount)))
plotData <- data.frame(plotX, plotY)

#create data frame of unique universities
universityNames <- data.frame(unique(inputFile[,8, with=FALSE]))
investigatorNames <- data.frame(unique(inputFile[,7, with=FALSE]))

totalMoney <- sum(as.numeric(as.character(inputFile$AwardAmount)), na.rm = TRUE)
avgMoney <- mean(as.numeric(as.character(inputFile$AwardAmount)), na.rm = TRUE)
sdMoney <- sd(as.numeric(as.character(inputFile$AwardAmount)), na.rm = TRUE)
stats <- NULL

shinyServer( 
  function(input, output, session){  
    observe({
      
      institutionSearchResult <- input$institutionSearch
      piSearchResult <- input$PIsearch
      awardSearchResult <- input$awardSearch

##############search by institution#############################
      if(input$searchMode == 'Institution' ){
        d <- data.frame(universityNames, universityNames)
        colnames(d) <- c("value", "label")

        #initial case
        if(is.null(institutionSearchResult)){
          updateSelectizeInput(session, 'institutionSearch', choices = d, server = TRUE)
        }
        #every other time
        if(! is.null(institutionSearchResult)){
          updateSelectizeInput(session, 'institutionSearch', choices = d, selected = input$institutionSearch, server = TRUE)  
        }

        for(searchTerm in 1:length(institutionSearchResult)){
          singleUniversityStats <- subset(inputFile, Institutions == (institutionSearchResult[searchTerm]))
          stats <- rbind(stats, singleUniversityStats)
        } 
        #write output to screen
        totalAward <- sum(as.numeric(as.character(stats$AwardAmount)))
        output$percent <- renderText(c(institutionSearchResult, 'received', format(round((totalAward / totalMoney)*100, 4), scientific = FALSE), "% of all NSF Funding in 2014"))
        output$totalAward <- renderText({c(dollar(totalAward), 'was awarded to the recipient(s) in your search')})
      }

################search by investigators##########################
      if(input$searchMode == 'Principal Investigator(s)' ){
        d <- data.frame(investigatorNames, investigatorNames)
        colnames(d) <- c("value", "label")
        
        #initial case
        if(is.null(piSearchResult)){
          updateSelectizeInput(session, 'PIsearch', choices = d, server = TRUE)
        }
        #every other time
        if(! is.null(piSearchResult)){
          updateSelectizeInput(session, 'PIsearch', choices = d, selected = input$PIsearch, server = TRUE)  
        }
        for(searchTerm in 1:length(piSearchResult)){
          singlePIStats <- subset(inputFile, Investigators == (piSearchResult[searchTerm]))
          stats <- rbind(stats, singlePIStats)
        } 
        #write output to screen
        totalAward <- sum(as.numeric(as.character(stats$AwardAmount)))
        output$percent <- renderText(c(piSearchResult, 'received', format(round((totalAward / totalMoney)*100, 4), scientific = FALSE), "% of all NSF Funding in 2014"))
        output$totalAward <- renderText({c(dollar(totalAward), 'was awarded to the recipient(s) in your search')})
      }

##################search by award title###########################
      if(input$searchMode == 'Award Title'){
        d <- data.frame(inputFile[,2, with=FALSE], inputFile[,2, with=FALSE])
        colnames(d) <- c("value", "label")
        #updateSelectizeInput(session, "awardSearch", choices = d, server = TRUE)
        
        #initial case
        if(is.null(awardSearchResult)){
          updateSelectizeInput(session, 'awardSearch', choices = d, server = TRUE)
        }
        #every other time
        if(! is.null(awardSearchResult)){
          updateSelectizeInput(session, 'awardSearch', choices = d, selected = input$awardSearch, server = TRUE)  
        }
        for(searchTerm in 1:length(awardSearchResult)){
          singleAwardStats <- subset(inputFile, AwardTitle == (awardSearchResult[searchTerm]))
          stats <- rbind(stats, singleAwardStats)
        
        #write output to screen
        totalAward <- sum(as.numeric(as.character(stats$AwardAmount)))
        output$percent <- renderText(c(awardSearchResult, 'received', format(round((totalAward / totalMoney)*100, 4), scientific = FALSE), "% of all NSF Funding in 2014"))
        output$totalAward <- renderText({c(dollar(totalAward), 'was awarded to the recipient(s) in your search')})
      }
      }

######################total NSF stats###############################
      output$Funding <- renderText({dollar(totalMoney)})
      output$avgAward <- renderText({c('The average NSF award was', dollar(avgMoney), 'with a standard deviation of', dollar(sdMoney))})
      
      if(input$displayGraph == TRUE){
        output$view <- renderGvis({
          gvisScatterChart(plotData, options = list(legend = 'none', title = 'NSF Award Distribution for 2014', pointSize=0.5, vAxis="{title: 'Award Amounts ($)'}",
                                                    hAxis = list(list(viewWindowMode = "explicit",
                                                                      title = 'Award',
                                                                      viewWindow = list(max = 10904, min = 0))), width = 1200, height = 800))
        }) 
      }
    })    
      }
  )
