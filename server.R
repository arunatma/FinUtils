library(shiny)
library(reshape2)
library(ggplot2)
library(plotly)
source("finCalc.R")

retirementTable <- data.table()

shinyServer(function(input, output, session) {
    
    output$annuityCalcTable <- renderDataTable(
        retTable <- getRetirementTable(
            age=33, 
            corpus=as.numeric(input$s1Corpus), 
            randomize=FALSE, 
            growth=as.numeric(input$s1Growth),
            sdg=4, 
            expense=getAnnuity(as.numeric(input$s1Corpus) * 1000,
                as.numeric(input$s1Growth),
                as.numeric(input$s1Inflation),
                as.numeric(input$s1LagYears),
                as.numeric(input$s1Years)
            ) / 1000000, 
            inflation=as.numeric(input$s1Inflation), 
            retireAge=33 + as.numeric(input$s1LagYears), 
            lifeYrs=85, 
            annualInv=0, 
            invGrowth=7, 
            saveAfterRetire=FALSE)
    )
    output$corpusCalcTable <- renderDataTable(
        retTable <- getRetirementTable(
            age=33, 
            corpus=getCorpus(as.numeric(input$s2Annuity) * 1000,
                as.numeric(input$s2Growth),
                as.numeric(input$s2Inflation),
                as.numeric(input$s2LagYears),
                as.numeric(input$s2Years)
            ) / 1000000, 
            randomize=FALSE, 
            growth=as.numeric(input$s2Growth),
            sdg=4, 
            expense=as.numeric(input$s2Annuity), 
            inflation=as.numeric(input$s2Inflation), 
            retireAge=33 + as.numeric(input$s2LagYears), 
            lifeYrs=85, 
            annualInv=0, 
            invGrowth=7, 
            saveAfterRetire=FALSE)
    )
    
    observe({
        retirementTable <<- data.table(getRetirementTable(
                age=as.numeric(input$s3Age), 
                corpus=as.numeric(input$s3Corpus), 
                randomize=FALSE, 
                growth=as.numeric(input$s3Growth), 
                sdg=4, 
                expense=(as.numeric(input$s3Expense) / 1000) * 12, 
                inflation=as.numeric(input$s3Inflation), 
                retireAge=as.numeric(input$s3RetireAge), 
                lifeYrs=as.numeric(input$s3LifeAge), 
                annualInv=(as.numeric(input$s3AnnualInv) / 1000) * 12,  
                invGrowth=as.numeric(input$s3InvGrowth), 
                saveAfterRetire=as.logical(input$s3saveAfterRetire)))
        retirementTable[, NetWorth := round(NetWorth, 2)]
        retirementTable[, Expenditure := round(Expenditure, 2)]
        retirementTable[, Investment := round(Investment, 2)]
    })
    
    output$retirementTable <- renderDataTable(
        retirementTable
    )
            
    observe({
        updateSliderInput(session, "s1LagYears", max=input$s1Years - 1)
        updateSliderInput(session, "s2LagYears", max=input$s2Years - 1)
    })
    
    output$annuityPlot <- renderPlotly({
        retTable <- getRetirementTable(
            age=33, 
            corpus=as.numeric(input$s1Corpus), 
            randomize=FALSE, 
            growth=as.numeric(input$s1Growth),
            sdg=4, 
            expense=getAnnuity(as.numeric(input$s1Corpus) * 1000000,
                as.numeric(input$s1Growth),
                as.numeric(input$s1Inflation),
                as.numeric(input$s1LagYears),
                as.numeric(input$s1Years)
            ) / 1000000, 
            inflation=as.numeric(input$s1Inflation), 
            retireAge=33 + as.numeric(input$s1LagYears), 
            lifeYrs=85, 
            annualInv=0, 
            invGrowth=7, 
            saveAfterRetire=FALSE)
        retTableLong <- melt(retTable, id="Age")  
        retTableLong$value <- retTableLong$value / 1e+06
        gg <- ggplot(data=retTableLong, aes(x=Age, y=value, colour=variable)) + geom_line()
        gg$labels$y <- "Value (in Million)"
        p <- ggplotly(gg)
    })
    
    output$corpusPlot <- renderPlotly({
        retTable <- getRetirementTable(
            age=33, 
            corpus=getCorpus(as.numeric(input$s2Annuity) * 1000000,
                as.numeric(input$s2Growth),
                as.numeric(input$s2Inflation),
                as.numeric(input$s2LagYears),
                as.numeric(input$s2Years)
            ) / 1000000, 
            randomize=FALSE, 
            growth=as.numeric(input$s2Growth),
            sdg=4, 
            expense=as.numeric(input$s2Annuity), 
            inflation=as.numeric(input$s2Inflation), 
            retireAge=33 + as.numeric(input$s2LagYears), 
            lifeYrs=85, 
            annualInv=0, 
            invGrowth=7, 
            saveAfterRetire=FALSE)
        retTableLong <- melt(retTable, id="Age")  
        retTableLong$value <- retTableLong$value #/ 1e+06
        gg <- ggplot(data=retTableLong, aes(x=Age, y=value, colour=variable)) + geom_line()
        gg$labels$y <- "Value (in Million)"
        p <- ggplotly(gg)
    })

    output$retirementPlot <- renderPlotly({
        retTableLong <- melt(retirementTable, id="Age")  
        retTableLong$value <- retTableLong$value #/ 1e+06
        gg <- ggplot(data=retTableLong, aes(x=Age, y=value, colour=variable)) + geom_line()
        gg$labels$y <- "Value (in Million)"
        p <- ggplotly(gg)
    })    
    
})
