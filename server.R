library(shiny)
library(reshape2)
library(ggplot2)
library(plotly)
library(data.table)
source("finCalc.R")

shinyServer(function(input, output, session) {
    
    output$annuityCalcTable <- renderDataTable(
        annuityTable <- data.table(Year = c((1 + input$s1LagYears):(input$s1Years)), 
            Annuity = round(getAnnuity(as.numeric(input$s1Corpus),
                        as.numeric(input$s1Growth),
                        as.numeric(input$s1Inflation),
                        as.numeric(input$s1LagYears),
                        as.numeric(input$s1Years))
                      , 0)
            )
    )
    output$corpusCalcTable <- renderTable(
        data.table(Corpus = getCorpus(as.numeric(input$s2Annuity),
            as.numeric(input$s2Growth),
            as.numeric(input$s2Inflation),
            as.numeric(input$s2LagYears),
            as.numeric(input$s2Years)
        ))
    )
    
    output$retirementTable <- renderDataTable({
        retirementTable <- data.table(getRetirementTable(
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
        retirementTable[, NetWorth := round(NetWorth, 0)]
        retirementTable[, Expenditure := round(Expenditure, 0)]
        retirementTable[, Investment := round(Investment, 0)]    
        retPositveNW <- retirementTable[retirementTable[, NetWorth > 0]]
        retPositveNW
    })
            
    observe({
        updateSliderInput(session, "s1LagYears", max=input$s1Years - 1)
        updateSliderInput(session, "s2LagYears", max=input$s2Years - 1)
    })
    
    output$retirementPlot <- renderPlotly({
        retirementTable <- data.table(getRetirementTable(
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
        retPositveNW <- retirementTable[retirementTable[, NetWorth > 0]]
        
        if(input$showNetWorth){
            graphTable <- retPositveNW
        } else {
            graphTable <- retPositveNW[, !c("NetWorth"), with=FALSE]
        }
        
        retTableLong <- melt(graphTable, id="Age")  
        retTableLong$value <- retTableLong$value #/ 1e+06
        gg <- ggplot(data=retTableLong, aes(x=Age, y=value, colour=variable)) + geom_line()
        if(input$showNetWorth){
            gg <- gg + scale_color_manual(values = c("black", "red", "blue"))
        } else {
            gg <- gg + scale_color_manual(values = c("red", "blue"))
        }
        
        gg$labels$y <- "Value (in Million)"
        p <- ggplotly(gg)
    })    
    
})
