library(reshape2)  
library(ggplot2)
library(plotly)
# convert to long format

retTable <- getRetirementTable(33, 3000000, TRUE, 10, 4, 1000000, 8, 60, 85, 400000, 7, FALSE)
retTableLong <- melt(retTable, id="Age")  
p <- ggplot(data=retTableLong, aes(x=Age, y=value, colour=variable)) + geom_line()
p
#gg <- ggplotly(p)

retTable <- getRetirementTable(
    age=25, 
    corpus=1000000, 
    randomize=TRUE, 
    growth=10, 
    sdg=4, 
    expense=1000000, 
    inflation=8, 
    retireAge=60, 
    lifeYrs=100, 
    annualInv=400000, 
    invGrowth=7, 
    saveAfterRetire=FALSE)
retTableLong <- melt(retTable, id="Age")  
p <- ggplot(data=retTableLong, aes(x=Age, y=value, colour=variable)) + geom_line()
p


    
retTable <- getRetirementTable(
    age=33, 
    corpus=10000000, 
    randomize=FALSE, 
    growth=10, 
    sdg=4, 
    expense=1200000, 
    inflation=8, 
    retireAge=60, 
    lifeYrs=85, 
    annualInv=500000, 
    invGrowth=6, 
    saveAfterRetire=FALSE)

retTableLong <- melt(retTable, id="Age")  
retTableLong$value <- retTableLong$value / 1e+06
p <- ggplot(data=retTableLong, aes(x=Age, y=value, colour=variable)) + geom_line()
p$labels$y <- "Value (in Million)"
x <- ggplotly(p)
    
p <- ggplot(data=retTableLong, aes(x=Age, y=value, fill=variable)) + 
    geom_bar(stat="identity", position="dodge") +
    scale_fill_brewer(palette="Set1")
p
x <- ggplotly(p)