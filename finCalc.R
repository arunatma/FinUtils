getAnnuity <- function(corpus, growth, inflation, lagYrs, years){
# To calculate the annuity given a corpus, corpus growth and inflation
#
# Inputs:
# Corpus    : The current Corpus Value
# Growth    : Expected CAGR for the corpus 
# Inflation : Expected Inflation, with which annuity is to be adjusted yearly
# LagYears  : Years till which the corpus is left untouched
# Years     : Years till which the corpus is expected to last 
#
# Output:
# Annuity   : Maximum yearly annuity that can be withdrawn from corpus 
    annuity <- 10
    initVal <- corpus * (1 + growth/100) ^ lagYrs
    while(1){
        initAnn <- annuity * (1 + inflation/100) ^ lagYrs
        ppl <- initVal
        ant <- initAnn
        yrsCount <- years-lagYrs
        while(yrsCount > 0){
            ppl <- ppl * (1 + growth/100)
            ant <- ant * (1 + inflation/100)
            ppl <- ppl - ant 
            yrsCount <- yrsCount - 1
        }
        if (ppl > 5){
            lo = annuity 
            annuity = annuity * 2
        }
        else {
            if (ppl < -5){
                hi = annuity
                annuity = annuity - (hi - lo)/2
            }
            else {
                break
            }
        }
    }
    #return((annuity * (1 + inflation/100) ^ lagYrs) * ((1+inflation/100) ^ (1:(years-lagYrs))))
    return(annuity)
}

getCorpus <- function(annuity, growth, inflation, lagYrs, years){
# To calculate the Corpus required for a raising annuity.
#
# Inputs:
# Annuity   : The current yearly expenses
# Growth    : Expected CAGR for the corpus 
# Inflation : Expected Inflation, with which annuity is to be adjusted yearly
# LagYears  : Years till which the corpus is left untouched
# Years     : Years till which the corpus is expected to last 
#
# Output:
# Corpus    : Minimum corpus that is needed now, to provide for the annuity.
    corpus <- 10
    initAnn <- annuity * (1 + inflation/100) ^ lagYrs
    while(1){
        initVal <- corpus * (1 + growth/100) ^ lagYrs
        ppl <- initVal
        ant <- initAnn
        yrsCount <- years-lagYrs
        while(yrsCount > 0){
            ppl <- ppl * (1 + growth/100)
            ant <- ant * (1 + inflation/100)
            ppl <- ppl - ant 
            yrsCount <- yrsCount - 1
        }
        if (ppl > 5){
            hi = corpus 
            corpus = corpus - (hi - lo)/2
        }
        else {
            if (ppl < -5){
                lo = corpus
                corpus = corpus * 2
            }
            else {
                break
            }
        }
    }
    return(corpus)
}

                
getRetirementTable <- function(age, corpus, randomize, growth, sdg, expense, 
    inflation, retireAge, lifeYrs, annualInv, invGrowth, saveAfterRetire){
# To calculate the annuity given a corpus, corpus growth and inflation
#
# Inputs:
# Corpus    : The current Corpus Value
# Growth    : Expected CAGR for the corpus 
# Inflation : Expected Inflation, with which annuity is to be adjusted yearly
# LagYears  : Years till which the corpus is left untouched
# Years     : Years till which the corpus is expected to last 
#
# Output:
# Annuity   : Maximum yearly annuity that can be withdrawn from corpus 
    totalYears <- lifeYrs - age
    accYrs <- retireAge - age
    
    netWorth <- corpus
    spend <- expense 
    invest <- annualInv
    result <- data.frame()
    for(i in c(1:totalYears)){
        gt <- ifelse(randomize, rnorm(1, growth, sdg), growth)
        netWorth <- (netWorth + invest) * (1 + gt/100)
        spend <- spend * (1 + inflation/100)        
        if(i <= accYrs){
            curRow <- c(age + i, netWorth, 0, invest)
        } else {
            netWorth <- netWorth - spend
            curRow <- c(age + i, netWorth, spend, invest)
        }
        
        result <- rbind(result, curRow)
        
        if (i > accYrs && !saveAfterRetire){
            invest <- 0
        }
        invest <- invest * (1 + invGrowth/100)
    }
    names(result) <- c("Age", "NetWorth", "Expenditure", "Investment")
    return(result)
}



    