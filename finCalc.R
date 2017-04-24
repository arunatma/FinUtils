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
# Annuity   : A list of maximum yearly annuity that can be withdrawn from corpus 
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
    return((annuity * (1 + inflation/100) ^ lagYrs) * ((1+inflation/100) ^ (1:(years-lagYrs))))
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
# Age       : Current age of the person in Years 
# Corpus    : The current Corpus Value
# randomize : Whether to randomize the growth number
# Growth    : Expected CAGR for the corpus (mean growth, if randomize is TRUE)
# sdg       : If yes to randomize, standard deviation (assuming normal dist.)
# Expense   : Current Yearly expenditure
# Inflation : Expected Inflation, with which annuity is to be adjusted yearly
# retireAge : Retirement Age in Years (not time till retirement)
# lifeYrs   : The age till which, the corpus has to serve the individual
# annualInv : Investment that is done annually
# invGrowth : The committed yearly percentage increment to investment
# saveAfterRetire : Whether the person is saving after retirement 
#
# Assumption: The investment is the amount remaining after accounting for 
#             expense. So, till the age the person retires, expenditure is 
#             shown as zero.
#             Year number are end of year.  Networth captures the previous year 
#             investment and corpus along with the growth in corpus
#
# Output:
# Ret. Table: Retirement Table with 3 columns - Networth, Expenditure and 
#             Investment
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
            curRow <- c(age + i, netWorth, spend, invest)
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



    