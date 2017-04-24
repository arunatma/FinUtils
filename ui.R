library(shiny)
library(plotly)

shinyUI(navbarPage("Calculator Suite for Retirement",
    tabPanel("Retirement Calculator",
        sidebarLayout(
            sidebarPanel(
                sliderInput("s3Age", "Age(Years):", 20, 75, 30, 1),
                textInput("s3Corpus", "Current Corpus(in Mil):", 10),
                sliderInput("s3Growth", "Corpus CAGR(%):", -5, 25, 10, 0.1),
                textInput("s3Expense", "Current Monthly Expense(in Thousands):", 100),
                sliderInput("s3Inflation", "Inflation(%):", -5, 25, 8, 0.1),
                sliderInput("s3RetireAge", "Retirement Age(Years):", 35, 75, 60, 1),
                sliderInput("s3LifeAge", "Life Expectancy(Years):", 35, 100, 80, 1),
                textInput("s3AnnualInv", "Monthly Investment(in Thousands):", 50),
                sliderInput("s3InvGrowth", "Investment Yearly Increase (%):", 0, 20, 8, 0.1),
                radioButtons("s3saveAfterRetire", "Save After Retire?:", c("No"=FALSE, "Yes"=TRUE))
            ),
            mainPanel(
                fluidRow(
                    radioButtons("showNetWorth", "Plot NetWorth?", c("No"=FALSE, "Yes"=TRUE)),
                    plotlyOutput("retirementPlot"),
                    dataTableOutput("retirementTable")
                )
            )
        )
    ),
    tabPanel("Annuity Calculator",
        sidebarLayout(
            sidebarPanel(
                textInput("s1Corpus", "Current Corpus:", 5000000),
                sliderInput("s1Growth", "Corpus CAGR(%):", -5, 25, 10, 0.1),
                sliderInput("s1Inflation", "Inflation(%):", -5, 25, 8, 0.1),
                sliderInput("s1LagYears", "Corpus Untouched for (Years):", 0, 100, 0, 1),
                sliderInput("s1Years", "Corpus to last for (Years):", 1, 100, 10, 1)
            ),
            mainPanel(
                fluidRow(
                    dataTableOutput("annuityCalcTable")
                )
            )
        )
    ),
    tabPanel("Corpus Calculator",
        sidebarLayout(
            sidebarPanel(
                textInput("s2Annuity", "Required Annuity:", 1000000),
                sliderInput("s2Growth", "Corpus CAGR(%):", -5, 25, 10, 0.1),
                sliderInput("s2Inflation", "Inflation(%):", -5, 25, 8, 0.1),
                sliderInput("s2LagYears", "Corpus Untouched for (Years):", 0, 100, 0, 1),
                sliderInput("s2Years", "Corpus to last for (Years):", 1, 100, 10, 1)
            ),
            mainPanel(
                fluidRow(
                    tableOutput("corpusCalcTable")
                )
            )
        )
    )
))
