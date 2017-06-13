
library(data.table)
library(quantmod)
library(ggplot2)
library(dplyr)
library(lubridate)
library(shiny)
library(DT)
library(dygraphs)
library(curl)
library(scales)
library(shinyjs)
stockTickers <- as.vector(read.csv('data/tickers.csv'))
names(stockTickers) <- 'Tickers'
# ui.R
shinyUI(navbarPage(
        title = "AOTC",
        tabPanel(
                title = "Technical Analysis",
                value = "TA",
                sidebarLayout(
                        fluid = TRUE,
                        sidebarPanel(
                                width = 3,
                                selectizeInput(
                                        "ticker",
                                        label = h5("Stock Ticker"),
                                        choices = stockTickers,
                                        selected = 'FB',
                                        size = 5
                                ),
                                checkboxInput('split',label = h5('Split/Dividend Adjusted'),value = FALSE),
                                conditionalPanel(
                                        "input.tab==1 || input.tab==2",
                                        sliderInput(
                                                "smaval",
                                                label = h5("Simple Moving Average Days"),
                                                min = 1,
                                                max = 100,
                                                value = 100
                                        )
                                ),
                                conditionalPanel(
                                        "input.tab==1 || input.tab==2",
                                        sliderInput(
                                                "emaval",
                                                label = h5("Exponential MA Days"),
                                                min = 1,
                                                max = 100,
                                                value = 100
                                        )
                                ),
                                conditionalPanel(
                                        "input.tab==1",
                                        checkboxGroupInput(
                                                "price",
                                                selected = c(1, 2, 3),
                                                inline = TRUE,
                                                h5("Indicators"),
                                                c(
                                                        "MACD" = 1,
                                                        "Elder" = 2,
                                                        "Chai" = 3
                                                )
                                        )
                                )
                                #submitButton(text = "Apply Changes", icon = NULL, width = NULL)
                                


                        ),
                        mainPanel(tabsetPanel(type = "tabs", id = "tab",
                                              tabPanel("Stocks",value = 1,
                                                       div(style = "padding-top: 5px; padding-bottom: 5px; border-top: 1px solid silver; border-bottom: 1px solid silver;",
                                                           dygraphOutput('candlestick', width ='100%')
                                                           )
                                                       )
                                              )#Close tabsetPanel
                                  )##Close Mainpanel
                        )##Close Sidebar Layout
                ),
        tabPanel(
                title = "Fundamental Analysis",
                value = "stockFA",
                bootstrapPage(mainPanel(
                        width = 12,
                        tabsetPanel(
                                type = "tabs",
                                id = "stockFA1",
                                tabPanel("Company Overview",value = 1,
                                         div(style="width: 100%; height: 15px"),
                                         fluidRow(h6(textOutput("thisTicker"))),
                                         #div(style="border-bottom: 1px solid silver; width: 100%; height: 5px"),
                                         fluidRow(
                                                 column(width = 4,offset= 0,
                                                        DT::dataTableOutput("stockNews",width='400',height = 400)
                                                        ),
                                                 column(width = 8,offset = 0,
                                                        div(style ="padding: 10px; border-left : 1px solid silver;",dygraphOutput('sharePrice2', width ='100%',height = 375))
                                                        )
                                         ),
                                         div(style="border-top: 1px solid silver; width: 100%; height: 30px"),
                                         
                                         div(h5(textOutput('Yearly_Financials'))),
                                         div(style="width: 100%",DT::dataTableOutput("YFinancials",width='100%')),
                                         div(style="width: 100%; height: 30px"),
                                         div(h5(textOutput('Quarterly_Financials'))),
                                         div(style="width: 100%",DT::dataTableOutput("QFinancials",width='100%')),
                                         div(style="width: 100%; height: 30px")
                                )
                        ))
                )##Close Markets bootstrapPage
        ),##Close Markets tabPanel
        #Close tabPanel
        tabPanel(
                title = "Copper",
                value = "cu",
                bootstrapPage(mainPanel(
                        width = 12,
                        tabsetPanel(
                                type = "tabs",
                                id = "fa",
                                tabPanel("LME Data",value = 1,
                                                div(style = "padding-bottom: 5px; border-bottom: 1px solid silver;", dygraphOutput('COTGraph',height = '275',width ='100%')),
                                                div(style = "padding-bottom: 5px; border-bottom: 1px solid silver;",dygraphOutput('LMEInvGraph',height='275', width ='100%'))
                                        ),
                                tabPanel("News Analysis",value = 5,
                                         div(style="width: 500px",DT::dataTableOutput("newsDF",width='100%'))
                                                )
                                        )#Close tabsetPanel
                                )#Close Copper mainPanel
                        )##Close Copper bootstrapPage
                ),##Close Copper tabPanel
        tabPanel(
                title = "Crude Oil",
                value = "oil",
                bootstrapPage(mainPanel(
                        width = 12,
                        tabsetPanel(
                                type = "tabs",
                                id = "fa",
                                tabPanel("Crude Supply", value = 1,
                                        div(style = "padding-bottom: 5px; border-bottom: 1px solid silver;",dygraphOutput('oilStocks',height=250,width ='100%')),
                                        div(style = "padding-bottom: 5px; border-bottom: 1px solid silver;",dygraphOutput('supplyGraph',height=250, width ='100%'))),
                                tabPanel("Crude Imports", value = 2,
                                         div(style = "padding-bottom: 5px; border-bottom: 1px solid silver;",dygraphOutput('importsGraph',height=250, width ='100%')),
                                         div(style = "padding-bottom: 5px; border-bottom: 1px solid silver;",dygraphOutput('importsAvgGraph',height=250, width ='100%')))
                        )#Close tabsetPanel
                )#Close Copper mainPanel
                )##Close Copper bootstrapPage
        )##Close Copper tabPanel
        )##Close Navbar Page
        )##Close Shiny UI
        