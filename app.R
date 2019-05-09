#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dygraphs)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(tidyquant)
library(quantmod)
library(magrittr)
library(DT)
library(twitteR)
library(QuantTools)
library(timeDate)
library(shinythemes)

tweets <- read_rds("trump_tweets")

# Define UI for application that draws a histogram
ui <- 
  navbarPage("Trump's Tweets & The Stock Market", theme = shinytheme("simplex"),
    tabPanel("About",
             titlePanel(h1("Twitter Induces Intraday Stock Price Fluctuations", h2(a("@realDonaldTrump",href="https://twitter.com/realDonaldTrump")))),
             

             hr(),
             br(),
             br(),
             h4("This interface is designed to allow users to explore the relationship between Donald Trump's tweets
                     and stock market fluctuations."),
   
               # The br() function adds white space to the app.
               
               br(),
               br(),
               h4(paste("Using Twitter's API and the Quantmod library, I was able to create an interactive dashboard for users to
               explore specific keywords in Trump's tweets, and then view its effects on major publicly traded companes"))
            ),

    tabPanel("Explore",

       
      fluidPage(
        
        titlePanel("Effects of President Trump's Tweets on Stock Price Changes "),
      
        sidebarLayout(
          sidebarPanel(
            helpText("Input a Symbol, Date, and Interval to see the stock's pricing"),
            h3("Stock Pricing"),
            textInput("symb", label = ("Stock Symbol"), value = "MSFT"), 
            dateInput('date',
                           label = 'Date Input',
                           min = "2016-01-01", 
                           max = Sys.Date(),
                           value = Sys.Date() - 1, 
                           weekstart = 1 
            ),
            selectInput("period", "Interval:",
                        c("1 Minute" = "1min",
                          "5 Minutes" = "5min",
                          "30 Minutes" = "30min",
                          "1 Hour" = "hour")),
            br(),
            hr(),
            br(),
            
            h3("Tweet Search"),
            textInput("keyword", "Please enter a keyword to search, e.g. Amazon", "Amazon")
            
          ),
          
          
          mainPanel(
            tags$head(
              tags$style(HTML("
                              .shiny-output-error-validation {
                              font-size: 22px;
                              }
                              "))
              ),
              tabsetPanel(
                    tabPanel("Pricing", dygraphOutput("plot")),
                    tabPanel("Trade Volume", dygraphOutput("volume"))
                    ),
              DTOutput("word_table"),
              fluidRow(
                column(6, verbatimTextOutput('x5'))
              )
          )
    
        )
      )
    )
  )




# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
  # Check table clicked
  output$x5 = renderPrint({
    cat('\n\nSelected rows:\n\n')
    paste(input$word_table_cell_clicked)
  })
  
  observeEvent(input$word_table_cell_clicked, {
    info = input$word_table_cell_clicked
    # do nothing if not clicked yet, or the clicked cell is not in the 1st column
    if (is.null(info$value) || info$col != 1) return()
      updateDateInput(session,'date', value = info$value)

  })
  
  priceInput <- reactive({
    

    
    shiny::validate(
      need(as.logical(isWeekday(input$date)) == TRUE , "Please select a valid weekday")
    )
    
    shiny::validate(
      need(get_finam_data(input$symb, from=input$date, to = input$date, period = input$period), "Not A Valid Stock Symbol")
    )
    
    prices <- get_finam_data(input$symb, from = input$date, to = input$date, period = input$period) %>% 
      mutate(since_midnight = hour(time) * 60 + minute(time)) %>% 
      filter(since_midnight >= 9*60 & since_midnight <= (16 * 60)) %>% 
      select(time,open, high,low,close)
    

  })
  
  volumeInput <- reactive({
    
    shiny::validate(
      need(as.logical(isWeekday(input$date)) == TRUE , "Please select a valid weekday")
    )
    
    volume <- get_finam_data(input$symb, from = input$date, to = input$date, period = input$period) %>% 
      mutate(since_midnight = hour(time) * 60 + minute(time)) %>% 
      filter(since_midnight >= 9*60 & since_midnight <= (16 * 60)) %>% 
      select(time,volume)
  })
  
  output$plot <- renderDygraph({
    
    prices <- priceInput()
    prices_xts <- xts(prices, order.by = prices$time)
    prices_xts$time <- NULL
    dygraph(prices_xts, main = paste(input$symb, input$date)) %>% 
    dyCandlestick() %>% 
    dyRangeSelector() %>% 
    dyOptions(useDataTimezone = TRUE)
  })
  
  output$volume <- renderDygraph({
    
    volume <- volumeInput()
    volume_xts <- xts(volume, order.by = volume$time)
    volume_xts$time <- NULL
    dygraph(volume_xts, main = paste(input$symb, input$date)) %>% 
    dyOptions(stepPlot = TRUE, fillGraph = TRUE, colors="blue", useDataTimezone = TRUE)

  })
  
  

  
  output$word_table <- renderDT({
    
    datatable(tweets %>% filter(str_detect(text, input$keyword)) ,
    class = 'display',
    rownames = FALSE,
    selection = 'single',
    colnames = c('Tweet Text', 'Date', 'Time', 'Retweets', 'Favorites'),
    options = list(dom = 'tip')
    ) %>% formatStyle(2, cursor = 'pointer')
  })
  
  
})

# Run the application 
shinyApp(ui = ui, server = server)


