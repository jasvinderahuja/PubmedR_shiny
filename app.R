#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#jasvinderahuja@gmail.com
list.of.packages <- c("shiny", "tidyverse", "ggplot2", "Rcpp", "RISmed", "viridis", "topicmodels", "tm", "tidytext", "wordcloud")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
suppressPackageStartupMessages(library(viridis))
suppressPackageStartupMessages(library(topicmodels))
library(shiny)
require(RISmed)
source('functionsPUBMED2.R')

# Define UI for application that draws a histogram
ui <- fluidPage(
   # Application title
   titlePanel(paste0("Search Pubmed [To maintain response time search for limited results!]", collapse="")),
   # Sidebar with a slider input for number of bins 
   column(3,
        fluidRow(textInput("searchfor1", "Search", value = "meiosis recombination", width = NULL, placeholder = "space = AND")),
        hr(),
        fluidRow(sliderInput("mxres", "Max Results:", 
                             min = 100, max = 50000,  
                             value = 100)
        ),
        hr(),
        fluidRow(sliderInput("date_range", "Choose Date Range:", 
                    min = 1900, max = as.numeric(format(Sys.time(), "%Y")), 
                    value = c(2000, format(Sys.time(), "%Y"))
        )),
        hr(),
        fluidRow(actionButton("button", "Search")),
        hr(),
        fluidRow(actionButton("button2", "Draw New Plots")),
        hr(),
        fluidRow(plotOutput("authors")),
        hr(),
        fluidRow(plotOutput("timeline")),
        hr(),
        fluidRow(plotOutput("words1")),
        hr(),
        fluidRow(plotOutput("bigrams1")),
        hr(),
        fluidRow(plotOutput("journal"))
        ),
  column(9,
        uiOutput('myTable')
      )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$myTable <- renderUI({
                input$button
                  isolate({ 
                      srch <<-input$searchfor1 
                  })
                pubmedResults <<- PaperID.yr(srch, input$date_range[1], input$date_range[2], input$mxres)
                output$pubout <- DT::renderDataTable(pubmedResults, filter = "top", options = list(lengthChange = FALSE))
                DT::dataTableOutput("pubout")
                })
    output$timeline <- renderPlot({
      input$button2
      plotPubmed(pubmedResults, srch)
    }
    )
    output$words1 <- renderPlot({
      input$button2
      fetchwordcloud(pubmedResults)
    }, execOnResize = TRUE)
    output$bigrams1 <- renderPlot({
      input$button2
      fetchbigrams(pubmedResults)
    }, execOnResize = TRUE)
    output$journal <- renderPlot({
      input$button2
      fetchjournal(pubmedResults)
    }, execOnResize = TRUE)
    output$authors <- renderPlot({
      input$button2
      fetchauthorcloud(pubmedResults)
    }, execOnResize = TRUE)
session$onSessionEnded(stopApp)  
}

# Run the application 
shinyApp(ui = ui, server = server)

