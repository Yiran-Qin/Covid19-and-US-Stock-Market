
# Import necessary packages
library(shiny)
library(shinythemes)
library(shinydashboard)
library(plotly)
library(dplyr)
library(quantmod)
library(magrittr)
library(DT)
library(fs)
library(leaflet)
library(ggmap)
library(readr)
library(mapproj)
library(tidyverse)


# Read and access the live COVID data from JHU CSSE Github page
confirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
recovered <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

# Get states data from mkinlan Guthub page
states <- read.csv("https://raw.githubusercontent.com/mkinlan/rpubs_data/master/states.csv")





# This is the User Interface for the App
ui <- fluidPage(theme = shinytheme("darkly"),
  navbarPage( "Stock vs Covid-19",
    tabPanel("Stock Prices Change",

     sidebarLayout(
       sidebarPanel(
        dateInput("start", "Start Date:", value = "2020-01-01", format = "mm/dd/yy"),
        dateInput("end", "End Date:", value = "2020-07-01", format = "mm/dd/yy"),
        radioButtons("srcInput", "Source",
                   choices = c("yahoo"),
                   selected = "yahoo"),
        selectInput("companyInput", "Company",
                  choices = c("AAPL", "AMZN", "BA", "BABA", "DAL", "FB", "GM", "GOOG", "GS", "JPM", 
                              "NFLX", "TSLA", "TSN", "XOM"))
  
          ),  # sidebarPanel
      mainPanel(
        plotOutput("coolplot")
        
      )  # mainPanel
    )  # Navbar 1, tabPanel
  ),
  tabPanel("Covid19 World Map"),
  tabPanel("US city pandemic vs stock"),
  tabPanel("US state pandemic vs stock"),
  
  tabPanel("Feedback", 
           textInput("caption", "What can we improve?"),
           verbatimTextOutput("value")
           )
  
  ) # navbarPanel
)  # fluidPage


# This is the server of the App
server <- function(input, output) {
  output$coolplot <- renderPlot({
    start <- input$start
    end <- input$end
    company <- input$companyInput
    source <- input$srcInput
    getSymbols(company, src = source, from = start, to = end)
    if(company == "AAPL"){
      plot(AAPL[, "AAPL.Close"], main = "Apple Inc.")}
    else if(company == "AMZN"){
      plot(AMZN[, "AMZN.Close"], main = "Amazon.com, Inc.")}
    else if(company == "BA"){
      plot(BA[, "BA.Close"], main = "The Boeing Company")}
    else if(company == "BABA"){
      plot(BABA[, "BABA.Close"], main = "Alibaba Group Holding Limited")}
    else if(company == "DAL"){
      plot(DAL[, "DAL.Close"], main = "Delta Air Lines, Inc.")}
    else if(company == "FB"){
      plot(FB[, "FB.Close"], main = "Facebook, Inc.")}
    else if(company == "GM"){
      plot(GM[, "GM.Close"], main = "General Motors Company")}
    else if(company == "GOOG"){
      plot(GOOG[, "GOOG.Close"], main = "Alphabet Inc.(Google)")}
    else if(company == "GS"){
      plot(GS[, "GS.Close"], main = "The Goldman Sachs Group, Inc.")}
    else if(company == "JPM"){
      plot(JPM[, "JPM.Close"], main = "JPMorgan Chase & Co.")}
    else if(company == "NFLX"){
      plot(NFLX[, "NFLX.Close"], main = "Netflix, Inc.")}
    else if(company == "TSLA"){
      plot(TSLA[, "TSLA.Close"], main = "Tesla, Inc.")}
    else if(company == "TSN"){
      plot(TSN[, "TSN.Close"], main = "Tyson Foods, Inc.")}
    else if(company == "XOM"){
      plot(XOM[, "XOM.Close"], main = "Exxon Mobil Corporation")}
  })
  
}


shinyApp(ui = ui, server = server)