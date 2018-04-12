#Nicola Hill
#This script is the user interface (ui) portion of the web application.
#The script loads appropriate files/packages and sets up the side bar and tabes


#1. Connect Online-----
library(tidyverse)
library(plyr)
library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(shiny)
library(DT)
library(shinythemes)
library(lubridate)
library(lettercase)
library(leaflet)
library(stringi)
library(stringr)

library(sf)
library(tidycensus)
library(tigris)
library(maps)
library(plotly)



#Scripting the user interface-----
shinyUI(fluidPage(
  theme = shinytheme("cerulean"),
  headerPanel("U.S Industrial Water Usage"),
  sidebarPanel(
    #Drop down to Select a State-----
    wellPanel(
      radioButtons(
        inputId = "state",
        label = "Choose State",
        choices = list(
          "Texas" = "Texas",
          "Indiana" = "Indiana",
          "North Carolina" = "NorthCarolina"
        )
      )
    ),
    
    #Change Date Range on all tabs except comparison & Projections-----
          conditionalPanel(
            condition = "input.state == 'Texas' & (input.tabSelected == 'trends'|
            input.tabSelected == 'Map'| input.tabSelected == 'Summary'|
            input.tabSelected == 'table' )",
            sliderInput (
              "dateRange",
              label = "Date Range",
              sep = "",
              min = 1970,
              max = 2015,
              value = c(1970, 2015)
            )
        ),
        
        conditionalPanel(
          condition = "input.state == 'Indiana'& (input.tabSelected == 'trends'|
          input.tabSelected == 'Map'| input.tabSelected == 'Summary'|
          input.tabSelected == 'table' )",
          sliderInput (
            "dateRange1",
            label = "Date Range",
            sep = "",
            min = 1985,
            max = 2016,
            value = c(1985, 2016)
          )
      ),

        conditionalPanel(
          condition = "input.state == 'NorthCarolina'& (input.tabSelected == 'trends'|
          input.tabSelected == 'Map'| input.tabSelected == 'Summary'|
          input.tabSelected == 'table' )",
          sliderInput (
            "dateRange2",
            label = "Date Range",
            sep = "",
            min = 2008,
            max = 2016,
            value = c(2008, 2016)
          )
          ),




#Add Radio Buttons ------
#Temporal Trends Tab-----
conditionalPanel(
  condition = "input.tabSelected == 'trends'",
  radioButtons(
    inputId = "WaterType",
    label = "Choose Type of Water",
    choices = list(
      "All" = "All",
      "Surface Water" = "SurfaceWater",
      "Groundwater" = "Groundwater",
      "Reuse Water" = "Reuse"
    )
  ),
  conditionalPanel(
    condition = "input.tabSelected=='trends' & input.WaterType== 'All'
    & (input.state == 'Texas'|input.state == 'Indiana') ",
    checkboxInput("USGS", "USGS Data", FALSE)
  )
),

#spatial Trends Tab-----
conditionalPanel(
  condition = "input.tabSelected == 'Map'& (input.state == 'Texas'|
  input.state == 'NorthCarolina'|input.state == 'Indiana') " ,
  radioButtons(
    inputId = "FacWater",
    label = "Show distrbution of:",
    choices = list("Industrial Water Use" = "wateruse",
                   "Facilities" = "facilities")
  )
),

#Largest User Trends Tab----
conditionalPanel(
  condition = "input.tabSelected == 'table' & (input.state == 'Texas' |
  input.state == 'NorthCarolina')",
  radioButtons(
    inputId = "tablePie",
    label = "Show:",
    choices = list(
      "Largest Users" = "Table",
      "Largest NAICS Categories" = "Pie"
    )
  )
  ),
# conditionalPanel(condition="input.tabSelected == 'table' & input.state == 'Indiana'",
#                       radioButtons(inputId = "tablePie",
#                                   label = "Show:",
#                                  choices = list("Largest Users" = "Table"))),

#Add Text to main panel----
#Show Where data is from------
conditionalPanel(
  condition = "input.state == 'Texas'& (input.tabSelected == 'trends'|
  input.tabSelected == 'Map'| input.tabSelected == 'Summary'|
  input.tabSelected == 'Table'|input.tabSelected == 'projections')",
  print(h5(
    em("Data provided by the Texas Water Development Board")
  ))
),

conditionalPanel(
  condition = "input.state == 'Indiana'& (input.tabSelected == 'trends'|
  input.tabSelected == 'Map'| input.tabSelected == 'Summary'|
  input.tabSelected == 'Table'|input.tabSelected == 'projections')",
  print(h5(
    em("Data provided by the Indiana Department of Natural Resources")
  ))
  ),

conditionalPanel(
  condition = "input.state == 'NorthCarolina'& (input.tabSelected == 'trends'|
  input.tabSelected == 'Map'| input.tabSelected == 'Summary'|
  input.tabSelected == 'Pie'|input.tabSelected == 'projections')",
  print(h5(
    em(
      "Data provided by North Carolina Department of Environmental Quality"
    )
  ))
  )



),



#Create Main Panel-----
mainPanel(
  tabsetPanel(
    id = "tabSelected",
    tabPanel(
      strong(h3("Summary")),
      value = "Summary",
      fluidRow(
        #column(10, wellPanel(h4(textOutput("Summary")))),
        column(4, wellPanel(h4(
          textOutput("Summary1")
        ))),
        column(4, wellPanel(h4(
          textOutput("Summary2")
        ))),
        column(4, wellPanel(h4(
          textOutput("Summary3")
        ))),
        column(12, wellPanel(plotOutput("Compare2"))),
        value = "Summary"
      )
    ),
    
    tabPanel(
      strong(h3("Largest Users")),
      value = "table",
      conditionalPanel(
        condition =  "input.tablePie == 'Table'",
        tabPanel(
          "Largest Users",
          value = "Table",
          DT::dataTableOutput("LargestUsers", width = "100%", height =
                                "560px")
        )
      ),
      conditionalPanel(
        condition =  "input.tablePie == 'Pie' & (input.state == 'Texas' |
        input.state == 'NorthCarolina')",
        tabPanel(
          "Largest NAICS Categories",
          value = "Pie",
          plotlyOutput("Pie", width = "100%", height = "560px")
        )
    )
  ),
  
  tabPanel(
    strong(h3("Spatial Trends")),
    leafletOutput(
      "Map",
      value = "Map",
      width = "100%",
      height = "580px"
    )
  ),
  tabPanel(
    strong(h3("Temporal Trends")),
    plotOutput("HistoricalTrends", width = "100%", height =
                 "560px"),
    value = "trends"
  ),
  
  
  tabPanel(
    strong(h3("Water Use Projections")),
    value = "projections",
    plotOutput("Projections", width = "100%", height = "560px")
  )
  
  )
)
))
