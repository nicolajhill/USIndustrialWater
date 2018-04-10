#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
#connect to online

library(shiny)
library("tidyverse")
library(plyr)
library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(shiny) 
library("DT")
library("shinythemes")
library("lubridate")
library("lettercase")
library("leaflet")
library("stringr")
library("sf")
library("tidycensus")
library("tigris")
library("maps")
library("plotly")
library("rsconnect")
library(shinythemes)

#Load Necessary Data-----
  #Texas
  TXIndusall<- read.csv("yearlyTXshort4.csv")
  TXIndusall<- subset(TXIndusall,TXIndusall$Year>1970)
  
  
  #IN
  monthlyIN<- read.csv("IN_updated3.csv")  
  INFips<- read.csv("INCountyFIPS.csv")
  monthlyIN$DateProper<-as.Date(as.character(monthlyIN$Date), "%m/%d/%Y")  
  
  #NC
  NC<- read.csv("monthlyNCshort.csv")
  NC$Date<-as.Date(as.character(NC$Date), "%m/%d/%Y")  
  
  #
  usgs<- read.csv("usgs.csv")


#2. Server Script ------
shinyServer(function(input,output,session) { #server is defined within these parentheses
  
  #3. We set the data source and make it change with the date! ------ prep data once and then pass around the program----
  
  passData<- reactive({
    if(input$state == "Texas"){
      data <- filter(TXIndusall, TXIndusall$Year >= input$dateRange[1] &
                       Year <= input$dateRange[2])
      
    }
    if(input$state == "Indiana"){
      data <- filter(monthlyIN, monthlyIN$Year >= input$dateRange1[1] &
                       monthlyIN$Year <= input$dateRange1[2])
      
    }
    if(input$state == "NorthCarolina"){
      data <- filter(NC, NC$Year >= input$dateRange2[1] &
                       NC$Year <= input$dateRange2[2])
      
    }
    return(data)
  })
  })
  
