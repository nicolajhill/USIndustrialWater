
#This is the server side of the Shiny app- so when a user clicks
    #on a tab, or selects a button, this script says how it reacts

#Sections:
  #1. load .rdata
  #2. Create the server file
    #2a. Prep the data so that it is filtered by the date selected by user
    #2b. Summary Tab: Create info for the summary tab text boxes
    #2bb Comparison Graph for summary tab
    #2c. Largest Users Tab:(Table) Create table of top facilities
    #2cc.Largest Users Tab: (pie) Create pie chart of top users by NAICS category
    #2d. Spatial Trend: Make map of water usage by county using leaflet
    #2dd. Spatial Trend: Make map of facility number by county using leaflet 
    #2e. Temporal Trend: Write a few line for summary tab
    #2f. Projections: load ggplot of projections for each state

#1. Load data .Rdatafile?----
      load(".RData")

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
    

#data prep
#setwd("Z:/Working3_18/Working3_18")
setwd("C:/Users/NicolaHill/Documents/GitHub/USIndustrialWater")

#data prep-----
    #Texas
      TXIndusall<- read.csv("TexasData.csv")
      TXIndusall<- subset(TXIndusall,TXIndusall$Year>1970)

    
    #IN
    IN<- read.csv("INData.csv")  
    IN$DateProper<-as.Date(as.character(IN$Date), "%m/%d/%Y")  
    
    #NC
    NC<- read.csv("NCData.csv")
    NC$Date<-as.Date(as.character(NC$Date), "%m/%d/%Y")  
    
    #
    usgs<- read.csv("usgs.csv")

    
    
#2. Server Script ------
  shinyServer(function(input,output,session) { #server is defined within these parentheses
    
#2a. Make the date interactive
      #We set the data source and make it change with the date!

     passData<- reactive({
       if(input$state == "Texas"){
        data <- filter(TXIndusall, TXIndusall$Year >= input$dateRange[1] &
                         Year <= input$dateRange[2])

       }
       if(input$state == "Indiana"){
        data <- filter(IN, IN$Year >= input$dateRange1[1] &
                             IN$Year <= input$dateRange1[2])
       
        }
       if(input$state == "NorthCarolina"){
         data <- filter(NC, NC$Year >= input$dateRange2[1] &
                         NC$Year <= input$dateRange2[2])
         
       }
       return(data)
     })
     
#2b. summary tabs/ info in text boxes 
     observe(if(input$state== "Indiana" |input$state== "NorthCarolina"| input$state== "Texas" ){
       output$Summary<- renderText({
         paste0("Key facts in this time period:")})
     })
     observe(if(input$state== "Indiana"){
       output$Summary1<- renderText({
         
         #Percent Surface Water  ----
         GroupSource<- passData() %>%
           dplyr::group_by(SourceCode)%>%
           dplyr::summarise("TotalWaterUseMG" = sum(monthlyWaterUseMG)) %>%
           dplyr:: mutate(Percent = round(TotalWaterUseMG/ sum(TotalWaterUseMG)*100))
         paste0("Key facts:")
         TextWater<- paste0("Surface Water Use:")
         TextWater2<- paste0(" ", GroupSource$Percent[GroupSource$SourceCode =="Surface Water"], "%")
         paste0(TextWater, TextWater2)
       })
       
       output$Summary2<- renderText({
         
         
         
         #organization company use
         groupUsers<- passData()%>%
           dplyr::group_by(Facility)%>%
           dplyr::summarise('Total Water Usage (MG)'=  
                              as.numeric(round(sum(monthlyWaterUseMG))))
         sortedUsers<- arrange(groupUsers, desc(groupUsers$`Total Water Usage (MG)`))
         
         
         TextUser<-paste0("Largest water user: ")
         extraline<- "                   "
         textuser2<- paste0(head(sortedUsers$Facility, 1),".")
         
         paste(TextUser,extraline, textuser2)
         
         
       })
       output$Summary3<- renderText({
         
         #Percent Surface Water  ----
         GroupSource<- passData() %>%
           dplyr::group_by(SourceCode)%>%
           dplyr::summarise("TotalWaterUseMG" = sum(monthlyWaterUseMG)) %>%
           dplyr:: mutate(Percent = round(TotalWaterUseMG/ sum(TotalWaterUseMG)*100))
         paste0("Key facts:")
         TextWater<- paste0("Groundwater accounts for", " ", GroupSource$Percent[GroupSource$SourceCode =="Groundwater"], "%",
                            " of the total water used in this time period.")
         
         #organization company use
         groupUsers<- passData()%>%
           dplyr::group_by(Facility)%>%
           dplyr::summarise('Total Water Usage (MG)'=  
                              as.numeric(round(sum(monthlyWaterUseMG))))
         sortedUsers<- arrange(groupUsers, desc(groupUsers$`Total Water Usage (MG)`))
         
         
         TextUser<-paste0(" They have used ", format(head(sortedUsers$`Total Water Usage (MG)`, 1),big.mark = ","), 
                          " MG.")
         
         paste0(TextUser, collapse = "\n")
         
         
       })
     })
     observe(if (input$state == "Texas"){
       output$Summary1<- renderText({
         
         #Percent Surface Water  ----
         GroupPurSS<- passData() %>%
           dplyr::group_by(PurchasedSS)%>%
           dplyr::summarise("TotalWaterUseMG" = sum(monthlyWaterUseMG, na.rm = TRUE)) %>%
           dplyr:: mutate(Percent = round(TotalWaterUseMG/ sum(TotalWaterUseMG)*100))
         paste0("Key facts:")
         TextWater<- paste0("Surface water use: ", GroupPurSS$Percent[GroupPurSS$PurchasedSS =="SS"], "%")
         
         
         paste0(TextWater)
         
         
       })
       output$Summary2<- renderText({
         
         
         #Aquifer with the largest withdrawal
         #group by the aquifer name----
         GroupBasin<- passData() %>%
           dplyr::group_by(BasinSource)%>%
           dplyr::summarise("TotalWaterUseMG" = format(round(as.numeric(sum(monthlyWaterUseMG))), big.mark = ","))
         colnames(GroupBasin)[1]<- "Basin"
         GroupBasin$Basin <-str_ucfirst(as.character(GroupBasin$Basin))
         TextAquifer<- paste0(" The aquifier with the most groundwater withdrawal: ", str_ucfirst(head(GroupBasin$Basin, 1)),".")
         
         
         paste0(TextAquifer)
         
         
       })
       output$Summary3<- renderText({
         
         
         #organization company use
         groupUsers<- passData()%>%
           dplyr::group_by(Organization, Description)%>%
           dplyr::summarise('Total Water Usage (MG)'=  
                              as.numeric(round(sum(monthlyWaterUseMG))))
         sortedUsers<- arrange(groupUsers, desc(groupUsers$`Total Water Usage (MG)`))
         TextUser<-paste0("Largest water user:", "\n", head(sortedUsers$Organization, 1),", which specializes in ", 
                          head(sortedUsers$Description, 1, ".", "They have used a total of ", head(sortedUsers$`Total Water Usage (MG)`, 1), "MG"))
         
         paste0(TextUser)
         
         
       })
     })
     observe(if(input$state== "NorthCarolina"){
       output$Summary1<- renderText({
         
         #Get Data ----
         GroupSource<- passData()%>%
           dplyr::group_by(NewWaterType)%>%
           dplyr::summarise("TotalWaterUseMG" = sum(monthlyWaterUseMG)) %>%
           dplyr:: mutate(Percent = round(TotalWaterUseMG/ sum(TotalWaterUseMG)*100))
         
         groupUsers<- passData()%>%
           dplyr::group_by(FacilityName, NewSubType)%>%
           dplyr::summarise('Total Water Usage (MG)'=  
                              as.numeric(round(sum(monthlyWaterUseMG))))
         sortedUsers<- arrange(groupUsers, desc(groupUsers$`Total Water Usage (MG)`))
         colnames(sortedUsers)[1]<- "Facility Name"
         colnames(sortedUsers)[2]<- "Facility Description"
         
         #Text
         TextWater1<- paste0("Groundwater Use: ", GroupSource$Percent[GroupSource$NewWaterType =="Groundwater"], "%",
                             " ", "Surface water use: ", GroupSource$Percent[GroupSource$NewWaterType =="Surface Water"],
                             "%.")
         TextWater<- paste0("Surface water use: ", GroupSource$Percent[GroupSource$NewWaterType =="Surface Water"],
                            "%.")
         
         paste(TextWater1)
         paste0(TextWater)
         
         
       })
       output$Summary2<- renderText({
         
         #Get Data ----
         GroupSource<- passData()%>%
           dplyr::group_by(NewWaterType)%>%
           dplyr::summarise("TotalWaterUseMG" = sum(monthlyWaterUseMG)) %>%
           dplyr:: mutate(Percent = round(TotalWaterUseMG/ sum(TotalWaterUseMG)*100))
         
         groupUsers<- passData()%>%
           dplyr::group_by(FacilityName, NewSubType)%>%
           dplyr::summarise('Total Water Usage (MG)'=  
                              as.numeric(round(sum(monthlyWaterUseMG))))
         sortedUsers<- arrange(groupUsers, desc(groupUsers$`Total Water Usage (MG)`))
         colnames(sortedUsers)[1]<- "Facility Name"
         colnames(sortedUsers)[2]<- "Facility Description"
         
         #Text
         TextWater<- paste0("Groundwater and surface water account for ", GroupSource$Percent[GroupSource$NewWaterType =="Groundwater"], "%",
                            " and ", GroupSource$Percent[GroupSource$NewWaterType =="Surface Water"],
                            "%"," of the total water used in this time period respectively.")
         
         TextUser<- paste0("Largest user: ", head(sortedUsers$`Facility Name`, 1),
                           ", which specializes in ", head(sortedUsers$'Facility Description', 1), ".")
         
         paste0(TextUser)
         
         
       })
       output$Summary3<- renderText({
         
         #Get Data ----
         GroupSource<- passData()%>%
           dplyr::group_by(NewWaterType)%>%
           dplyr::summarise("TotalWaterUseMG" = sum(monthlyWaterUseMG)) %>%
           dplyr:: mutate(Percent = round(TotalWaterUseMG/ sum(TotalWaterUseMG)*100))
         
         groupUsers<- passData()%>%
           dplyr::group_by(FacilityName, NewSubType)%>%
           dplyr::summarise('Total Water Usage (MG)'=  
                              as.numeric(round(sum(monthlyWaterUseMG))))
         sortedUsers<- arrange(groupUsers, desc(groupUsers$`Total Water Usage (MG)`))
         colnames(sortedUsers)[1]<- "Facility Name"
         colnames(sortedUsers)[2]<- "Facility Description"
         
         #Text
         TextWater<- paste0("Groundwater and surface water account for ", GroupSource$Percent[GroupSource$NewWaterType =="Groundwater"], "%",
                            " and ", GroupSource$Percent[GroupSource$NewWaterType =="Surface Water"],
                            "%"," of the total water used in this time period respectively.")
         
         TextUser<- paste0("Their water use:", format(head(sortedUsers$`Total Water Usage (MG)`, 1),big.mark = ","),
                           " MG")
         
         paste0(TextUser)
         
         
       })
     })
     
#2bb Comparison Graph for summary tab -----    
     # I use the original data sets (instead of using the passData() command)
     observe(if (input$state == "Indiana"){
       output$Compare2 <- renderPlot({ 
         #TExas
         TXgraphdata<-TXIndusall %>%
           dplyr::group_by(Year) %>%
           dplyr::summarise(monthlyWaterUseMG = sum(monthlyWaterUseMG, na.rm = TRUE ))
         #IN
         INgraphdata<-IN %>%
           dplyr::group_by(Year) %>%
           dplyr::summarise(monthlyWaterUseMG = sum(monthlyWaterUseMG, na.rm = TRUE ))
         #NC
        
         NCgraphdata<- NC %>%
           dplyr::group_by(Year) %>%
           dplyr::summarise(monthlyWaterUseMG = sum(monthlyWaterUseMG, na.rm = TRUE ))
         
         
         
         #Graph of total water usage------
         theGraph <- ggplot(data= TXgraphdata,aes(TXgraphdata$Year,TXgraphdata$monthlyWaterUseMG)) + 
           geom_line(aes(color="Texas"), size= 1) +
           labs(x ="\n Year", y="\nIndustrial Water Use (MG)\n", 
                title = "\n State Industrial Water Use \n",
                caption= "Dashed line is data provided by USGS 5 year reports") +
           scale_y_continuous(labels=comma, breaks = seq(0,1750000, by =200000))+
           scale_x_continuous(breaks = seq(1970,2015, by= 5))+
           #geom_ribbon(ymin=0, ymax=graphdata$monthlyWaterUseMG, alpha=0.3)+
           theme(axis.text =element_text(size=16), axis.title=element_text(size=20, face= "bold"), 
                 legend.title = element_text(size = 18, face = "bold"), 
                 legend.text = element_text(size = 16),
                 plot.title = element_text(size = 22, face = "bold"),
                 plot.caption = element_text(size=16))
         
         #plot the graph with all states
         theGraph <- theGraph+
           geom_line(data= NCgraphdata, aes(NCgraphdata$Year,NCgraphdata$monthlyWaterUseMG, color='North Carolina'), size= 1)+
           geom_line(data= INgraphdata, aes(INgraphdata$Year,INgraphdata$monthlyWaterUseMG, color='Indiana'), size= 1.6)+
           geom_line(linetype=2, data= usgs, aes(usgs$Year, usgs$North.Carolina, color= 'North Carolina'),size= 1)+
           geom_point(data= usgs, aes(usgs$Year, usgs$North.Carolina, color= 'North Carolina'),size= 2)+
           scale_colour_discrete(name="State")
         
         print(theGraph)
       })  
     })
     observe(if (input$state == "Texas"){
       output$Compare2 <- renderPlot({ 
         #TExas
         TXgraphdata<-TXIndusall%>%
           dplyr::group_by(Year) %>%
           dplyr::summarise(monthlyWaterUseMG = sum(monthlyWaterUseMG, na.rm = TRUE ))
         #IN
         INgraphdata<-IN %>%
           dplyr::group_by(Year) %>%
           dplyr::summarise(monthlyWaterUseMG = sum(monthlyWaterUseMG, na.rm = TRUE ))
         #NC
         NCgraphdata<- NC %>%
           dplyr::group_by(Year) %>%
           dplyr::summarise(monthlyWaterUseMG = sum(monthlyWaterUseMG, na.rm = TRUE ))
         
         
         
         #Graph of total water usage------
         theGraph <- ggplot(data= TXgraphdata,aes(TXgraphdata$Year,TXgraphdata$monthlyWaterUseMG)) + 
           geom_line(aes(color="Texas"), size= 1.6) +
           labs(x ="\n Year", y="\nIndustrial Water Use (MG)\n", 
                title = "\n State Industrial Water Use \n",
                caption= "Dashed line is data provided by USGS 5 year reports") +
           scale_y_continuous(labels=comma, breaks = seq(0,1750000, by =200000))+
           scale_x_continuous(breaks = seq(1970,2015, by= 5))+
           #geom_ribbon(ymin=0, ymax=graphdata$monthlyWaterUseMG, alpha=0.3)+
           theme(axis.text =element_text(size=16), axis.title=element_text(size=20, face= "bold"), 
                 legend.title = element_text(size = 18, face = "bold"), 
                 legend.text = element_text(size = 16),
                 plot.title = element_text(size = 22, face = "bold"),
                 plot.caption = element_text(size=16))
         
         
         #plot the graph with all states
         theGraph <- theGraph+
           geom_line(data= NCgraphdata, aes(NCgraphdata$Year,NCgraphdata$monthlyWaterUseMG, color='North Carolina'), size= 1)+
           geom_line(data= INgraphdata, aes(INgraphdata$Year,INgraphdata$monthlyWaterUseMG, color='Indiana'), size= 1)+
           geom_line(linetype=2, data= usgs, aes(usgs$Year, usgs$North.Carolina, color= 'North Carolina'),size= 1)+
           geom_point(data= usgs, aes(usgs$Year, usgs$North.Carolina, color= 'North Carolina'),size= 2)+
           scale_colour_discrete(name="State")
         
         print(theGraph)
       })        
     })
     observe(if (input$state == "NorthCarolina"){
       output$Compare2 <- renderPlot({ 
         #TExas
         TXgraphdata<-TXIndusall %>%
           dplyr::group_by(Year) %>%
           dplyr::summarise(monthlyWaterUseMG = sum(monthlyWaterUseMG, na.rm = TRUE ))
         #IN
         INgraphdata<-IN %>%
           dplyr::group_by(Year) %>%
           dplyr::summarise(monthlyWaterUseMG = sum(monthlyWaterUseMG, na.rm = TRUE ))
         #NC
         NCgraphdata<- NC %>%
           dplyr::group_by(Year) %>%
           dplyr::summarise(monthlyWaterUseMG = sum(monthlyWaterUseMG, na.rm = TRUE ))
         
         
         
         #Graph of total water usage------
         theGraph <- ggplot(data= TXgraphdata,aes(TXgraphdata$Year,TXgraphdata$monthlyWaterUseMG)) + 
           geom_line(aes(color="Texas"), size= 1) +
           labs(x ="\n Year", y="\nIndustrial Water Use (MG)\n", 
                title = "\n State Industrial Water Use \n",
                caption= "Dashed line is data provided by USGS 5 year reports") +
           scale_y_continuous(labels= scales::comma, breaks = seq(0,1750000, by =200000))+
           scale_x_continuous(breaks = seq(1970,2015, by= 5))+
           #geom_ribbon(ymin=0, ymax=graphdata$monthlyWaterUseMG, alpha=0.3)+
           theme(axis.text =element_text(size=16), axis.title=element_text(size=20, face= "bold"), 
                 legend.title = element_text(size = 18, face = "bold"), 
                 legend.text = element_text(size = 16),
                 plot.title = element_text(size = 22, face = "bold"),
                 plot.caption = element_text(size=16))
         
         #plot the graph with all states
         theGraph <- theGraph+
           geom_line(data= NCgraphdata, aes(NCgraphdata$Year,NCgraphdata$monthlyWaterUseMG, color='North Carolina'), size= 1.6)+
           geom_line(data= INgraphdata, aes(INgraphdata$Year,INgraphdata$monthlyWaterUseMG, color='Indiana'), size= 1)+
           geom_line(linetype=2, data= usgs, aes(usgs$Year,usgs$North.Carolina, color= 'North Carolina'),size= 1.6)+
           geom_point(data= usgs, aes(usgs$Year,usgs$North.Carolina, color= 'North Carolina'),size= 2)+
           scale_colour_discrete(name="State")
         
         print(theGraph)
       })        
     })

#2c largest Users (table)------
     observe(if(input$state== "Texas" & input$tablePie =="Table"){
       
       output$LargestUsers<- DT::renderDataTable({
         
         #get the data grouped
         groupUsers<- passData()%>%
           dplyr::group_by(Organization, Description)%>%
           dplyr::summarise('Total Water Usage (MG)'=  
                              as.numeric(round(sum(monthlyWaterUseMG))))
         sortedUsers<- arrange(groupUsers, desc(groupUsers$`Total Water Usage (MG)`))
         datatable(sortedUsers)%>%
           formatCurrency(columns="Total Water Usage (MG)", currency= "", mark= ",", digits=0 )
       })
     })
     observe(if (input$state == "Indiana") {
       output$LargestUsers<- DT::renderDataTable({
         #get the data grouped
         groupUsers<- passData()%>%
           dplyr::group_by(Facility)%>%
           dplyr::summarise('Total Water Usage (MG)'=  
                              as.numeric(round(sum(monthlyWaterUseMG))))
         sortedUsers<- arrange(groupUsers, desc(groupUsers$`Total Water Usage (MG)`))
         datatable(sortedUsers)%>%
           formatCurrency(columns="Total Water Usage (MG)", currency= "", mark= ",", digits=0 )
         
       })
     })
     observe(if(input$state== "NorthCarolina"& input$tablePie =="Table"){
       output$LargestUsers<- DT::renderDataTable({
         #get the data grouped
         groupUsers<- passData()%>%
           dplyr::group_by(FacilityName, NewSubType)%>%
           dplyr::summarise('Total Water Usage (MG)'=  
                              as.numeric(round(sum(monthlyWaterUseMG))))
         sortedUsers<- arrange(groupUsers, desc(groupUsers$`Total Water Usage (MG)`))
         colnames(sortedUsers)[1]<- "Facility Name"
         colnames(sortedUsers)[2]<- "Facility Description"
         datatable(sortedUsers)%>%
           formatCurrency(columns="Total Water Usage (MG)", currency= "", mark= ",", digits=0 )
         
         
       })
     })
     
#2cc largest Users (pie)------
     observe(if(input$state== "Texas" & input$tablePie =="Pie"){
       
       output$Pie <- renderPlotly({
         #prep data
         NAICSGROUP<- passData()%>%
           dplyr::group_by(DescShort) %>%
           dplyr::summarise(TotalWaterUseMG = sum(monthlyWaterUseMG, na.rm = TRUE )) %>% 
           dplyr:: mutate(Percent = round(TotalWaterUseMG/ sum(TotalWaterUseMG)*100)) %>% 
           dplyr::arrange(desc(TotalWaterUseMG))
         
         #subset data to the top 5 NAICS groups
         Piedata<- NAICSGROUP[1:5,]
         
         #get the sum of the last 6 NAICS categories
         #Sum the water use
         other<- NAICSGROUP %>% 
           slice(6:n()) %>% 
           dplyr::summarise(TotalWaterUseMG = sum(TotalWaterUseMG, na.rm = TRUE ))
         
         #make new column for percent
         otherP<- NAICSGROUP %>% 
           slice(6:n()) %>% 
           dplyr::summarise(Percent = sum(Percent, na.rm = TRUE ))
         
         
         #make new table that has the row information for the 'other' category
         Other<- data.frame("Other", other, otherP )
         colnames(Other)[1]<- "DescShort"
         
         #Now combine the table with the top five and addedlast row for 'other'
         Piedata<-rbind(Piedata, Other)
         
         
         #Plot Pie Chart
         colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
         
         pie<- plot_ly(Piedata, labels = Piedata$DescShort , values = Piedata$Percent, type = 'pie',
                       textinfo = 'label+percent',
                       insidetextfont = list(color = '#FFFFFF'),
                       marker = list(colors = colors,
                                     line = list(color = '#FFFFFF', width = 1)),
                       showlegend = TRUE) %>%
           layout(title = "Total Water Use by NAICS Characterization",
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
         print(pie)
         
       })
     })
     observe(if(input$state== "NorthCarolina"& input$tablePie =="Pie"){
       
       output$Pie <- renderPlotly({ 
         #Organize data
         NAICSGroupNC<- passData() %>% 
           dplyr::group_by(NewSubType) %>%
           dplyr::summarise(TotalWaterUseMG = sum(monthlyWaterUseMG, na.rm = TRUE )) %>% 
           dplyr:: mutate(Percent = round(TotalWaterUseMG/ sum(TotalWaterUseMG)*100)) %>% 
           dplyr::arrange(desc(TotalWaterUseMG))
         
         # Make plot
         colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
         
         pie<- plot_ly(NAICSGroupNC, labels = NAICSGroupNC$NewSubType , values = NAICSGroupNC$TotalWaterUseMG, type = 'pie',
                       textinfo = 'label+percent',
                       insidetextfont = list(color = '#FFFFFF'),
                       marker = list(colors = colors,
                                     line = list(color = '#FFFFFF', width = 1)),
                       showlegend = TRUE) %>%
           layout(title = "Total Water Use by NAICS Characterization",
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
         
         print(pie)
       })
     })
     
     
     
#2d.Spatial Trends: Map trends tab Water use
     #I used a leaflet to create the cholorpleth. 
          #I first followed https://juliasilge.com/blog/using-tidycensus/
          #but then I saved the actual file so the code wouldn't have to keep downloading the file from online
     
     observe(if(input$state == "Texas" & input$FacWater == "wateruse"){
       output$Map<- renderLeaflet({
         #get data ready
         Mapdata<- passData()%>%
           dplyr::group_by(FIPS ) %>%
           dplyr::summarise("Water Use (MG)" = 
                              sum(monthlyWaterUseMG, na.rm = TRUE ))
         
         #change column names of Mapdata file to FIPS
         names(Mapdata)[1] <- paste("FIPS")
         as.character(Mapdata$FIPS)

         #pull Texas ACS data the shapefile
         TXshape <- read_sf("TX.shp")
         
         #Create a new column named 'FIPS'
         TXshape$FIPS <- as.integer(TXshape$GEOID)
         
         
         #merge water usage data with this data
         mapmergeTX<- geo_join(TXshape, Mapdata, "FIPS", "FIPS", how = "left") 
         mapmergeTX$log<- log10(mapmergeTX$`Water Use (MG)`)
         
         #make map color palette
         pal <- colorNumeric(palette = "viridis", domain = mapmergeTX$log)
         
         #Create the leaflet!
         TXshape %>%
           st_transform(crs = "+init=epsg:4326") %>%
           leaflet(width = "100%") %>%
           addProviderTiles(provider = "CartoDB.Positron") %>%
           addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                       stroke = FALSE,
                       smoothFactor = 0,
                       fillOpacity = 0.7,
                       color = ~ pal(mapmergeTX$log)) %>%
           addLegend("bottomright", 
                     pal = pal, 
                     values = ~ mapmergeTX$log,
                     title = "Log of Water Use (MG)",
                     opacity = 1) 
       })
     }) 
     observe(if(input$state == "Indiana"  & input$FacWater == "wateruse"){
       output$Map<- renderLeaflet({
         #get data ready
         Mapdata<- passData() %>%
           dplyr::group_by(FIPS) %>%
           dplyr::summarise("Water Use (MG)" = 
                              sum(monthlyWaterUseMG, na.rm = TRUE ))
         
         #Load in the shapefile
         INshape <- read_sf("IN.shp")
         
         #change the name of the GEOID to fIPS and make it an integer
         INshape$FIPS <- as.integer(INshape$GEOID)
         
         #merge water usage data with this data
         mapmergeIN<- geo_join(INshape, Mapdata, "FIPS", "FIPS", how = "left") 
         
         
         #make map color palette
         bins<- c(0,500,1000,3000,5000,7000,10000,15000,20000,25000)
         pal <- colorBin(palette = "viridis", domain = mapmergeIN$'Water Use (MG)', bin=bins)
         
         #make leaflet map
         INshape %>%
           st_transform(crs = "+init=epsg:4326") %>%
           leaflet(width = "100%") %>%
           addProviderTiles(provider = "CartoDB.Positron") %>%
           addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                       stroke = FALSE,
                       smoothFactor = 0,
                       fillOpacity = 0.7,
                       color = ~ pal(mapmergeIN$`Water Use (MG)`)) %>%
           addLegend("bottomright", 
                     pal = pal, 
                     values = ~ mapmergeIN$`Water Use (MG)`,
                     title = "Water Use (MG)",
                     opacity = 1)  
         
       })     
     })
     observe(if(input$state == "NorthCarolina" & input$FacWater == "wateruse"){  
       output$Map<- renderLeaflet({
         
         #get data ready
         Mapdata<-passData()%>%
           dplyr::group_by(County ) %>%
           dplyr::summarise("Water Use (MG)" = 
                              sum(monthlyWaterUseMG, na.rm = TRUE ))
         Mapdata$County<- toupper(Mapdata$County)
         Mapdata$County <- as.character(Mapdata$County)

         
         #pull NorthCarolina shapefile
         NCshape <- read_sf("NC.shp")
         
         #the county names are written like "XXX county", so place the county name in new column
         NCshape$County<- word(NCshape$NAME, 1)
         NCshape$County<- toupper(NCshape$County)

         
         #merge water usage data with this data
         mapmergeNC<- geo_join(NCshape, Mapdata, "County", "County", how = "left") 
         
         
         #make map color palette
         pal <- colorNumeric(palette = "viridis", domain = mapmergeNC$'Water Use (MG)')
         
         #make leaflet!
         NCshape %>%
           st_transform(crs = "+init=epsg:4326") %>%
           leaflet(width = "100%") %>%
           addProviderTiles(provider = "CartoDB.Positron") %>%
           addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                       stroke = FALSE,
                       smoothFactor = 0,
                       fillOpacity = 0.7,
                       color = ~ pal(mapmergeNC$`Water Use (MG)`)) %>%
           addLegend("bottomright", 
                     pal = pal, 
                     values = ~ mapmergeNC$`Water Use (MG)`,
                     title = "Water Use (MG)",
                     opacity = 1)  
         
       })    
     })

#2dd Spatial Trends: Map of Facility Number
     observe(if(input$state == "Texas"& input$FacWater == "facilities"){
       output$Map <- renderLeaflet({
         
         #get data ready
         Mapdata<- passData() %>%
           dplyr::count(FIPS, Organization, Year) %>%
           dplyr::count(Organization, FIPS) %>% 
           dplyr::group_by(FIPS) %>% 
           dplyr::summarise(Facility = n_distinct(Organization))

         #change column names of Mapdata file
         names(Mapdata)[1] <- paste("FIPS")
         as.character(Mapdata$FIPS)
         
         #pull shapefile
         TXshape <- read_sf("TX.shp")

         #create a new column called FIPS
         TXshape$FIPS <- as.integer(TXshape$GEOID)


         #merge the number of facilites with the shapefile
         mapmergeTX<- geo_join(TXshape, Mapdata, "FIPS", "FIPS", how = "left") 
         mapmergeTX$log<- log10(as.numeric(mapmergeTX$Facility))
         
         #make map color palette
         pal <- colorNumeric(palette = "viridis", domain = mapmergeTX$log)
         
         #make a leaflet map of facilites
         TXshape %>%
           st_transform(crs = "+init=epsg:4326") %>%
           leaflet(width = "100%") %>%
           addProviderTiles(provider = "CartoDB.Positron") %>%
           addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                       stroke = FALSE,
                       smoothFactor = 0,
                       fillOpacity = 0.7,
                       color = ~ pal(mapmergeTX$log)) %>%
           addLegend("bottomright", 
                     pal = pal, 
                     values = ~ mapmergeTX$log,
                     title = "Log (Number of Facilities)",
                     opacity = 1) 
       })
     }) 
     observe(if(input$state == "NorthCarolina" & input$FacWater == "facilities"){  
       output$Map<- renderLeaflet({
         
         #get data ready
         Mapdata<-passData() %>%
           dplyr::count(County, FacilityName, Year) %>%
           dplyr::count(County, FacilityName) %>% 
           dplyr::group_by(County) %>% 
           dplyr::summarise(Facility = n_distinct(FacilityName))
         
         
        #capitalize the county names
         Mapdata$County<- toupper(Mapdata$County)
         
         #pull NorthCarolina shapefile
         NCshape <- read_sf("NC.shp")
         
         #the county names are written like "XXX county", so place the county name in new column
         NCshape$County<- word(NCshape$NAME, 1)
         NCshape$County<- toupper(NCshape$County)
         
         
         #merge water usage data with this data
         mapmergeNC<- geo_join(NCshape, Mapdata, "County", "County", how = "left") 
         
         
         #make map color palette
         pal <- colorNumeric(palette = "viridis", domain = mapmergeNC$Facility)
        
         #make a leaflet map of facilites 
         NCshape %>%
           st_transform(crs = "+init=epsg:4326") %>%
           leaflet(width = "100%") %>%
           addProviderTiles(provider = "CartoDB.Positron") %>%
           addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                       stroke = FALSE,
                       smoothFactor = 0,
                       fillOpacity = 0.7,
                       color = ~ pal(mapmergeNC$Facility)) %>%
           addLegend("bottomright", 
                     pal = pal, 
                     values = ~ mapmergeNC$Facility,
                     title = "Number of Facilities",
                     opacity = 1)  
         
       })    
     })
     observe(if(input$state == "Indiana"  & input$FacWater == "facilities"){
       output$Map<- renderLeaflet({
         #get data ready
         Mapdata<- passData() %>%
           dplyr::count(FIPS, Facility, Year) %>%
           dplyr::count(FIPS, Facility) %>% 
           dplyr::group_by(FIPS) %>% 
           dplyr::summarise(Facility = n_distinct(Facility))
         
        
         #pull shapefiles
         INshape <- read_sf("IN.shp")
         
         #change the name of the GEOID to fIPS and make it an integer
         INshape$FIPS <- as.integer(INshape$GEOID)
         
         #merge water usage data with this data
         mapmergeIN<- geo_join(INshape, Mapdata, "FIPS", "FIPS", how = "left") 
         
         
         #make map color palette
         pal <- colorNumeric(palette = "viridis", domain = mapmergeIN$Facility)
         
         #make a leaflet map of facilites
         INshape %>%
           st_transform(crs = "+init=epsg:4326") %>%
           leaflet(width = "100%") %>%
           addProviderTiles(provider = "CartoDB.Positron") %>%
           addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                       stroke = FALSE,
                       smoothFactor = 0,
                       fillOpacity = 0.7,
                       color = ~ pal(mapmergeIN$Facility)) %>%
           addLegend("bottomright", 
                     pal = pal, 
                     values = ~ mapmergeIN$Facility,
                     title = "Number of Facilities",
                     opacity = 1)  
         
       })     
     })
     

#2e Temporal Trends: graph of historical trends-----
     observe(if (input$state == "Indiana"){
       #Update the radio buttons to reflect the Indiana data
       updateRadioButtons(session,  "WaterType",
                          choices = list("All"= "All","Surface Water" = "SurfaceWater", 
                                         "Groundwater" = "Groundwater"))
       
      output$HistoricalTrends <- renderPlot({
         
         #Get the data ready to be plotted
          #Total use
           graphdata<-passData() %>%
             dplyr::group_by(Year) %>%
             dplyr::summarise(monthlyWaterUseMG = sum(monthlyWaterUseMG, na.rm = TRUE ))
           #Source- Intake (surface water)
           graphswdata<-passData()[passData()$SourceCode == "Surface Water",]%>%
             dplyr::group_by(Year) %>%
             dplyr::summarise(monthlyWaterUseMG = sum(monthlyWaterUseMG, na.rm = TRUE ))
           #Source- Well (groundwater)
           graphgwdata<-passData()[passData()$SourceCode == "Groundwater",]%>%
             dplyr::group_by(Year) %>%
             dplyr::summarise(monthlyWaterUseMG = sum(monthlyWaterUseMG, na.rm = TRUE ))
         
         
         #Graph of total water usage
         theGraph <- ggplot(data= graphdata,aes(graphdata$Year,graphdata$monthlyWaterUseMG)) + 
           geom_line(aes(color="Total Water"), size= 1) +
           labs(x ="Year", y="\n Industrial Water Use (MG)\n", 
                title = "\nIndiana Industrial Water Use by Water Type\n") +
           scale_y_continuous(labels = scales::comma)+#, breaks = seq(0,900000, by =200000))+
           scale_x_continuous(breaks = seq(1970,2020, by= 5))+
           theme(axis.text =element_text(size=16), axis.title=element_text(size=20, face= "bold"), 
                 legend.title = element_text(size = 18, face = "bold"), 
                 legend.text = element_text(size = 16),
                 plot.title = element_text(size = 22, face = "bold"))
         

         if(input$WaterType == "SurfaceWater"){
           theGraph <- theGraph +
             geom_line(data= graphswdata, aes(graphswdata$Year,graphswdata$monthlyWaterUseMG, color='Surface Water'),size= 1)+
             scale_colour_discrete(name="Water Type")
         }
         if(input$WaterType == "Groundwater"){
           theGraph <- theGraph +
             geom_line(data= graphgwdata, aes(graphgwdata$Year,graphgwdata$monthlyWaterUseMG, color='Groundwater'), size= 1)+
             scale_colour_discrete(name="Water Type")
         }
         if(input$WaterType == ("All" )){ 
           theGraph <- theGraph+
             geom_line(data= graphswdata, aes(graphswdata$Year,graphswdata$monthlyWaterUseMG, color='Surface Water'), size= 1) +
             geom_line(data= graphgwdata, aes(graphgwdata$Year,graphgwdata$monthlyWaterUseMG, color='Groundwater'), size= 1)+
             scale_colour_discrete(name="Water Type")
         }
         #Add a section for if the user checks the checkbox for adding USGS data
         if(input$WaterType == ("All") & input$USGS == TRUE){ 
           theGraph <- theGraph+
             geom_line(data= graphswdata, aes(graphswdata$Year,graphswdata$monthlyWaterUseMG, color='Surface Water'), size= 1) +
             geom_line(data= graphgwdata, aes(graphgwdata$Year,graphgwdata$monthlyWaterUseMG, color='Groundwater'), size= 1)+
             geom_line(linetype=2, data= usgs, aes(usgs$Year, usgs$Indiana, color="USGS"), size = 1)+
             geom_point( data= usgs, aes(usgs$Year, usgs$Indiana, color="USGS"),size= 2.5)+
             scale_colour_discrete(name="Water Type")
         }

         print(theGraph)
       })
     }) 
     observe(if(input$state == "Texas"){
       #update radio buttons to reflect Texas data
       updateRadioButtons(session,  "WaterType",
                          choices = list("All"= "All",
                                         "Surface Water" = "SurfaceWater", 
                                         "Groundwater" = "Groundwater",
                                         "Reuse Water"= "Reuse"))
       
       output$HistoricalTrends <- renderPlot({
         #Get the data ready to be plotted!!------
         graphdata<-passData() %>%
           dplyr::group_by(Year) %>%
           dplyr::summarise(monthlyWaterUseMG = sum(monthlyWaterUseMG, na.rm = TRUE ))
         #Groundwater
         graphGWdata<-passData()[passData()$WaterType == "Groundwater",]%>%
           dplyr::group_by(Year) %>%
           dplyr::summarise(monthlyWaterUseMG = sum(monthlyWaterUseMG, na.rm = TRUE ))
         #SurfaceWater
         graphSWdata<-passData()[passData()$WaterType == "Surface Water",]%>%
           dplyr::group_by(Year) %>%
           dplyr::summarise(monthlyWaterUseMG = sum(monthlyWaterUseMG, na.rm = TRUE ))
         #ReuseWater
         graphRWdata<-passData()[passData()$WaterType == "Reuse",]%>%
           dplyr::group_by(Year) %>%
           dplyr::summarise(monthlyWaterUseMG = sum(monthlyWaterUseMG, na.rm = TRUE )) 
         
         #Graph of total water usage------
         theGraph <- ggplot(data= graphdata,aes(graphdata$Year,graphdata$monthlyWaterUseMG)) + 
           geom_line(aes(color="Total Water"), size= 1) +
           labs(x ="Year", y="Industrial Water Use (MG)\n", 
                title = "\n Texas Industrial Water Use by Water Type\n") +
           scale_y_continuous(labels = scales::comma, breaks = seq(0,1600000, by =200000))+
           scale_x_continuous(breaks = seq(1970,2015, by= 5))+
           #geom_ribbon(ymin=0, ymax=graphdata$monthlyWaterUseMG, alpha=0.3)+
           theme(axis.text =element_text(size=16), axis.title=element_text(size=20, face= "bold"), 
                 legend.title = element_text(size = 18, face = "bold"), 
                 legend.text = element_text(size = 16),
                 plot.title = element_text(size = 22, face = "bold"))
         
         
         if(input$WaterType == "Groundwater"){ 
           theGraph <- theGraph +
             geom_line(data= graphGWdata, aes(graphGWdata$Year,graphGWdata$monthlyWaterUseMG, color='Groundwater'), size= 1)+
             scale_colour_discrete(name="Water Type")
           
         }
         if(input$WaterType == "SurfaceWater"){ 
           theGraph <- theGraph +
             geom_line(data= graphSWdata, aes(graphSWdata$Year,graphSWdata$monthlyWaterUseMG, color='Surface Water'), size= 1)+
             scale_colour_discrete(name="Water Type")
           
         }
         if (input$WaterType == "Reuse"){ 
           theGraph <- theGraph+
             geom_line(data= graphRWdata, aes(graphRWdata$Year,graphRWdata$monthlyWaterUseMG, color='Reuse Water'), size= 1)+
             scale_colour_discrete(name="Water Type")
         }
         if(input$WaterType == ("All" )){
           theGraph <- theGraph+
             geom_line(data= graphGWdata, aes(graphGWdata$Year,graphGWdata$monthlyWaterUseMG, color='Groundwater'), size= 1)+
             geom_line(data= graphSWdata, aes(graphSWdata$Year,graphSWdata$monthlyWaterUseMG, color='Surface Water'), size= 1)+
             geom_line(data= graphRWdata, aes(graphRWdata$Year,graphRWdata$monthlyWaterUseMG, color='Reuse Water'), size= 1)+
             scale_colour_discrete(name="Water Type")
         }
         #This next section is added if user checks the box for USGS data
         if(input$WaterType == ("All") & input$USGS){ 
           theGraph <- theGraph+
             geom_line(data= graphGWdata, aes(graphGWdata$Year,graphGWdata$monthlyWaterUseMG, color='Groundwater'), size= 1)+
             geom_line(data= graphSWdata, aes(graphSWdata$Year,graphSWdata$monthlyWaterUseMG, color='Surface Water'), size= 1)+
             geom_line(data= graphRWdata, aes(graphRWdata$Year,graphRWdata$monthlyWaterUseMG, color='Reuse Water'), size= 1)+
             geom_line(linetype=2, data= usgs, aes(usgs$Year, usgs$Texas, color="USGS"),size= 1)+
             geom_point( data= usgs, aes(usgs$Year, usgs$Texas, color="USGS"),size= 2.5)+
              scale_colour_discrete(name="Water Type")
         }
         
         
         
         print(theGraph)
         
       })
     })
     observe(if (input$state == "NorthCarolina"){ 
       
       #update radio buttons to reflect NC data
       updateRadioButtons(session,  "WaterType",
                          choices = list("All"= "All",
                                         "Surface Water" = "SurfaceWater", 
                                         "Groundwater" = "Groundwater",
                                         "Mixed"= "Mixed"))
       output$HistoricalTrends <- renderPlot({
         
         #Get the data ready to be plotted
         graphdata<-passData() %>%
           dplyr::group_by(Year) %>%
           dplyr::summarise(monthlyWaterUseMG = sum(monthlyWaterUseMG, na.rm = TRUE ))
         #Source- surface water
         graphswdata<-passData()[passData()$NewWaterType == "Surface Water",]%>%
           dplyr::group_by(Year) %>%
           dplyr::summarise(monthlyWaterUseMG = sum(monthlyWaterUseMG, na.rm = TRUE ))
         #Source- groundwater)
         graphgwdata<-passData()[passData()$NewWaterType == "Groundwater",]%>%
           dplyr::group_by(Year) %>%
           dplyr::summarise(monthlyWaterUseMG = sum(monthlyWaterUseMG, na.rm = TRUE ))
         #Source- groundwater)
         graphmixdata<-passData()[passData()$NewWaterType == "Mix",]%>%
           dplyr::group_by(Year) %>%
           dplyr::summarise(monthlyWaterUseMG = sum(monthlyWaterUseMG, na.rm = TRUE ))
         

         #Graph of total water usage
         theGraph <-  ggplot(data= graphdata,aes(graphdata$Year,graphdata$monthlyWaterUseMG)) + 
           geom_line(aes(color="Total Water"), size= 1) +
           labs(y="\n Industrial Water Use (MG)\n", x="\n Year \n",
                title = "\nNorth Carolina Industrial Water Use by Water Type\n") +
           scale_y_continuous(labels = scales::comma)+#, limits= c(0,60000), 
                            #  breaks = c(0,10000,20000,30000,40000,50000,60000, 70000))+
           scale_x_continuous()+
           theme(axis.text =element_text(size=16), axis.title=element_text(size=20, face= "bold"), 
                 legend.title = element_text(size = 18, face = "bold"), 
                 legend.text = element_text(size = 16),
                 plot.title = element_text(size = 22, face = "bold"))#,
                 #legend.position = c(0.91,0.8))

        
         if(input$WaterType == "SurfaceWater"){
           theGraph <- theGraph +
             geom_line(data= graphswdata, aes(graphswdata$Year,graphswdata$monthlyWaterUseMG, color='Surface Water'),size= 1)+
             scale_colour_discrete(name="Water Type")
         }
         if(input$WaterType == "Groundwater"){
           theGraph <- theGraph +
             geom_line(data= graphgwdata, aes(graphgwdata$Year,graphgwdata$monthlyWaterUseMG, color='Groundwater'), size= 1)+
             scale_colour_discrete(name="Water Type")
         }
         if(input$WaterType == "Mixed"){
           theGraph <- theGraph +
             geom_line(data= graphmixdata, aes(graphmixdata$Year,graphmixdata$monthlyWaterUseMG, color='Mixed'), size= 1)+
             scale_colour_discrete(name="Water Type")
         }
         if(input$WaterType == ("All" )){ 
           theGraph <- theGraph+
             geom_line(data= graphswdata, aes(graphswdata$Year,graphswdata$monthlyWaterUseMG, color='Surface Water'), size= 1) +
             geom_line(data= graphgwdata, aes(graphgwdata$Year,graphgwdata$monthlyWaterUseMG, color='Groundwater'), size= 1)+
             geom_line(data= graphmixdata, aes(graphmixdata$Year,graphmixdata$monthlyWaterUseMG, color='Mixed'), size= 1)+
           scale_colour_discrete(name= "Water Type")
          
         }
 
         print(theGraph)
       })
     }) 
     

#2f projections tab
    observe(if (input$state == "Indiana"){
      output$Projections <- renderPlot({ 
        
        #load Data
        forc <- read_csv("ForecastIN.csv", 
                         col_types = cols(Date = col_date(format = "%m/%d/%Y")))
        
        #Find make find total water consumption each month in IN
         MonthlyIN <- IN %>%
          dplyr::group_by(DateProper) %>%
          dplyr::summarise("TotalWaterUseMG" = sum(monthlyWaterUseMG))

        #plot
        INproj<-ggplot(MonthlyIN, aes(MonthlyIN$DateProper, MonthlyIN$TotalWaterUseMG)) + 
          geom_line(color="navyblue", size= 1, aes(color= "Original Data")) +
          labs(x= "\nYear\n", y="\n Industrial Water Use (MG)\n", 
               title = "\n Industrial Water Use Prediction\n",
               caption = "Seasonal ARIMA(1,1,1,0,1,1,12) model was used to \n project industrial water use for the next 5 years.\n Error, shown in grey, indicates 95% CI") +
          scale_y_continuous(labels = scales::comma, breaks = seq(0,100000, by =10000))+
          #scale_x_date(breaks= c(1985,2030, by= 5))+
          geom_ribbon(data=forc, fill='grey', aes(forc$Date, forc$ErrorUp, ymin= forc$Projection,
                                                  ymax = forc$ErrorUp))+
          geom_ribbon(data=forc, fill ='grey', aes(forc$Date, forc$ErrorDown, 
                                                   ymax= forc$Projection,ymin = forc$ErrorDown))+
          geom_line(linetype =2,data=forc,color='navy', aes(forc$Date, forc$Projection), size= 1)+
          theme(axis.text =element_text(size=16), axis.title=element_text(size=20, face= "bold"), 
                legend.title = element_text(size = 18, face = "bold"), 
                legend.text = element_text(size = 16),
                plot.title = element_text(size = 22, face = "bold"),
                plot.caption= element_text(size=14))
        print(INproj)
        
      })
    })
    observe(if (input$state== "Texas"){
      output$Projections <- renderPlot({ 
        #load data
        forc <- read_csv("Forecasttx.csv")
                        
        
        #Find make find total water consumption each year in TX
        Yearlytx <- TXIndusall %>%
          dplyr::group_by(Year) %>%
          dplyr::summarise("TotalWaterUseMG" = sum(monthlyWaterUseMG))
        
        #subset the years to match projection script
        Yearlytx <- subset(Yearlytx, Yearlytx$Year > 1975)
        

      #plot 
      proj<-  ggplot(Yearlytx, aes(Yearlytx$Year, Yearlytx$TotalWaterUseMG)) + 
          geom_line(color="navyblue", size= 1) +
          labs(x ="\nYear\n", y="\n Industrial Water Use (MG)\n", 
               title = "\n Industrial Water Use Prediction\n",
               caption = "ARIMA(0,1,2) model was used to \n project industrial water use for the next 10 years.\n Error, shown in grey, indicates 95% CI") +
        
          scale_y_continuous(labels = scales::comma,breaks = seq(0,1800000, by =200000))+
          scale_x_discrete()+
          geom_ribbon(data=forc, fill='grey', aes(forc$Date, forc$ErrorUp, ymin= forc$Projection,
                                                  ymax = forc$ErrorUp))+
          geom_ribbon(data=forc, fill ='grey', aes(forc$Date, forc$ErrorDown, 
                                                   ymax= forc$Projection,ymin = forc$ErrorDown))+
          geom_line(linetype =2,data=forc,color='navy', aes(forc$Date, forc$Projection), size= 1)+
          
        theme(axis.text =element_text(size=16), axis.title=element_text(size=20, face= "bold"), 
              legend.title = element_text(size = 18, face = "bold"), 
              legend.text = element_text(size = 16),
              plot.title = element_text(size = 22, face = "bold"),
              plot.caption= element_text(size=15))
      print(proj)
    })
    })  
    observe(if (input$state== "NorthCarolina"){
      output$Projections <- renderPlot({ 
    #load Data
    forc <- read_csv("ForecastNC.csv", 
                     col_types = cols(Date = col_date(format = "%m/%d/%Y")))
    
    
    #Find make find total water consumption each Month in NC
    MonthlyNC <- NC %>%
      dplyr::group_by(Date) %>%
      dplyr::summarise("TotalWaterUseMG" = sum(monthlyWaterUseMG))
    
   
    #plot
    proj<-ggplot(MonthlyNC, aes(MonthlyNC$Date, MonthlyNC$TotalWaterUseMG)) + 
      geom_line(color="navyblue", size= 1) +
      labs(x ="\nYear\n", y="\n Industrial Water Use (MG)\n", 
           title = "\n Industrial Water Use Prediction\n",
           caption = "Seasonal ARIMA(1,1,1,0,1,1,12) model was used to \n project industrial water use for the next 5 years.\n Error, shown in grey, indicates 95% CI") +
      scale_y_continuous(labels = comma, breaks = seq(0,5500, by =500))+
      scale_x_date()+
      # geom_ribbon(ymin=0, ymax=graphdata$monthlyWaterUseMG, fill = 'lightgreen', alpha=0.3)+
      geom_ribbon(data=forc, fill='grey', aes(forc$Date, forc$ErrorUp, ymin= forc$WaterUse,
                                              ymax = forc$ErrorUp))+
      geom_ribbon(data=forc, fill ='grey', aes(forc$Date, forc$ErrorDown, 
                                               ymax= forc$WaterUse,ymin = forc$ErrorDown))+
      geom_line(linetype =2,data=forc,color='navy', aes(forc$Date, forc$WaterUse), size= 1)+
      
      theme(axis.text =element_text(size=16), axis.title=element_text(size=20, face= "bold"), 
            legend.title = element_text(size = 18, face = "bold"), 
            legend.text = element_text(size = 16),
            plot.title = element_text(size = 22, face = "bold"),
            plot.caption= element_text(size=15))
     
    print(proj)
      })
    })
    
    
#end script
})


    
    