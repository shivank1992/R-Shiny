
#Author : Shivank Garg


#Load packages
#library(rsconnect)
library(shiny)
library(leaflet)
library(dplyr)
library(tidyverse)
library(tidyr)
library(readr)
library(data.table)
library(plotly)
library(ggthemes)
library(RColorBrewer)
library(shinycssloaders)



# READ THE DATA FILE
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
crimes.df <- fread("Crimes_-_2018.csv", stringsAsFactors = F)



#glimpse(crimes.df)

# FORMATTING THE columns
crimes.df$PosixDate <- as.POSIXct(strptime(crimes.df$Date, format="%m/%d/%Y %I:%M:%S %p"))
crimes.df$month <- gsub("0(\\d)", "\\1", format(as.Date(crimes.df$PosixDate), "%m"))

names(crimes.df)[names(crimes.df) == 'Primary Type'] <- "Primary.Type"
names(crimes.df)[names(crimes.df) == 'Location Description'] <- "Location.Description"
names(crimes.df)[names(crimes.df) == 'Case Number'] <- "Case.Number"
crimes.df$hours <- strftime(crimes.df$PosixDate, format="%H:%M:%S")

#glimpse(crimes.df)

##Check for NA values again
sapply(crimes.df,function(x){sum(is.na(x))})


##Crime Types for tab choices
#count by crime type 
count_by_type <- crimes.df %>%
  group_by(Primary.Type) %>%
  summarise(Count=n())

#Count by arrest
count_by_arrest <- crimes.df %>%
  group_by(Arrest) %>%
  summarise(Count=n())

#Count by Domestic
count_by_domestic <- crimes.df %>%
  group_by(Domestic) %>%
  summarise(Count=n())

#Count by Premises
count_by_premises <- crimes.df %>%
  group_by(Location.Description) %>%
  summarise(Count=n())

#Create choices for inputs
crimetype_choices<- unique(count_by_type$Primary.Type)
arrestchoices <- unique(count_by_arrest$Arrest)
domesticchoices <- unique(count_by_domestic$Domestic)
premiseschoices <- unique(count_by_premises$Location.Description)


#Remove NA observations
##Remove observations with NA Latitude and Longitude
crimes.df<-  crimes.df[complete.cases(crimes.df$Latitude), ]
##Remove observations that have NA values for time when TimeZone changes in Chicago(10 Mar)
crimes.df<-  crimes.df[complete.cases(crimes.df$PosixDate), ]


##Check for NA values again
sapply(crimes.df,function(x){sum(is.na(x))})

##We are left with 3 NA values for Wards and 2 NA values for Community area
#NOTE : But we won't remove them because we won't use these columns

##Select only the values we want
crimes.df<- crimes.df %>%
  select(PosixDate,month, ID, Case.Number, Date, Primary.Type, Latitude, Longitude, Location.Description,Arrest, Domestic)


####################  START SHINY Programming ##############################################################
# UI COMPONENT

ui = shinyUI(navbarPage(tags$b("Shiny App Title : Crimes that occurred in the City of Chicago in the year 2018 | Author : Shivank Garg"),
                        

                        tabPanel("Tab 1 : Location of crimes by date on a map",
                                 titlePanel("Location of crimes by date on a map"),
                                 sidebarPanel(
                                   dateRangeInput(inputId = "daterange", label = "Select date range to filter crimes by date",
                                                  start = as.Date('2018-01-01'), end = as.Date('2018-01-07')),
                                 checkboxGroupInput(inputId="arresttype", label="Checkbox : View Arrested Cases",
                                                    choices=arrestchoices, selected=arrestchoices),
                                 checkboxGroupInput(inputId="domestictype", label="Checkbox: View Domestic Cases",
                                                    choices=domesticchoices, selected=arrestchoices),
                                 width = 3),
                                 
                                 mainPanel(leafletOutput(outputId = "tab2map",height = 600,width = "auto")%>% withSpinner(color="#0dc5c1"),
                                           br(),
                                           DT::dataTableOutput(outputId = "tab2table"),
                                           width = 9)
                                 
                        ),
                        tabPanel("Tab 2: Frequency of crime by month and crime type",
                                 titlePanel("Frequency of crime by month and crime type"),
                                 sidebarPanel(
                                   selectInput(inputId = "crime_type", label = "Select Crime Type",
                                               choices = crimetype_choices,
                                               selected = "THEFT"),
                                   sliderInput(inputId = "month", label = "Select Month", min=1, max=12, step = 1,
                                               sep= , value = c(1,12)),
                                   width = 3),
                                 
                                 mainPanel(plotlyOutput(outputId ='crimetypesmonth', height = "auto", width = "auto")%>% withSpinner(color="#0dc5c1"),
                                           br(),
                                           plotOutput(outputId ='allbarchart', height = "600",width = "100%"),
                                           br(),br(),
                                           DT::dataTableOutput(outputId = "tab1table"),
                                           width = 9)
                                 
                        ),
                        tabPanel("Tab 3: Heatmap of the type of crime vs hour of the day",
                                 titlePanel("Heatmap of type of crime vs the hour of the day"),
                                 sidebarPanel(
                                   selectInput(inputId = "crime_type_tab3", label = "Select Crime Type",
                                               choices = crimetype_choices,
                                               selected = "ROBBERY"),
                                   width = 3),
                                 
                                 mainPanel(plotlyOutput(outputId ='heatmap', height = "auto", width = "auto",inline = T)%>% withSpinner(color="#0dc5c1"),
                                           br(),br(),
                                           plotlyOutput(outputId ='typeheatmap', height = "auto", width = "auto"),
                                           br(),br(),
                                           plotlyOutput(outputId = "typelinechart", height = "auto", width = "auto"),
                                           br(),br(),
                                           plotOutput(outputId ='linechart', height = 600, width = "100%"),
                                           br(),br(),
                                           DT::dataTableOutput(outputId = "tab3table"),
                                           width = 9)
                        )
))


# SERVER COMPONENT

server <- function(input,output, session){
  
  ###########################################################################################
  #Tab-2
  #Draw barchart for crime types by month 

  #Reactive data filtering
  filtered_data <- reactive({
    reactbargraph <-  crimes.df[crimes.df$Primary.Type == input$crime_type,] %>%
      mutate(month = as.integer(as.character(month)))%>%
      group_by(month) %>%
      summarise(frequency = n()) %>%
      filter(month >= input$month[1] & month <= input$month[2])
  })
  
  
  #Make barchart
  output$crimetypesmonth <- renderPlotly({
          plot_ly(data=filtered_data(), x = ~month, y = ~frequency, type = 'bar', width =0.1,
                     marker = list(color = 'rgb(31, 119, 180)',
                                   line = list(color = 'transparent'))) %>%
          layout(title = paste("Barplot : Crime Type |",input$crime_type,"| Frequency by Month"),
                 xaxis = list(title = "Month"),
                 yaxis = list(title = "Frequency"),
                 plot_bgcolor = "#FFFFFF",
                 paper_bgcolor='#FFFFFF',
                 bargap = 0.7)
      })
  
  ##Line chart for all crime types vs all months
  output$allbarchart <- renderPlot({
    bargraph <-  crimes.df %>%
      mutate(month = as.integer(as.character(month)))%>%
      group_by(Primary.Type, month) %>%
      summarise(frequency = n())
    
    ggplot(data = bargraph,
           aes(x = month, y = frequency)) +
      geom_bar(stat = "identity") +
      facet_wrap(~Primary.Type, scales = "free") +
      labs(title="Barchart of all Crime Types vs Month")+
      scale_y_continuous(name = "Frequency", labels = scales::comma) +
      scale_x_continuous(name = "Month", labels = scales::comma,limits= c(1, 12))+
      theme(strip.text = element_text(size = 7),plot.title=element_text(size=20,face="bold"))
  })
  
  # Create Data Table
  
  output$tab1table <- DT::renderDataTable({
    DT::datatable(data = filtered_data(),
                  rownames = FALSE,
                  options = list(pageLength = 12),
                  class = 'cell-border stripe',
                  colnames = c('Month', 'Frequency of Crime'),
                  caption = paste("Table Title : Frequency of crime by month and crime type : ",input$crime_type))
  })
  
  
  ###########################################################################################

  ######Tab1 : Location of crimes by date on a map
  
  #Reactive data filtering according to the date
  datefiltered_data <- reactive({
    out <- crimes.df %>%
      filter(PosixDate >= input$daterange[1] & PosixDate <= input$daterange[2])%>%
      select(ID, Case.Number, Date, Primary.Type, Latitude, Longitude, Location.Description,Arrest, Domestic) %>%
      filter(Arrest %in% input$arresttype &
               Domestic %in% input$domestictype)

  })
  
  #Create Map for Tab2
  output$tab2map <- renderLeaflet({
    df <- datefiltered_data()
    m <- leaflet(data = df ) %>%
      addProviderTiles(providers$Esri.WorldStreetMap) %>%
      setView(-87.6105, 41.8947,zoom=10) %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addCircleMarkers(clusterOptions=markerClusterOptions(), 
                       lng=~Longitude, lat=~Latitude,radius=5, group='Cluster',
                       popup=~paste('<b><font color="Black">','Crime Information','</font></b><br/>',
                                    'Case No:', Case.Number,'<br/>',
                                    'Crime ID:', ID,'<br/>',
                                    'Crime Type:', Primary.Type,'<br/>',
                                    'Date & Time:', Date,'<br/>',
                                    'Arrest:', Arrest, '<br/>',
                                    'Location:', Location.Description,'<br/>'))
    m
    
  })
  
  # Create Data Table for Tab2
  
  output$tab2table <- DT::renderDataTable({
    DT::datatable(data = datefiltered_data(),
                  rownames = FALSE,
                  colnames = c('Crime ID','Case Number','Date and Time', 'Crime Type', 'Latitude','Longitude','Type of Location','Arrest','Domestic'),
                  class = 'cell-border stripe',
                  caption = paste("Table Title : Details of crimes between dates ",input$daterange[1]," and ",input$daterange[2]))
  })
  
  ###########################################################################################
  ##TAB-3
  #Reactive data filtering for linechart for particular crime
  hourly_type_filtered_data <- reactive({
    crime_typeheatmap <-
      crimes.df %>%
      mutate_if(is.factor, as.character) %>% 
      mutate(Hour = as.integer(as.character(strftime(crimes.df$PosixDate, format="%H")))) %>% 
      group_by(Primary.Type, Hour) %>% 
      summarize(reported_incidents = n()) %>% 
      group_by(Primary.Type) %>% 
      mutate(total_hours = n()) %>% 
      filter(Primary.Type == input$crime_type_tab3)%>%
      select(Primary.Type, Hour, reported_incidents)%>%
      mutate(Hour = factor(format(strptime(Hour, format='%H'), '%I%p'), levels = format(strptime(0:23, format='%H'), '%I%p')))

  })
  
  ##Heatmap1 for all crimes vs hour
  
  output$heatmap <- renderPlotly({
    allcrime_heatmap <-
      crimes.df %>%
      mutate_if(is.factor, as.character) %>% 
      mutate(Hour = as.integer(as.character(strftime(crimes.df$PosixDate, format="%H")))) %>% 
      group_by(Primary.Type, Hour) %>% 
      summarize(reported_incidents = n()) %>% 
      group_by(Primary.Type) %>% 
      mutate(total_hours = n()) %>% 
      select(Primary.Type, Hour, reported_incidents,total_hours) %>%
      mutate(reported_incidents = reported_incidents/max(reported_incidents)) %>%
      mutate(Hour = factor(format(strptime(Hour, format='%H'), '%I%p'), levels = format(strptime(0:23, format='%H'), '%I%p'))) %>%
      arrange(desc(total_hours),Hour,reported_incidents)
    
    hm<- plot_ly(x=allcrime_heatmap$Hour, y=allcrime_heatmap$Primary.Type, z = allcrime_heatmap$reported_incidents, type = "heatmap",
                 colorbar = list(title = "Frequency", tickmode='array', tickvals=c(0.1,1),ticktext=c("Min","Max"))) %>%
      layout(title = paste("Heatmap : Chicago Crimes vs Hour the the day"),
             xaxis = list(title = "Hour of the Day"),
             yaxis = list(title = "Type of Crime",showticklabels=T,autorange=T,tick0=0,dtick=1,categoryorder = "trace"))
    hm
  })
  
  ##Heatmap2 for selected crime type vs hour

  output$typeheatmap <- renderPlotly({
    heatmap_data <- hourly_type_filtered_data()
    hm<- plot_ly(x=heatmap_data$Hour, y=heatmap_data$Primary.Type, z = heatmap_data$reported_incidents, type = "heatmap",
                 colorbar = list(title = "Frequency")) %>%
      layout(title = paste("Heatmap : Crime(",input$crime_type_tab3,") vs Hour the the day"),
             xaxis = list(title = "Hour of the Day"),
             yaxis = list(title = "Type of Crime"))
    hm
  })
  
  
  ##Line chart for selected crime type vs hour
  output$typelinechart <- renderPlotly({
    typeline_chart_data <-
        crimes.df %>% 
        mutate_if(is.factor, as.character) %>% 
        mutate(Hour = as.integer(as.character(strftime(crimes.df$PosixDate, format="%H")))) %>% 
        group_by(Primary.Type, Hour) %>% 
        summarize(reported_incidents = n()) %>% 
        group_by(Primary.Type) %>% 
        mutate(total_hours = n()) %>% 
        filter(Primary.Type == input$crime_type_tab3)%>%
        select(Primary.Type, Hour, reported_incidents)
      print(
      ggplotly(
        ggplot(data = typeline_chart_data, 
               aes(x = Hour, y = reported_incidents)) + 
        geom_line(size = 1.5, color = "red") + 
        labs(title = paste("Linechart : Crime(",input$crime_type_tab3,") vs Hour the the day"), y="Incidents", x="Hour of the day(0 means 12AM)")+
        theme_tufte()+
        theme(strip.text = element_text(size = 7))
      ))
  })
  
  ##Line chart for crime types vs hour
  output$linechart <- renderPlot({
    crime_hourly <-
      crimes.df %>%
      mutate_if(is.factor, as.character) %>%
      mutate(Hour = as.integer(as.character(strftime(crimes.df$PosixDate, format="%H")))) %>%
      group_by(Primary.Type, Hour) %>%
      summarize(reported_incidents = n()) %>%
      group_by(Primary.Type) %>%
      mutate(total_hours = n()) %>%
      filter(total_hours > 15)

    sm_plot <-
      ggplot(data = crime_hourly,
             aes(x = Hour, y = reported_incidents)) +
      geom_line(size = 1.5, color = "darkred") +
      facet_wrap(~Primary.Type, scales = "free") +
      labs(title="Linechart of all Crime Types vs Hour of the day")+
      scale_y_continuous(name = "Frequency of Incidents", labels = scales::comma) +
      scale_x_continuous(name = "Hour of the day(0=12AM)", labels = scales::comma,limits= c(0, 23))+
      theme(strip.text = element_text(size = 7),plot.title=element_text(size=20,face="bold"))

      sm_plot
  })
  
  #Table for Tab3
  output$tab3table <- DT::renderDataTable({
    DT::datatable(data = hourly_type_filtered_data(),
                  rownames = FALSE,
                  options = list(pageLength = 24),
                  class = 'cell-border stripe',
                  colnames = c('Crime Type', 'Hour of the Day', 'Total no of reported Incidents'),
                  caption = paste("Table Title : Hourly No of incidents of ",input$crime_type_tab3))
  })
  
  
}
# CREATE A SHINY APP OBJECT

shinyApp(ui = ui, server = server)
