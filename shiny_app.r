###GROUP 14: Willem Himpe - Willem Lannoye - Axelle Sels - Thibault Verduyn - Camille Wittouck

library(shinydashboard)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(concaveman)
library(sf)
library(geojsonio)
library(sp)
library(rgeos)
library(GISTools)
library(openintro)
library(spatstat)  
library(stats)
library(fpp)
library(dplyr)
library(forecast)
library(prioritizr)
library(Metrics)
set.seed(1000)


#setworking directory
setwd("C:/Users/Willem Himpe/Desktop/final")

# all data is in the 'data/' file
#CREATION SUBSET DATASET

uscities <- read.csv("data/uscities.csv")
DF <- read.csv("data/futureAccidentDATA.csv")
DFsub<-DF[,c(6,7,15,17,ncol(DF))]
Crashcities <- merge(DFsub, uscities[,c('city', 'state_id', 'population', 'county_name')], by.x = c('City', 'State'), by.y= c('city', 'state_id'))
random_indices <- sample(1:nrow(Crashcities), 10000, replace= FALSE)
DF_sample <- Crashcities[random_indices,]
DF_sample <- na.omit(DF_sample)


random_indices2 <- sample(1:nrow(DF), 100000, replace= FALSE)
DFsub2 <- DF[random_indices2,]
Crashcities2 <- merge(DFsub2, uscities[,c('city', 'state_id', 'population', 'county_name')], by.x = c('City', 'State'), by.y= c('city', 'state_id'))
Crashcities2$time <- strptime(x = as.character(Crashcities2$time),format = "%Y-%m-%d %H:%M:%S")
years <- unique(Crashcities2$time$year)
years <- years + 1900


#import polgons
states_loc <-read.csv("data/states_locations.csv",header = TRUE)
states <- geojsonio::geojson_read(x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", what = "sp")
counties <- geojsonio::geojson_read(x ="https://raw.githubusercontent.com/python-visualization/folium/master/tests/us-counties.json", what = "sp" )
## Add the states for the county
coords_centroid_of_county <- getSpPPolygonsLabptSlots(counties)
points_of_centroids <-  SpatialPoints(coords_centroid_of_county)

points_of_centroids_city <-  SpatialPoints(cbind(uscities$lng,uscities$lat))
points_of_centroids_city <-  SpatialPoints(cities@coords)


#match the counties + cities with the states
list_of_state_to_corresponding_county <- matrix()
for(i in 1:length(points_of_centroids)){
    
    bool_vector <- gContains(states, points_of_centroids[i], byid = TRUE)
    
    for(nr_boolean in 1:length(bool_vector)){
        
        if(bool_vector[nr_boolean]){
            list_of_state_to_corresponding_county[i] <- states@data$name[nr_boolean]
        }
        
    }
    
}
list_of_state_to_corresponding_city <- matrix()
for(i in 1:length(points_of_centroids_city)){
    
    bool_vector <- gContains(states, points_of_centroids_city[i], byid = TRUE)
    
    for(nr_boolean in 1:length(bool_vector)){
        
        if(bool_vector[nr_boolean]){
            list_of_state_to_corresponding_city[i] <- states@data$name[nr_boolean]
        }
        
    }
    
}


counties@data <- cbind(counties@data,list_of_state_to_corresponding_county)
counties@data$list_of_state_to_corresponding_county <- as.character(counties@data$list_of_state_to_corresponding_county)

cities@data <- cbind(cities@data,list_of_state_to_corresponding_city)
cities@data$list_of_state_to_corresponding_city <- as.character(cities@data$list_of_state_to_corresponding_city)

#preperation description plots
points <- SpatialPointsDataFrame(coords = DF_sample[,c(3,4)], data = DF_sample, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
count_of_crashes <- poly.counts(points ,counties)  #crashes per county
counties@data <- cbind(counties@data,count_of_crashes)

DF_sample$time <- strptime(x = as.character(DF_sample$time),format = "%Y-%m-%d %H:%M:%S")
years <- unique(DF_sample$time$year)
years <- years + 1900
require(tidyverse)

#creation timeseries
make_time_series_df_per_geographic_area <- function(df = Crashcities2,time_interval = "month"){
    
    month <- sort(unique(format(as.Date(df$time), "%Y-%m")))
    crashpertime <- data.frame(unique(df[,c('City','county_name', 'State','population')]))
    
    for(i in month){
        data_subset <- df[which(format(as.Date(df$time), "%Y-%m") == i) ,]
        
        count_of_crashes_time_interval <- data.frame(data_subset %>% count(City, county_name, State))
        
        crashpertime <- merge(crashpertime, count_of_crashes_time_interval, by=c("City", "county_name", "State"), all.x=TRUE) 
        
    }
    colnames(crashpertime)[5:ncol(crashpertime)] <- month
    return(crashpertime)
}
make_time_series_df_per_geographic_area2 <- function(df = Crashcities2,time_interval = "month"){
  
  year <- sort(unique(format(as.Date(df$time), "%Y-%m")))
  crashpertime <- data.frame(unique(df[,c('county_name', 'State','population')]))
  
  for(i in month){
    data_subset <- df[which(format(as.Date(df$time), "%Y-%m") == i) ,]    
    count_of_crashes_time_interval <- data.frame(data_subset %>% count( county_name, State))
    
    crashpertime <- merge(crashpertime, count_of_crashes_time_interval, by=c("county_name", "State"), all.x=TRUE) 
    
  }
  colnames(crashpertime)[5:ncol(crashpertime)] <- month
  return(crashpertime)
}
opl_yr_counties = make_time_series_df_per_geographic_area2()

  dframe = opl_yr_counties
  copy_counties = duplicate(counties)
  copy_counties@data = cbind(copy_counties@data,matrix(0,nrow = 3108,ncol=2))
  for(i2 in 1:nrow(dframe)){
    index_r1 = which(opl_yr_counties$State==as.character(dframe$State[i2]))
    index_r2 = which(copy_counties@data$name==as.character(dframe$County[i2]))
    val = intersect(index_r1,index_r2)
    if(length(val)!=0){
      copy_counties@data[val,6:257] = dframe[i2,50:ncol(dframe)]
    }
  }
  opl_yr_counties = dframe
  



opl_cities_month = make_time_series_df_per_geographic_area(DF_sample,"month")
opl_cities_month[is.na(opl_cities_month)] <- 0
colnames(opl_cities_month)[2]<-'County'
opl_counties_month = aggregate(opl_cities_month[,5:ncol(opl_cities_month)], by=list(opl_cities_month$County,opl_cities_month$State), sum)
colnames(opl_counties_month)[1]<-'County'
colnames(opl_counties_month)[2]<-'State'
opl_states_month = aggregate(opl_cities_month[,5:ncol(opl_cities_month)], by=list(opl_cities_month$State), sum)
colnames(opl_states_month)[1]<-'State'




##############################################################
###### LOADING PREDICITION DATA + CONVERTING TO POLYGON ######
##############################################################

#mean
load("data/mean_pred_mt_cities.RData")
load("data/mean_pred_mt_counties.RData")
load("data/mean_pred_mt_states.RData")
#mov_avg
load("data/mov_avg_mt_cities.RData")
load("data/mov_avg_mt_counties.RData")
load("data/mov_avg_mt_states.RData")
#exp_smooth
load("data/exp_smoothing_mt_cities.RData")
load("data/exp_smoothing_mt_counties.RData")
load("data/exp_smoothing_mt_states.RData")
#holt
load("data/holt_mt_cities.RData")
load("data/holt_mt_counties.RData")
load("data/holt_mt_states.RData")
#holt_winter
load("data/holtwinter_mt_cities.RData")
load("data/holtwinter_mt_counties.RData")
load("data/holtwinter_mt_states.RData")
#arima
load("data/pred_arima_mt_cities.RData")
load("data/pred_arima_mt_counties.RData")
load("data/pred_arima_mt_states.RData")
#ensemble
load("data/pred_cities_month.RData")
load("data/pred_counties_month.RData")
load("data/pred_states_month.RData")


#read in data for tradeoffs
costs <- read.csv("data/costs4.csv")
costs[,2] <- round(costs[,2]/1e9,2)
designmatrix <- read.csv("data/designmatrix4.csv")
initials <-read.csv("data/initials.csv")

designmatrix2 <- read.csv("data/designmatrix_counties_reference.csv")
colnames(designmatrix2)[2] <-"county_name"
costs2 <- read.csv("data/costs_counties.csv")
costs2[,2] <- round(costs2[,2]/1e9,2)

ts_city_pred =  read.csv("data/timeseriesmatrix.csv")



mov_avg_mt_states[,52:ncol(mov_avg_mt_states)] = round(mov_avg_mt_states[,52:ncol(mov_avg_mt_states)])
mov_avg_mt_counties[,52:ncol(mov_avg_mt_counties)] = round(mov_avg_mt_counties[,52:ncol(mov_avg_mt_counties)])
mov_avg_mt_cities[,52:ncol(mov_avg_mt_cities)] = round(mov_avg_mt_cities[,52:ncol(mov_avg_mt_cities)])
full_pred_states_month =list(mean_pred_mt_states,mov_avg_mt_states,exp_smoothing_mt_states,holt_mt_states, holtwinter_mt_states,pred_arima_mt_states,pred_states_month)                 #pred_arima_mt_states
full_pred_counties_month=list(mean_pred_mt_counties,mov_avg_mt_counties,exp_smoothing_mt_counties,holt_mt_counties, holtwinter_mt_counties,pred_arima_mt_counties,pred_counties_month)  #pred_arima_mt_counties
full_pred_cities_month=list(mean_pred_mt_cities,mov_avg_mt_cities,exp_smoothing_mt_cities,holt_mt_cities, holtwinter_mt_cities,pred_arima_mt_cities,pred_cities_month)                  #pred_arima_mt_cities



#create polygons state
library(rlang)

states_loc = read.csv("data/states_locations.csv")
states_loc$ï..name <- as.character(states_loc$ï..name)
states@data$code <- states_loc$code

#create list of polygons
poly_states =list()
poly_counties = list()


#poly_states creation
for(i1 in 1:length(full_pred_states_month)){
  dframe = full_pred_states_month[[i1]]
  copy_states = duplicate(states)
  copy_states@data = cbind(states@data,matrix(0,nrow = 52,ncol=252))
  for(i2 in 1:nrow(dframe)){
    index = match(as.character(dframe$State[i2]),copy_states@data$code)
    if(!is.na(index)){
      copy_states@data[index,5:257] = dframe[i2,49:ncol(dframe)]
    }
  }
  poly_states = append(poly_states,copy_states)
  
}

#poly_counties creation
counties@data = cbind(counties@data,"code"=matrix(0,nrow(counties@data),ncol=1))
for(i1 in 1:nrow(states_loc)){    #matching the code together with the states
  stat_i=states_loc$ï..name[i1]
  code_i=as.character(states_loc$code[i1])
  indexen =which((counties@data$list_of_state_to_corresponding_county==stat_i)==TRUE)
  counties@data$code[indexen] <- rep(code_i,length(indexen))
}  

for(i1 in 1:length(full_pred_counties_month)){
  dframe = full_pred_counties_month[[i1]]
  copy_counties = duplicate(counties)
  copy_counties@data = cbind(copy_counties@data,matrix(0,nrow = 3108,ncol=252))
  for(i2 in 1:nrow(dframe)){
    index_r1 = which(copy_counties@data$code==as.character(dframe$State[i2]))
    index_r2 = which(copy_counties@data$name==as.character(dframe$County[i2]))
    val = intersect(index_r1,index_r2)
    if(length(val)!=0){
      copy_counties@data[val,6:257] = dframe[i2,50:ncol(dframe)]
    }
  }
  poly_counties = append(poly_counties,copy_counties)
  
}

#poly_cities creation
dframe = mean_pred_mt_cities
lat_long_cities = matrix(0,nrow =nrow(dframe) ,ncol=2)
for(i1 in 1:nrow(dframe)){
  index_r1 = which(uscities$state_id==as.character(dframe$State[i1]))
  index_r2 = vector()
  for(i2 in index_r1){
    if(uscities$county_name[i2]==as.character(dframe$County[i1]))
      index_r2 = append(index_r2,i2)
  }
  index_r3=vector()
  for(i3 in index_r2){
    if(uscities$city[i3]==as.character(dframe$City[i1]))
      index_r3 = append(index_r3,i3)      
  }
  if(length(index_r3)!=0)
    lat_long_cities[i1,] = c(uscities$lng[index_r3],uscities$lat[index_r3])
}
for(i in 1:7){
  
  full_pred_cities_month[[i]]$long = lat_long_cities[,1]
  full_pred_cities_month[[i]]$lat = lat_long_cities[,2]
  
}


#match counties met designmatrix
design_poly = poly_counties[[1]]
design_poly@data = design_poly@data[,1:4]
design_poly@data = cbind(design_poly@data,matrix(0,3108,ncol=18)) 
for(i1 in 1:nrow(designmatrix2)){
  index_r1 = which(design_poly@data$code==as.character(designmatrix2$State[i1]))
  index_r2 = which(design_poly@data$name==as.character(designmatrix2$county_name[i1]))
  val = intersect(index_r1,index_r2)
  if(length(val)!=0)
    design_poly@data[val,5:ncol(design_poly@data)] = designmatrix2[i1,5:ncol(designmatrix2)]
}

big_design_poly = duplicate(design_poly)
colnames(big_design_poly@data)
big_design_poly@data = cbind(big_design_poly@data,centroids_counties)
m_counties <-as.matrix(distm(centroids_counties, fun=distHaversine),labels=TRUE)
mcounties <-m_counties*0.0006213
m2counties = ifelse(mcounties<100,1,0)


for(i1 in 5:22){
  #kolom = big_design_poly@data[,i1]
  for(i2 in 1:nrow(big_design_poly@data)){
    
    if(big_design_poly@data[i2,i1]==1){
      
      for(i3 in 1:nrow(m2counties)){
        if(m2counties[i2,i3]==1 && big_design_poly@data[i3,i1]!=1 )
          big_design_poly@data[i3,i1] <- -1
      }
    }
  }
}




#read in data for shiny plots
load("data/data_for_shiny.RData")



#######################################
####### CREATION SHINY APP    #########
#######################################


# Create an empty header
header <- dashboardHeader()

# Create an empty sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(id = "sidebar",
              
              menuItem("Graph per year", tabName = "peryear", icon = icon("dashboard")),
              #menuItem("Crashes per state", icon = icon("th"), tabName = "crashesperstate"),
              menuItem("Hexagon raster", tabName = "Hexagon_raster"),
              menuItem("predictions per city", tabName = "pred_per_cities"),
              menuItem("predictions per county", tabName = "pred_per_counties"),
              menuItem("predictions per states", tabName = "pred_per_states"),
              menuItem("hospital prediction per city", tabName = "hosp_pred "),
              menuItem("hospital prediction per county", tabName = "hosp_pred_county"),
              menuItem("hospital prediction per county_radius", tabName = "hosp_pred_county_radius"),
              menuItem("hospital prediction per time - city", tabName = "hosp_pred_times_city",  badgeLabel = "new", badgeColor = "red")
              
              
              
  ))

# Create an empty body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "peryear",
            h2("Crashes per year"),
            box(width = 6, sliderInput(inputId ="slider",label = "Year", min = min(years) , max = max(years),value = years[1] ,step =1)),
            box(width= 12, leafletOutput("plot1", height = 250)),
            box(width= 12, leafletOutput("plot2", height = 250)))
    
    ,
    
    tabItem(tabName = "crashesperstate",
            h2("Crashes per state"),
            box(width= 12, leafletOutput("plot3", height = 250)),
            h2("Crashes per county"),
            box(width= 12, leafletOutput("plot4", height = 250))),
    
    
    tabItem(tabName = "Hexagon_raster",
            h2("Hexagon raster"),
            box(width= 12, leafletOutput("plot5", height = 250))),
    
    
    
    tabItem(tabName = "pred_per_cities",
            h2("City predictions per month"),
            box(width = 6, sliderInput(inputId ="slider2",label = "Month", min = 1 , max = 252, value = 1 ,step =1)),
            radioButtons("rb2", "Choose one prediction algo:",
                         choiceNames = list( "mean","moving_average","exponential_smoothing","holt_linear", "holt_winter","arima","ensemble"),
                         choiceValues = list(1,2,3,4,5,6,7)),
            
            box(width= 12,title = "CITIES", leafletOutput("plot6", height = 250))),
    #box(width= 12,title = "COUNTIES",leafletOutput("plot7_2", height = 250))) 
    
    tabItem(tabName = "pred_per_counties",
            h2("County predictions per month"),
            box(width = 6, sliderInput(inputId ="slider3",label = "Month", min = 1 , max = 252, value = 1 ,step =1)),
            radioButtons("rb3", "Choose one prediction algo:",
                         choiceNames = list( "mean","moving_average","exponential_smoothing","holt_linear", "holt_winter","arima","ensemble"),
                         choiceValues = list(1,2,3,4,5,6,7)),
            
            box(width= 12,title = "COUNTIES", leafletOutput("plot7", height = 250))),
    
    tabItem(tabName = "pred_per_states",
            h2("State predictions per month"),
            box(width = 6, sliderInput(inputId ="slider4",label = "Month", min = 1 , max = 249, value = 1 ,step =1)),
            radioButtons("rb4", "Choose one prediction algo:",
                         choiceNames = list( "mean","moving_average","exponential_smoothing","holt_linear", "holt_winter","arima","ensemble"),
                         choiceValues = list(1,2,3,4,5,6,7)),
            
            box(width= 12,title ="STATES" , leafletOutput("plot8", height = 250))),
    
    tabItem(tabName = "hosp_pred",
            h2("hospital predictions per city"),
            box( width = 3,   radioButtons("rb5", "Choose miles constraint:",
                                           choiceNames = list( "100 miles","80 miles","120 miles"),
                                           choiceValues = list(1,2,3))),
            box( width = 3,radioButtons("rb6", "Choose coverage ratio:",
                                        choiceNames = list( "98 %", "99 %"),
                                        choiceValues = list(1,2))),
            box( width = 3,radioButtons("rb7", "Choose minimum population size:",
                                        choiceNames = list( "50 000","40 000","60 000"),
                                        choiceValues = list(1,2,3))),
            box(width= 12 , leafletOutput("plot9", height = 350)),
            box(width = 6,h3(textOutput(outputId = "text1"))),
            box(width = 6,h3(textOutput(outputId = "text2")))),
    
  tabItem(tabName = "hosp_pred_county",
          h2("hospital predictions per county"),
          box( width = 3,   radioButtons("rb8", "Choose miles constraint:",
                                         choiceNames = list( "100 miles","80 miles","120 miles"),
                                         choiceValues = list(1,2,3))),
          box( width = 3,radioButtons("rb9", "Choose coverage ratio:",
                                      choiceNames = list( "98 %", "99 %"),
                                      choiceValues = list(1,2))),
          box( width = 3,radioButtons("rb10", "Choose minimum population size:",
                                      choiceNames = list("50 000", "40 000","60 000"),
                                      choiceValues = list(1,2,3))),
          box(width= 12 , leafletOutput("plot10", height = 350)),
          box(width = 6,h3(textOutput(outputId = "text3"))),
          box(width = 6,h3(textOutput(outputId = "text4")))),
  
  
  tabItem(tabName = "hosp_pred_county_radius",
          h2("hospital predictions per county_radius"),
          box( width = 3,   radioButtons("rb11", "Choose miles constraint:",
                                         choiceNames = list( "100 miles","80 miles","120 miles"),
                                         choiceValues = list(1,2,3))),
          box( width = 3,radioButtons("rb12", "Choose coverage ratio:",
                                      choiceNames = list( "98 %", "99 %"),
                                      choiceValues = list(1,2))),
          box( width = 3,radioButtons("rb13", "Choose minimum population size:",
                                      choiceNames = list("50 000", "40 000","60 000"),
                                      choiceValues = list(1,2,3))),
          box(width= 12 , leafletOutput("plot12", height = 350)),
          box(width = 6,h3(textOutput(outputId = "text9"))),
          box(width = 6,h3(textOutput(outputId = "text10")))),
  
  
  tabItem(tabName = "hosp_pred_times_city", 
          h2("hospital predictions per time city"),
          box(width = 6, sliderInput(inputId ="slider5",label = "Year", min = 1 , max = 20, value = 1 ,step =1)),
          box(width= 12 , leafletOutput("plot11", height = 350)),
          box(width = 3,h3(textOutput(outputId = "text7"))),
          box(width = 3,h3(textOutput(outputId = "text8"))),
          box(width = 3,h3(textOutput(outputId = "text5"))),
          box(width = 3,h3(textOutput(outputId = "text6"))))
  
  )
)







# Create the UI using the header, sidebar, and body
ui <- dashboardPage(header, sidebar, body)



# creation plot/leaflet and texts

server <- function(input, output, session) {
  
  
  output$plot1 <- renderLeaflet({
    print(input$slider)
    data_for_plot <- Crashcities2[which(Crashcities2$time$year == (input$slider -1900)),]
    leaflet()    %>%
      addTiles()    %>%
      setView(-93.65, 42.0285, zoom = 3)     %>% 
      addHeatmap(lng = data_for_plot$Start_Lng, lat = data_for_plot$Start_Lat , intensity = 0.02)
    
    
  })  #graph 
  output$plot2 <- renderLeaflet({
    data_for_plot2 <- Crashcities2[which(Crashcities2$time$year == (input$slider -1900)),]
    leaflet()    %>%
      addTiles()    %>%
      setView(-93.65, 42.0285, zoom = 3)     %>% 
      addCircles(lng = data_for_plot2$Start_Lng, lat = data_for_plot2$Start_Lat , radius  = 0.15)
    
    
  })
  output$plot6 <- renderLeaflet({
    
    index_pred = as.integer(input$rb2)
    city_pred = full_pred_cities_month[[index_pred]]
    
    slid_val = as.integer(input$slider2)
    predictie =  city_pred[,as.character(slid_val)]
    
    labels <- sprintf(
      "<strong>%s</strong>  <strong>%s</strong> <br/>%g predicted crashes</strong> population =%g",
      city_pred$City, city_pred$State, predictie  , city_pred$population
    ) %>% lapply(htmltools::HTML)
    
    
    kolom_pred <- predictie
    kolom_pred_radius <- kolom_pred/max(kolom_pred)
    
    bins <- c(seq(0,30,1),Inf)
    pal <- colorBin("Blues", city_pred$`251`, bins = bins)
    
    
    leaflet(city_pred)    %>%
      addTiles()    %>%
      setView(-93.65, 42.0285, zoom = 3)     %>% 
      addCircleMarkers(
        lng = city_pred$long,
        lat = city_pred$lat,
        radius = kolom_pred_radius,
        color =  ~pal(`251`), 
        
        label = labels
      )
    #addCircles(lng = mean_pred_mt_cities$long, lat = mean_pred_mt_cities$lat , radius  = 0.15)
    
    
  })   #CITIES MONTH PREDICTIONS
  output$plot7 <- renderLeaflet({
    
    index_pred = as.integer(input$rb3)
    county_poly = poly_counties[[index_pred]]
    
    slid_val = as.integer(input$slider3)
    predictie =  county_poly@data[[as.character(slid_val)]]
    
    
    bins <- c(seq(1,20,1), Inf)
    pal <- colorBin("YlOrRd", domain =county_poly$res, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><strong>%s</strong> <br/>%g crashes ",
      county_poly$name , county_poly$code , predictie  
    ) %>% lapply(htmltools::HTML)
    
    
    leaflet(county_poly)    %>%
      addTiles()    %>%
      setView(-93.65, 42.0285, zoom = 3) %>%
      
      
      addPolygons(
        fillColor = ~pal(predictie),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      
      addScaleBar()
  })  #COUNTIES MONTH PREDICTIONS
  output$plot8 <- renderLeaflet({
    
    index_pred = as.integer(input$rb4)
    stat_poly = poly_states[[index_pred]]
    
    slid_val = as.integer(input$slider4)
    predictie =  stat_poly@data[[as.character(slid_val)]]
    
    
    bins <- c(seq(1,20,1), Inf)
    pal <- colorBin("YlOrRd", domain =stat_poly$res, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g crashes</strong> density2019 =%g",
      stat_poly$name , predictie  , stat_poly$density
    ) %>% lapply(htmltools::HTML)
    
    
    leaflet(stat_poly)    %>%
      addTiles()    %>%
      setView(-93.65, 42.0285, zoom = 3) %>%
      
      
      addPolygons(
        fillColor = ~pal(predictie),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      
      addScaleBar()
  })  #STATES MONTH PREDICTIONS
  output$plot9 <- renderLeaflet({
    miles_options = c(161000,129000,193000) #100 #80  #120 
    index_miles = list(c(1,2,3,10,11,12),c(4,5,6,13,14,15),c(7,8,9,16,17,18))
    index_coverage = list(c(1:9),c(10:18))
    index_population = list(c(1,4,7,10,13,16),c(2,5,8,11,14,17),c(3,6,9,12,15,18))#50k #40k #60k
    
    
    designs_1 =index_miles[[as.integer(input$rb5)]]
    designs_2 =index_coverage[[as.integer(input$rb6)]]
    designs_3 =index_population[[as.integer(input$rb7)]]
    selected_design = intersect(designs_1,intersect(designs_2,designs_3))
    
    hosp_pred = designmatrix[,5:ncol(designmatrix)][,selected_design]
    
    labels <- sprintf(
      "<strong>%s</strong>  <strong>%s</strong> <br/> hospital allocated =%g",
      designmatrix$City, designmatrix$County,  hosp_pred
    ) %>% lapply(htmltools::HTML)
    
    longitude = (hosp_pred)*full_pred_cities_month[[1]]$long
    lattitude = (hosp_pred)*full_pred_cities_month[[1]]$lat
    print(selected_design)
    leaflet(full_pred_cities_month[[1]])    %>%
      addTiles()    %>%
      setView(-93.65, 42.0285, zoom = 3)     %>%
      addCircles(
        lng = longitude,
        lat = lattitude,
        radius = miles_options[as.integer(input$rb5)] ,
        color = "red",
        opacity = 0.1,
        stroke =FALSE, 
        
        label = labels
      )
    #addCircles(lng = mean_pred_mt_cities$long, lat = mean_pred_mt_cities$lat , radius  = 0.15)
    
    
  })  #hospital PREDICTIONS city
  output$text1 <- renderText({
    index_miles = list(c(1,2,3,10,11,12),c(4,5,6,13,14,15),c(7,8,9,16,17,18))
    index_coverage = list(c(1:9),c(10:18))
    index_population = list(c(1,4,7,10,13,16),c(2,5,8,11,14,17),c(3,6,9,12,15,18))
    designs_1 =index_miles[[as.integer(input$rb5)]]
    designs_2 =index_coverage[[as.integer(input$rb6)]]
    designs_3 =index_population[[as.integer(input$rb7)]]
    selected_design = intersect(designs_1,intersect(designs_2,designs_3))
    paste("total cost:",as.character(costs[selected_design,2])," Billion dollars")})     #text total cost
  output$text2 <- renderText({
    index_miles = list(c(1,2,3,10,11,12),c(4,5,6,13,14,15),c(7,8,9,16,17,18))
    index_coverage = list(c(1:9),c(10:18))
    index_population = list(c(1,4,7,10,13,16),c(2,5,8,11,14,17),c(3,6,9,12,15,18))
    designs_1 =index_miles[[as.integer(input$rb5)]]
    designs_2 =index_coverage[[as.integer(input$rb6)]]
    designs_3 =index_population[[as.integer(input$rb7)]]
    selected_design = intersect(designs_1,intersect(designs_2,designs_3))
    paste("Amount of hospitals:",as.character(sum(designmatrix[,5:ncol(designmatrix)][,selected_design])))
  })     #text amount of hospitals
  output$plot10 <- renderLeaflet({
    
    miles_options = c(129000,161000,193121)
    index_miles = list(c(1,2,3,10,11,12),c(4,5,6,13,14,15),c(7,8,9,16,17,18))
    index_coverage = list(c(1:9),c(10:18))
    index_population = list(c(1,4,7,10,13,16),c(2,5,8,11,14,17),c(3,6,9,12,15,18))
    designs_1 =index_miles[[as.integer(input$rb8)]]
    designs_2 =index_coverage[[as.integer(input$rb9)]]
    designs_3 =index_population[[as.integer(input$rb10)]]
    selected_design = intersect(designs_1,intersect(designs_2,designs_3))
    print(selected_design)
    hosp_pred = design_poly@data[,5:ncol(designmatrix2)][,selected_design]
    
    
    
    bins <- c(0,1)
    pal <- colorNumeric(c("white", "red"), 0:1)

    labels <- sprintf(
      "<strong>%s</strong><strong>%s</strong> <br/>%g hospitals ",
      design_poly$name , design_poly$code , hosp_pred  
    ) %>% lapply(htmltools::HTML)
    
    
    leaflet(design_poly)    %>%
      addTiles()    %>%
      setView(-93.65, 42.0285, zoom = 3) %>%
      
      
      addPolygons(
        fillColor = ~pal(hosp_pred),
        weight = 0.5,
        opacity = 1,
        color = "grey",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      
      addScaleBar()
  }) #hospital PREDICTIONS county
  output$text3 <- renderText({
    
    miles_options = c(129000,161000,193121)
    index_miles = list(c(1,2,3,10,11,12),c(4,5,6,13,14,15),c(7,8,9,16,17,18))
    index_coverage = list(c(1:9),c(10:18))
    index_population = list(c(1,4,7,10,13,16),c(2,5,8,11,14,17),c(3,6,9,12,15,18))
    designs_1 =index_miles[[as.integer(input$rb8)]]
    designs_2 =index_coverage[[as.integer(input$rb9)]]
    designs_3 =index_population[[as.integer(input$rb10)]]
    selected_design = intersect(designs_1,intersect(designs_2,designs_3))
    
    hosp_pred = designmatrix2[,5:ncol(designmatrix2)][,selected_design]
  
    paste("total cost:",as.character(costs2[selected_design,2])," Billion dollars")})
  output$text4 <- renderText({
    miles_options = c(129000,161000,193121)
    index_miles = list(c(1,2,3,10,11,12),c(4,5,6,13,14,15),c(7,8,9,16,17,18))
    index_coverage = list(c(1:9),c(10:18))
    index_population = list(c(1,4,7,10,13,16),c(2,5,8,11,14,17),c(3,6,9,12,15,18))
    designs_1 =index_miles[[as.integer(input$rb8)]]
    designs_2 =index_coverage[[as.integer(input$rb9)]]
    designs_3 =index_population[[as.integer(input$rb10)]]
    selected_design = intersect(designs_1,intersect(designs_2,designs_3))
    
    hosp_pred = designmatrix2[,5:ncol(designmatrix2)][,selected_design]
    
    paste("Amount of hospitals:",as.character(sum(designmatrix2[,5:ncol(designmatrix2)][,selected_design])))
  })
  
  output$plot12 <- renderLeaflet({
    
    miles_options = c(129000,161000,193121)
    index_miles = list(c(1,2,3,10,11,12),c(4,5,6,13,14,15),c(7,8,9,16,17,18))
    index_coverage = list(c(1:9),c(10:18))
    index_population = list(c(1,4,7,10,13,16),c(2,5,8,11,14,17),c(3,6,9,12,15,18))
    designs_1 =index_miles[[as.integer(input$rb11)]]
    designs_2 =index_coverage[[as.integer(input$rb12)]]
    designs_3 =index_population[[as.integer(input$rb13)]]
    selected_design = intersect(designs_1,intersect(designs_2,designs_3))
    print(selected_design)
    hosp_pred = big_design_poly@data[,5:ncol(designmatrix2)][,selected_design]
    
    
    
    bins <- c(-1,0,1)
    pal <- colorNumeric(c("blue", "white","red"), -1:1)
    
    labels <- sprintf(
      "<strong>%s</strong><strong>%s</strong> <br/>%g hospitals ",
      big_design_poly$name , big_design_poly$code , hosp_pred  
    ) %>% lapply(htmltools::HTML)
    
    
    leaflet(design_poly)    %>%
      addTiles()    %>%
      setView(-93.65, 42.0285, zoom = 3) %>%
      
      
      addPolygons(
        fillColor = ~pal(hosp_pred),
        weight = 0.5,
        opacity = 1,
        color = "grey",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      
      addScaleBar()
  }) #hospital PREDICTIONS county
  output$text9 <- renderText({
    
    miles_options = c(129000,161000,193121)
    index_miles = list(c(1,2,3,10,11,12),c(4,5,6,13,14,15),c(7,8,9,16,17,18))
    index_coverage = list(c(1:9),c(10:18))
    index_population = list(c(1,4,7,10,13,16),c(2,5,8,11,14,17),c(3,6,9,12,15,18))
    designs_1 =index_miles[[as.integer(input$rb11)]]
    designs_2 =index_coverage[[as.integer(input$rb12)]]
    designs_3 =index_population[[as.integer(input$rb13)]]
    selected_design = intersect(designs_1,intersect(designs_2,designs_3))
    
    hosp_pred = designmatrix2[,5:ncol(designmatrix2)][,selected_design]
    
    paste("total cost:",as.character(costs2[selected_design,2])," Billion dollars")})
  output$text10 <- renderText({
    miles_options = c(129000,161000,193121)
    index_miles = list(c(1,2,3,10,11,12),c(4,5,6,13,14,15),c(7,8,9,16,17,18))
    index_coverage = list(c(1:9),c(10:18))
    index_population = list(c(1,4,7,10,13,16),c(2,5,8,11,14,17),c(3,6,9,12,15,18))
    designs_1 =index_miles[[as.integer(input$rb11)]]
    designs_2 =index_coverage[[as.integer(input$rb12)]]
    designs_3 =index_population[[as.integer(input$rb13)]]
    selected_design = intersect(designs_1,intersect(designs_2,designs_3))
    
    hosp_pred = designmatrix2[,5:ncol(designmatrix2)][,selected_design]
    
    paste("Amount of hospitals:",as.character(sum(designmatrix2[,5:ncol(designmatrix2)][,selected_design])))
  })
  
  output$plot11 <- renderLeaflet({
   
    year_val = as.integer(input$slider5)
    hosp_pred =  ts_city_pred[,2:ncol(ts_city_pred)][,year_val]
    
    
    labels <- sprintf(
      "<strong>%s</strong>  <strong>%s</strong> <br/>  <strong>population:%g</strong>  <br/>hospital allocated =%g",
      mean_pred_mt_cities$City, mean_pred_mt_cities$State, mean_pred_mt_cities$population,  hosp_pred
    ) %>% lapply(htmltools::HTML)
    
    longitude = (hosp_pred)*mean_pred_mt_cities$long
    lattitude = (hosp_pred)*mean_pred_mt_cities$lat
    leaflet(mean_pred_mt_cities)    %>%
      addTiles()    %>%
      setView(-93.65, 42.0285, zoom = 3)     %>%
      addCircles(
        lng = longitude,
        lat = lattitude,
        radius = 161000,
        color = "red",
        opacity = 0.1,
        stroke =FALSE, 
        label = labels
      )
   
    
    
  })
  output$text5 <- renderText({"minimum population of 50 000"})
  output$text6 <- renderText({"98% needs to within a distance of 100 miles"})#hospital PREDICTIONS timeseries city
  output$text7 <- renderText({paste("the total cost",as.character(3.817),"Trillion Dollars")})
  output$text8 <- renderText({"the total amount of hospitals build: 145"})
  
  #HEXAGON GRID
  
  output$plot5 <- renderLeaflet({

    border <- gUnaryUnion(states)
    border <- as(border, "SpatialPolygonsDataFrame")

    lin <- as(border, "SpatialLinesDataFrame")
    pts <- as.data.frame(as(lin, "SpatialPointsDataFrame"))

    dfr <- data.frame(x = pts$x, y = pts$y)

    dfr <- dfr[which(dfr$x > -125),]

    dfr <- dfr[which(dfr$y > 24),]

    eg <- st_as_sf(dfr, coords = c("x", "y"))
    poly <- concaveman(eg)

    dfr <- data.frame(x = st_coordinates(poly)[,1], y = st_coordinates(poly)[,2])

    p <- st_polygon(list(as.matrix(dfr)))

    grens <- data.frame(st_coordinates(p))
    grens <- cbind(grens$X, grens$Y)
    grens <- Polygon(grens)
    grens <- Polygons(list(grens),1)
    grens <- SpatialPolygons(list(grens))

    pbuf <- st_buffer(p, .5)


    coords <- data.frame(st_coordinates(pbuf))

    xym <- cbind(coords$X, coords$Y)
    border <- Polygon(xym)
    border <- Polygons(list(border),1)
    border <- SpatialPolygons(list(border))

    # with clipping
    hex_points <- spsample(border, type = "hexagonal", cellsize = 1)
    hex_grid <- HexPoints2SpatialPolygons(hex_points, dx = 1)
    hex_grid <- gIntersection(grens,hex_grid, byid = T, )
    row.names(hex_grid) <- as.character(1:length(hex_grid))

    leaflet()    %>%
      addTiles()    %>%
      setView(-93.65, 42.0285, zoom = 3) %>%
      addScaleBar() %>%
      addPolygons(data = hex_grid,color = "blue" ) %>%
      addScaleBar()
  })

}


shinyApp(ui, server)
















