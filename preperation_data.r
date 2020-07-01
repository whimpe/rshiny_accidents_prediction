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

#Load data
setwd(readClipboard())
uscities <- read.csv("uscities.csv")
DF <- read.csv("futureAccidentDATA.csv")
DFsub<-DF[,c(6,7,15,17,ncol(DF))]
Crashcities <- merge(DFsub, uscities[,c('city', 'state_id', 'population', 'county_name')], by.x = c('City', 'State'), by.y= c('city', 'state_id'))
#CREATION SUBSET DATASET
random_indices <- sample(1:nrow(Crashcities), 10000, replace= FALSE)
DF_sample <- Crashcities[random_indices,]
DF_sample <- na.omit(DF_sample)

load("pred_cities_month.rdata")
load("pred_counties_month.rdata")
load("pred_states_month.rdata")

################################## MOET AANGEPAST WORDEN

states_loc <-read.csv("states_locations.csv",header = TRUE)
states <- geojsonio::geojson_read(x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", what = "sp")
counties <- geojsonio::geojson_read(x ="https://raw.githubusercontent.com/python-visualization/folium/master/tests/us-counties.json", what = "sp" )
cities <- geojsonio::geojson_read(x = "https://raw.githubusercontent.com/ResidentMario/geoplot-data/master/usa-cities.geojson", what = "sp")
## Add the states for the county
coords_centroid_of_county <- getSpPPolygonsLabptSlots(counties)
#--------------------------------------------------------------------  mischien lat en lng omdraaien hier
points_of_centroids <-  SpatialPoints(coords_centroid_of_county)
points_of_centroids_city <-  SpatialPoints(cbind(uscities$lng,uscities$lat))
points_city <- SpatialPointsDataFrame(coords = cbind(uscities$lng,uscities$lat), data = uscities, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


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


points <- SpatialPointsDataFrame(coords = DF_sample[,c(5,4)], data = DF_sample, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
count_of_crashes <- poly.counts(points ,counties)  #crashes per county
counties@data <- cbind(counties@data,count_of_crashes)
dfdf <- data.frame(counties@data)




DF_sample$time <- strptime(x = as.character(DF_sample$time),format = "%Y-%m-%d %H:%M:%S")
years <- unique(DF_sample$time$year)
years <- years + 1900
require(tidyverse)
make_time_series_df_per_geographic_area <- function(df = DF_sample,time_interval = "month"){
    
    
    month <- sort(unique(format(as.Date(DF_sample$time), "%Y-%m")))
    crashpertime <- data.frame(unique(DF_sample[,c('City','county_name', 'State','population')]))
    
    for(i in month){
        data_subset <- df[which(format(as.Date(df$time), "%Y-%m") == i) ,]
        
        count_of_crashes_time_interval <- data.frame(data_subset %>% count(City, county_name, State))
        
        crashpertime <- merge(crashpertime, count_of_crashes_time_interval, by=c("City", "county_name", "State"), all.x=TRUE) 
        
    }
    colnames(crashpertime)[5:ncol(crashpertime)] <- month
    return(crashpertime)
}

opl_cities_month = make_time_series_df_per_geographic_area(DF_sample,"month")
opl_cities_month[is.na(opl_cities_month)] <- 0
colnames(opl_cities_month)[2]<-'County'
opl_counties_month = aggregate(opl_cities_month[,5:ncol(opl_cities_month)], by=list(opl_cities_month$County,opl_cities_month$State), sum)
colnames(opl_counties_month)[1]<-'County'
colnames(opl_counties_month)[2]<-'State'
opl_states_month = aggregate(opl_cities_month[,5:ncol(opl_cities_month)], by=list(opl_cities_month$State), sum)
colnames(opl_states_month)[1]<-'State'

