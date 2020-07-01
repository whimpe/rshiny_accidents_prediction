##################
#Group 14        #
#Willem Himpe    #
#Willem Lannoye  #
#Axelle Sels     #
#Thibault Verduyn#
##################
rm(list=ls())
library(forecast)
library(Metrics)
library(dplyr)
library(ggplot2)
library(scales)
library(extrafont)
library(ggthemes)
set.seed(1000)

#Load data
setwd(choose.dir())
uscities <- read.csv("uscities.csv")
DF <- read.csv("futureAccidentDATA.csv")
DFsub<-DF[,c(6,7,15,17,ncol(DF))]
Crashcities <- merge(DFsub, uscities[,c('city', 'state_id', 'population', 'county_name')], by.x = c('City', 'State'), by.y= c('city', 'state_id'))
data <- na.omit(Crashcities)
data$time <- strptime(x = as.character(data$time),format = "%Y-%m-%d %H:%M:%S")
years <- unique(data$time$year)
years <- years + 1900

require(tidyverse)
make_time_series_df_per_geographic_area <- function(df = data, time_interval = "month"){
  
  
  month <- sort(unique(format(as.Date(data$time), "%Y-%m")))
  crashpertime <- data.frame(unique(data[,c('City','county_name', 'State','population')]))
  
  for(i in month){
    data_subset <- df[which(format(as.Date(df$time), "%Y-%m") == i) ,]
    
    count_of_crashes_time_interval <- data.frame(data_subset %>% count(City, county_name, State))
    
    crashpertime <- merge(crashpertime, count_of_crashes_time_interval, by=c("City", "county_name", "State"), all.x=TRUE) 
    
  }
  colnames(crashpertime)[5:ncol(crashpertime)] <- month
  return(crashpertime)
}
#sums per month and region
cities_month = make_time_series_df_per_geographic_area(data,"month")
cities_month[is.na(cities_month)] <- 0
cities_month<-cities_month[,-which(names(cities_month) %in% c("2035-03","2036-01","2040-01"))]
colnames(cities_month)[2]<-'County'
counties_month = aggregate(cities_month[,5:ncol(cities_month)], by=list(cities_month$County,cities_month$State), sum)
colnames(counties_month)[1]<-'County'
colnames(counties_month)[2]<-'State'
states_month = aggregate(cities_month[,5:ncol(cities_month)], by=list(cities_month$State), sum)
colnames(states_month)[1]<-'State'

#totals
total_accidents <-as.data.frame(colSums(states_month %>% select(`2036-02`:ncol(states_month))))
total_accidents<-cbind(total_accidents,colnames(states_month %>% select(`2036-02`:ncol(states_month))))
colnames(total_accidents)<-c("number_of_accidents","month")
total_accidents$month <- anytime::anydate(total_accidents$month)
#plot totals
p <- ggplot(total_accidents, aes(x=month, y=number_of_accidents)) + 
  geom_point(colour = "#2a383f") +
  theme_minimal() +
  xlab("Time") +
  ylab("Number of accidents") +
  scale_y_continuous(labels = comma) +
  geom_vline(xintercept = as.numeric(total_accidents[6,2]))
p + theme(text=element_text(size=16,  family="Clear Sans", colour = "#2a383f", face="bold")) +
  theme(plot.title = element_text(hjust = 0.5, colour = "#2a383f")) +
  theme(axis.text.x = element_text(colour = "#2a383f"))

#select only number of accidents
opl_cities_month <- cities_month %>% select(`2036-07`:ncol(cities_month))
opl_counties_month <- counties_month %>% select(`2036-07`:ncol(counties_month))
opl_states_month <- states_month %>% select(`2036-07`:ncol(states_month))

#######################################
####### TIME SERIES PREDICTION#########
#######################################
#Change wd to save results later
setwd(choose.dir())

#Create train and test sets
opl_states_month_train <- opl_states_month %>% select(`2036-07`:`2038-12`)
opl_states_month_test <- opl_states_month %>% select(`2039-01`:`2039-12`)
opl_counties_month_train <- opl_counties_month %>% select(`2036-07`:`2038-12`)
opl_counties_month_test <- opl_counties_month %>% select(`2039-01`:`2039-12`)
opl_cities_month_train <- opl_cities_month %>% select(`2036-07`:`2038-12`)
opl_cities_month_test <- opl_cities_month %>% select(`2039-01`:`2039-12`)

#Create performance measures dataframe
performanceMeasures <- as.data.frame(matrix(0, nrow = 21, ncol = 3))
colnames(performanceMeasures) <- c("MAE","RMSE","MASE")
row.names(performanceMeasures) <- c("mean_pred_mt_states","mov_avg_mt_states","exp_smoothing_mt_states","holt_mt_states","holtwinter_mt_states","pred_arima_mt_states","ensemble_mt_states",
                                    "mean_pred_mt_counties","mov_avg_mt_counties","exp_smoothing_mt_counties","holt_mt_counties","holtwinter_mt_counties","pred_arima_mt_counties","ensemble_mt_counties",
                                    "mean_pred_mt_cities","mov_avg_mt_cities","exp_smoothing_mt_cities","holt_mt_cities","holtwinter_mt_cities","pred_arima_mt_cities","ensemble_mt_cities")


#Create performance measures function
pm.fun <- function(method){
  if(method=="forecast_mean"){
    name <- "mean_pred_"
  }else if(method=="forecast_mov_avg"){
    name <- "mov_avg_"
  }else if(method=="forecast_exp_smoothing"){
    name <- "exp_smoothing_"
  }else if(method=="forecast_holt"){
    name <- "holt_"
  }else if(method=="forecast_holtwinter"){
    name <- "holtwinter_"
  }else if(method=="forecast_arima"){
    name <- "pred_arima_"
  }
  
  performanceMeasures[paste(name,"mt_states",sep=""),"MAE"] <<- as.double(mae(unlist(opl_states_month_test),
                                                                              unlist(get(method)(opl_states_month_train) %>% select(`1`:`12`))))
  performanceMeasures[paste(name,"mt_states",sep=""),"RMSE"] <<- as.double(rmse(unlist(opl_states_month_test),
                                                                                unlist(get(method)(opl_states_month_train) %>% select(`1`:`12`))))
  performanceMeasures[paste(name,"mt_states",sep=""),"MASE"] <<- as.double(mase(unlist(opl_states_month_test),
                                                                                unlist(get(method)(opl_states_month_train) %>% select(`1`:`12`)),step_size = 12))
  performanceMeasures[paste(name,"mt_counties",sep=""),"MAE"] <<- as.double(mae(unlist(opl_counties_month_test),
                                                                                unlist(get(method)(opl_counties_month_train) %>% select(`1`:`12`))))
  performanceMeasures[paste(name,"mt_counties",sep=""),"RMSE"] <<- as.double(rmse(unlist(opl_counties_month_test),
                                                                                  unlist(get(method)(opl_counties_month_train) %>% select(`1`:`12`))))
  performanceMeasures[paste(name,"mt_counties",sep=""),"MASE"] <<- as.double(mase(unlist(opl_counties_month_test),
                                                                                  unlist(get(method)(opl_counties_month_train) %>% select(`1`:`12`)),step_size = 12))
  performanceMeasures[paste(name,"mt_cities",sep=""),"MAE"] <<- as.double(mae(unlist(opl_cities_month_test),
                                                                              unlist(get(method)(opl_cities_month_train) %>% select(`1`:`12`))))
  performanceMeasures[paste(name,"mt_cities",sep=""),"RMSE"] <<- as.double(rmse(unlist(opl_cities_month_test),
                                                                                unlist(get(method)(opl_cities_month_train) %>% select(`1`:`12`))))
  performanceMeasures[paste(name,"mt_cities",sep=""),"MASE"] <<- as.double(mase(unlist(opl_cities_month_test),
                                                                                unlist(get(method)(opl_cities_month_train) %>% select(`1`:`12`)),step_size = 12))
}


#MEAN prediction
forecast_mean <- function(data=opl_states_month){
  predicted_mean_crashes = round(rowMeans(data, na.rm = TRUE))
  pred = matrix(predicted_mean_crashes,nrow = nrow(data),ncol = 252)
  names = seq(1,252)
  colnames(pred) = names
  data = cbind(data,pred)
  return(data)
}

#predictions for 2040-2060
mean_pred_mt_cities = cbind(cities_month %>% select("City":"2036-06"),  forecast_mean(opl_cities_month))
mean_pred_mt_counties = cbind(counties_month %>% select("County":"2036-06"),  forecast_mean(opl_counties_month))
mean_pred_mt_states = cbind(states_month %>% select("State":"2036-06"),  forecast_mean(opl_states_month))
save(mean_pred_mt_states,file = "mean_pred_mt_states.RData")
save(mean_pred_mt_counties,file = "mean_pred_mt_counties.RData")
save(mean_pred_mt_cities,file = "mean_pred_mt_cities.RData")

#performance of method with train and test set
pm.fun("forecast_mean")

#MOVING AVERAGE prediction
forecast_mov_avg <- function(data=opl_states_month) {
  lengte <- 12
  predicted_crashes <- data
  pred = matrix(0,nrow = nrow(data),ncol = 252)
  names = seq(1,252)
  
  for (i in 1:252){
    pred[,i]=rowMeans(predicted_crashes[,(ncol(predicted_crashes)-lengte):ncol(predicted_crashes)],na.rm = TRUE)
    predicted_crashes = cbind(predicted_crashes,pred[,i])
  }
  
  colnames(pred) = names
  data = cbind(data,pred)
  return(data)
}

#predictions for 2040-2060
mov_avg_mt_cities = cbind(cities_month %>% select("City":"2036-06"),  forecast_mov_avg(opl_cities_month))
mov_avg_mt_counties = cbind(counties_month %>% select("County":"2036-06"),  forecast_mov_avg(opl_counties_month))
mov_avg_mt_states = cbind(states_month %>% select("State":"2036-06"),  forecast_mov_avg(opl_states_month))
save(mov_avg_mt_states,file = "mov_avg_mt_states.RData")
save(mov_avg_mt_counties,file = "mov_avg_mt_counties.RData")
save(mov_avg_mt_cities,file = "mov_avg_mt_cities.RData")

#performance of method with train and test set
pm.fun("forecast_mov_avg")

#EXPONENTIAL SMOOTHING  prediction
forecast_exp_smoothing <- function(data=opl_states_month,region="state"){
  alpha <- 0.2
  predicted_crashes <- data
  start = ncol(predicted_crashes)+1
  names = seq(1,252)
  
  pred = matrix(0,ncol = 252, nrow = nrow(data))
  for(i in 1:nrow(data)){
    pred[i,] = ses(predicted_crashes[i,], alpha = 0.2, initial = "simple", h = 252)$mean
  }
  
  result = round(pred)
  colnames(result) = names
  data = cbind(data,result)
  return(data)
}

#predictions for 2040-2060
exp_smoothing_mt_cities = cbind(cities_month %>% select("City":"2036-06"),  forecast_exp_smoothing(opl_cities_month))
exp_smoothing_mt_counties = cbind(counties_month %>% select("County":"2036-06"),  forecast_exp_smoothing(opl_counties_month))
exp_smoothing_mt_states = cbind(states_month %>% select("State":"2036-06"),  forecast_exp_smoothing(opl_states_month))
save(exp_smoothing_mt_states,file = "exp_smoothing_mt_states.RData")
save(exp_smoothing_mt_counties,file = "exp_smoothing_mt_counties.RData")
save(exp_smoothing_mt_cities,file = "exp_smoothing_mt_cities.RData")

#performance of method with train and test set
pm.fun("forecast_exp_smoothing")

#HOLT'S LINEAR METHOD  prediction
forecast_holt <- function(data=opl_states_month){
  alpha <- 0.2
  beta <- 0.2
  
  names = seq(1,252)
  holt_pred = as.data.frame(matrix(0,nrow = nrow(data),ncol = 252))
  for(j in 1:nrow(data)){
    time_series <- ts(unlist(as.vector(data[j,])), start = c(36,2), end = c(39,12), frequency = 12) #first value of start & end is year 2nd item in vector is month
    pred <- as.data.frame(holt(time_series,alpha = alpha,beta = beta,initial="simple", h=252))
    holt_pred[j,1:252] <-  (pred$`Point Forecast`)
  }
  
  colnames(holt_pred)=names
  holt_pred[holt_pred<0]<-0
  holt_pred = round(holt_pred)
  data = cbind(data,holt_pred)
  return(data)
}

#predictions for 2040-2060
holt_mt_cities = cbind(cities_month %>% select("City":"2036-06"),  forecast_holt(opl_cities_month))
holt_mt_counties = cbind(counties_month %>% select("County":"2036-06"),  forecast_holt(opl_counties_month))
holt_mt_states = cbind(states_month %>% select("State":"2036-06"),  forecast_holt(opl_states_month))
save(holt_mt_states,file = "holt_mt_states.RData")
save(holt_mt_counties,file = "holt_mt_counties.RData")
save(holt_mt_cities,file = "holt_mt_cities.RData")

#performance of method with train and test set
pm.fun("forecast_holt")

#HOLT'S WINTER METHOD  prediction
forecast_holtwinter <- function(data=opl_states_month){
  alpha <- 0.2
  beta <- 0.2
  phi <- 0.2
  
  names = seq(1,252)
  holt_pred = as.data.frame(matrix(0,nrow = nrow(data),ncol = 252))
  for(j in 1:nrow(data)){
    time_series <- ts( unlist(as.vector(data[j,])) , start = c(36,2), end = c(39,12), frequency = 12) #first value of start en end is year 2nd item in vector is month
    pred <- as.data.frame(hw(time_series,alpha = alpha, beta= beta,phi = phi,initial="simple", h=252))
    holt_pred[j,1:252] <-  (pred$`Point Forecast`)
  }
  
  colnames(holt_pred)=names
  holt_pred[holt_pred<0]<-0
  holt_pred = round(holt_pred)
  data = cbind(data,holt_pred)
  return(data)
}

#predictions for 2040-2060
holtwinter_mt_cities = cbind(cities_month %>% select("City":"2036-06"),  forecast_holtwinter(opl_cities_month))
holtwinter_mt_counties = cbind(counties_month %>% select("County":"2036-06"),  forecast_holtwinter(opl_counties_month))
holtwinter_mt_states = cbind(states_month %>% select("State":"2036-06"),  forecast_holtwinter(opl_states_month))
save(holtwinter_mt_states,file = "holtwinter_mt_states.RData")
save(holtwinter_mt_counties,file = "holtwinter_mt_counties.RData")
save(holtwinter_mt_cities,file = "holtwinter_mt_cities.RData")

#performance of method with train and test set
pm.fun("forecast_holtwinter")

#ARIMA  prediction
forecast_arima <- function(data=opl_states_month){
  names=seq(1,252)
  arima_pred <- matrix(-1,nrow = nrow(data),ncol = 252)
  for(j in 1:nrow(data)){
    time_series <- ts( unlist(as.vector(data[j,])) , start = c(36,2), end = c(39,12), frequency = 12) #first value of start en end is year 2nd item in vector is month
    arima <- auto.arima(time_series, allowdrift=FALSE)
    pred <- predict(arima,n.ahead = 252)
    arima_pred[j,1:252] <-  as.vector(pred$pred)
  }
  arima_pred = round(arima_pred)
  colnames(arima_pred)=names
  arima_pred[arima_pred<0]<-0
  data = cbind(data,arima_pred)
  return(data)
}

#predictions 2040-2060
pred_arima_mt_cities = cbind(cities_month %>% select("City":"2036-06"),  forecast_arima(opl_cities_month))
pred_arima_mt_counties = cbind(counties_month %>% select("County":"2036-06"),  forecast_arima(opl_counties_month))
pred_arima_mt_states = cbind(states_month %>% select("State":"2036-06"),  forecast_arima(opl_states_month))
save(pred_arima_mt_states,file = "pred_arima_mt_states.RData")
save(pred_arima_mt_counties,file = "pred_arima_mt_counties.RData")
save(pred_arima_mt_cities,file = "pred_arima_mt_cities.RData")

#performance of method with train and test set
pm.fun("forecast_arima")

#create ensemble
pred_states_month <- round(mean_pred_mt_states[,2:300]*(1/performanceMeasures["mean_pred_mt_states","MASE"])/sum(1/performanceMeasures[1:6,"MASE"])+
                             mov_avg_mt_states[,2:300]*(1/performanceMeasures["mov_avg_mt_states","MASE"])/sum(1/performanceMeasures[1:6,"MASE"])+
                             exp_smoothing_mt_states[,2:300]*(1/performanceMeasures["exp_smoothing_mt_states","MASE"])/sum(1/performanceMeasures[1:6,"MASE"])+
                             holt_mt_states[,2:300]*(1/performanceMeasures["holt_mt_states","MASE"])/sum(1/performanceMeasures[1:6,"MASE"])+
                             holtwinter_mt_states[,2:300]*(1/performanceMeasures["holtwinter_mt_states","MASE"])/sum(1/performanceMeasures[1:6,"MASE"])+
                             pred_arima_mt_states[,2:300]*(1/performanceMeasures["pred_arima_mt_states","MASE"])/sum(1/performanceMeasures[1:6,"MASE"]))
pred_states_month <- cbind(State=states_month[,1],pred_states_month)
save(pred_states_month,file = "pred_states_month.RData")
pred_counties_month <- round(mean_pred_mt_counties[,3:301]*(1/performanceMeasures["mean_pred_mt_counties","MASE"])/sum(1/performanceMeasures[8:13,"MASE"])+
                               mov_avg_mt_counties[,3:301]*(1/performanceMeasures["mov_avg_mt_counties","MASE"])/sum(1/performanceMeasures[8:13,"MASE"])+
                               exp_smoothing_mt_counties[,3:301]*(1/performanceMeasures["exp_smoothing_mt_counties","MASE"])/sum(1/performanceMeasures[8:13,"MASE"])+
                               holt_mt_counties[,3:301]*(1/performanceMeasures["holt_mt_counties","MASE"])/sum(1/performanceMeasures[8:13,"MASE"])+
                               holtwinter_mt_counties[,3:301]*(1/performanceMeasures["holtwinter_mt_counties","MASE"])/sum(1/performanceMeasures[8:13,"MASE"])+
                               pred_arima_mt_counties[,3:301]*(1/performanceMeasures["pred_arima_mt_counties","MASE"])/sum(1/performanceMeasures[8:13,"MASE"]))
pred_counties_month <- cbind(counties_month[,1:2],pred_counties_month)
save(pred_counties_month,file = "pred_counties_month.RData")
pred_cities_month <- round(mean_pred_mt_cities[,5:303]*(1/performanceMeasures["mean_pred_mt_cities","MASE"])/sum(1/performanceMeasures[15:20,"MASE"])+
                             mov_avg_mt_cities[,5:303]*(1/performanceMeasures["mov_avg_mt_cities","MASE"])/sum(1/performanceMeasures[15:20,"MASE"])+
                             exp_smoothing_mt_cities[,5:303]*(1/performanceMeasures["exp_smoothing_mt_cities","MASE"])/sum(1/performanceMeasures[15:20,"MASE"])+
                             holt_mt_cities[,5:303]*(1/performanceMeasures["holt_mt_cities","MASE"])/sum(1/performanceMeasures[15:20,"MASE"])+
                             holtwinter_mt_cities[,5:303]*(1/performanceMeasures["holtwinter_mt_cities","MASE"])/sum(1/performanceMeasures[15:20,"MASE"])+
                             pred_arima_mt_cities[,5:303]*(1/performanceMeasures["pred_arima_mt_cities","MASE"])/sum(1/performanceMeasures[15:20,"MASE"]))
pred_cities_month <- cbind(cities_month[,1:4],pred_cities_month)
save(pred_cities_month,file = "pred_cities_month.RData")

#performance of ensemble with train and test set
pred_states_month_train <- round((forecast_mean(opl_states_month_train) %>% select(`1`:`12`))*(1/performanceMeasures["mean_pred_mt_states","MASE"])/sum(1/performanceMeasures[1:6,"MASE"])+
                                  (forecast_mov_avg(opl_states_month_train) %>% select(`1`:`12`))*(1/performanceMeasures["mov_avg_mt_states","MASE"])/sum(1/performanceMeasures[1:6,"MASE"])+
                                  (forecast_exp_smoothing(opl_states_month_train) %>% select(`1`:`12`))*(1/performanceMeasures["exp_smoothing_mt_states","MASE"])/sum(1/performanceMeasures[1:6,"MASE"])+
                                  (forecast_holt(opl_states_month_train) %>% select(`1`:`12`))*(1/performanceMeasures["holt_mt_states","MASE"])/sum(1/performanceMeasures[1:6,"MASE"])+
                                  (forecast_holtwinter(opl_states_month_train) %>% select(`1`:`12`))*(1/performanceMeasures["holtwinter_mt_states","MASE"])/sum(1/performanceMeasures[1:6,"MASE"])+
                                  (forecast_arima(opl_states_month_train) %>% select(`1`:`12`))*(1/performanceMeasures["pred_arima_mt_states","MASE"])/sum(1/performanceMeasures[1:6,"MASE"]))
performanceMeasures["ensemble_mt_states","MAE"] <- as.double(mae(unlist(opl_states_month_test),unlist(pred_states_month_train)))
performanceMeasures["ensemble_mt_states","RMSE"] <- as.double(rmse(unlist(opl_states_month_test),unlist(pred_states_month_train)))
performanceMeasures["ensemble_mt_states","MASE"] <- as.double(mase(unlist(opl_states_month_test),unlist(pred_states_month_train)),step_size=12)
pred_counties_month_train <- round((forecast_mean(opl_counties_month_train) %>% select(`1`:`12`))(1/performanceMeasures["mean_pred_mt_counties","MASE"])/sum(1/performanceMeasures[8:13,"MASE"])+
                                     (forecast_mov_avg(opl_counties_month_train) %>% select(`1`:`12`))*(1/performanceMeasures["mov_avg_mt_counties","MASE"])/sum(1/performanceMeasures[8:13,"MASE"])+
                                     (forecast_exp_smoothing(opl_counties_month_train) %>% select(`1`:`12`))*(1/performanceMeasures["exp_smoothing_mt_counties","MASE"])/sum(1/performanceMeasures[8:13,"MASE"])+
                                     (forecast_holt(opl_counties_month_train) %>% select(`1`:`12`))*(1/performanceMeasures["holt_mt_counties","MASE"])/sum(1/performanceMeasures[8:13,"MASE"])+
                                     (forecast_holtwinter(opl_counties_month_train) %>% select(`1`:`12`))*(1/performanceMeasures["holtwinter_mt_counties","MASE"])/sum(1/performanceMeasures[8:13,"MASE"])+
                                     (forecast_arima(opl_counties_month_train) %>% select(`1`:`12`))*(1/performanceMeasures["pred_arima_mt_counties","MASE"])/sum(1/performanceMeasures[8:13,"MASE"]))
performanceMeasures["ensemble_mt_counties","MAE"] <- as.double(mae(unlist(opl_counties_month_test),unlist(pred_counties_month_train)))
performanceMeasures["ensemble_mt_counties","RMSE"] <- as.double(rmse(unlist(opl_counties_month_test),unlist(pred_counties_month_train)))
performanceMeasures["ensemble_mt_counties","MASE"] <- as.double(mase(unlist(opl_counties_month_test),unlist(pred_counties_month_train)),step_size=12)
pred_cities_month_train <- round((forecast_mean(opl_cities_month_train) %>% select(`1`:`12`))*(1/performanceMeasures["mean_pred_mt_cities","MASE"])/sum(1/performanceMeasures[15:20,"MASE"])+
                                   (forecast_mov_avg(opl_cities_month_train) %>% select(`1`:`12`))*(1/performanceMeasures["mov_avg_mt_cities","MASE"])/sum(1/performanceMeasures[15:20,"MASE"])+
                                   (forecast_exp_smoothing(opl_cities_month_train) %>% select(`1`:`12`))*(1/performanceMeasures["exp_smoothing_mt_cities","MASE"])/sum(1/performanceMeasures[15:20,"MASE"])+
                                   (forecast_holt(opl_cities_month_train) %>% select(`1`:`12`))*(1/performanceMeasures["holt_mt_cities","MASE"])/sum(1/performanceMeasures[15:20,"MASE"])+
                                   (forecast_holtwinter(opl_cities_month_train) %>% select(`1`:`12`))*(1/performanceMeasures["holtwinter_mt_cities","MASE"])/sum(1/performanceMeasures[15:20,"MASE"])+
                                   (forecast_arima(opl_cities_month_train) %>% select(`1`:`12`))*(1/performanceMeasures["pred_arima_mt_cities","MASE"])/sum(1/performanceMeasures[15:20,"MASE"]))
performanceMeasures["ensemble_mt_cities","MAE"] <- as.double(mae(unlist(opl_cities_month_test),unlist(pred_cities_month_train)))
performanceMeasures["ensemble_mt_cities","RMSE"] <- as.double(rmse(unlist(opl_cities_month_test),unlist(pred_cities_month_train)))
performanceMeasures["ensemble_mt_cities","MASE"] <- as.double(mase(unlist(opl_cities_month_test),unlist(pred_cities_month_train)),step_size=12)
