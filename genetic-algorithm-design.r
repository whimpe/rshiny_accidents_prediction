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

#Load data and merge accidents with city information
setwd("H:/R/groupwork/FINAL ASSIGNMENT")
uscities <- read.csv("uscities.csv")
DF <- read.csv("futureAccidentDATA.csv")
DFsub<-DF[,c(6,7,15,17,ncol(DF))]
Crashcities <- merge(DFsub, uscities[,c('city', 'state_id', 'population', 'county_name')], by.x = c('City', 'State'), by.y= c('city', 'state_id'))

load("data/pred_cities_month.rdata")
load("data/pred_counties_month.rdata")
load("data/pred_states_month.rdata")

####################GENETIC ALGORITHM##############
#filter on unique cities 

DM<-Crashcities
DM$City <- paste(as.character(DM$City),"-",as.character(DM$State))
DM$State <- NULL
DM <- DM[order(DM$City),] 


#load previously calculated centroids of occurred accidents per city and create distance matrix
avgLatLon<-read.csv("data/centroids.csv")
avgLatLon$X<-NULL

library(geosphere)

m<-as.matrix(distm(avgLatLon[,2:3], fun=distHaversine),labels=TRUE)
colnames(m) <- rownames(m) <- avgLatLon[["City"]]

#convert to miles
m<-(m/1000)/1.609344
detach("package:geosphere")

#calculate total number of accidents per city based on the previously made forecasts

allocateInitialHospitals <- function(accidents){
  sub<-accidents[,(ncol(accidents)-240):ncol(accidents)]
  accidents$totalAccidents<-rowSums(sub)
  accidents<-accidents[,c(1:4,ncol(accidents))]
  
  accidents$City<-paste(as.character(accidents$City), '-', as.character(accidents$State))
  accidents$State<-NULL
  
  return(accidents)}

citypop<-allocateInitialHospitals(pred_cities_month)

#Read in feasible solutions, created by iterating the algorithm multiple times on the standard constraints without giving suggestions

initial<-read.csv("data/initials.csv")
initial$X<-NULL
initial<-as.matrix(initial)

#save indexes from all cities where a hospital may be built with standard constraints
indexstandaard<-c()
for(i in 1:nrow(citypop)){
  if(citypop$population[i] > 50000)
    indexstandaard <- append(indexstandaard, i)
  
}

#create matrix of 9 rows and the number of cities as number of columns, that will serve as matrix of suggestions for the genetic algorithm
#on the indexes of cities from the initial solution based on the standard constraints, a one is filled in

newinitial<-matrix(0,9,nrow(citypop))
newinitial<-data.frame(newinitial)
count=1
for(i in 1:nrow(citypop)){
  if(i %in% indexstandaard){
    newinitial[,i]<-initial[,count]
    count= count +1
  }
  
}




library(genalg)


#function to run the algorithm on multiple designs with different constraints

sensitivityanalysis <- function(popmin, distmax, noncovmax){

  distdf <- data.frame(m)
  habitantcities<-nrow(citypop[citypop$population>popmin,])

  #make from to distance matrix based on minimum population constraint
  index <- c()
  for(i in 1:nrow(distdf)){
    if(citypop$population[i] < popmin)
      index <- append(index, i)
  
  }
  distdf[,index] <- NULL
  
  #match initial solution with minimum population constraint
  suggestion <- newinitial
  suggestion[,index] <- NULL
  suggestion <- as.matrix(suggestion)

  #save information of cities where it is allowed to build a hospital
  pop <- citypop
  pop<-pop[-index,]


  #create evaluation function
  fn <- function(hospitals){
  
  
  depdist <- distdf
  cost = 0
  mindistancetohospital <- c()
  verschil = 0
  
  #set distances to cities without hospitals on infinity
  for(i in 1:length(hospitals)){
    if(hospitals[i]==0){
      depdist[,i]<-Inf
    }
  }
  
  #determine minimum distance per city
  mindistance <- apply(depdist,1, min)
  for (i in 1:length(mindistance)){
    mindistancetohospital[i]<- mindistance[[i]]
  }
  
  #check distance constraint and determine the percentage of coverage for all accidents
  #if the constraint is unsatisfied, put the cost extremely large and proportional with the percentage of non-coverage to enhance learning speed
  toofar <- c()
  for(i in 1:length(mindistancetohospital)){
    toofar[i] <- ifelse(mindistancetohospital[i]>distmax, 1,0)}
  noncoverage = sum(toofar * citypop$totalAccidents) / (sum(citypop$totalAccidents))
  print(noncoverage)
  if(noncoverage>noncovmax){
    cost = noncoverage*10000000000000000000000000000000000000000000000
  }
  
  #if constraint is satisfied, calculate costs of the solution
  if(cost==0){
    
    #remove distances for accidents that cannot be covered
    mindistance<-ifelse(mindistancetohospital>distmax,0,mindistancetohospital)
    #calculate cost
    cost = (sum(hospitals) * 50000000) + 5000*20*sum((hospitals)*(pop$population)) + 10*sum((mindistance)*(citypop$totalAccidents))
    }
  
  
  return(cost)
  
}

#run the algorithm based on created evaluation function and suggestion and demand the length of vector of number of cities matching the minimum population constraint
modelfin <- rbga.bin(size = habitantcities, popSize = 20,iters = 100, evalFunc = fn, mutationChance = 0.10, suggestions = suggestion)

standardmodel<-modelfin

#select the best solution of the iterations
mincost <- standardmodel$best[length(standardmodel$best)]
bestallocation <- standardmodel$population[which.min(standardmodel$evaluations),]

#create the new allocation vector with length of all cities, thus not only those satisfying the minimum population constraint
indexbigcities<-c()
for(i in 1:nrow(distdf)){
  if(citypop$population[i] > popmin)
    indexbigcities <- append(indexbigcities, i)
  
}

result<-matrix(0,nrow(citypop),1)
result<-data.frame(result)
count=1
for(i in 1:nrow(citypop)){
  if(i %in% indexbigcities){
    result[i,1]<-bestallocation[count]
    count= count +1
  }
  
}

#save the allocation of hosiptals and the matching cost
resultaat <- list()
resultaat[[1]] <- result
resultaat[[2]] <- mincost

return(resultaat)}

#end of function

#create parameters for multiple designs to perform sensitivity analysis

designs <- list()
designs[[1]] = c(50000, 100,0.02)
designs[[2]] = c(40000, 100,0.02)
designs[[3]] = c(60000, 100,0.02)
designs[[4]] = c(50000, 80,0.02)
designs[[5]] = c(40000, 80,0.02)
designs[[6]] = c(60000, 80,0.02)
designs[[7]] = c(50000, 120,0.02)
designs[[8]] = c(40000, 120,0.02)
designs[[9]] = c(60000, 120,0.02)

designs[[10]] = c(50000, 100,0.01)
designs[[11]] = c(40000, 100,0.01)
designs[[12]] = c(60000, 100,0.01)
designs[[13]] = c(50000, 80,0.01)
designs[[14]] = c(40000, 80,0.01)
designs[[15]] = c(60000, 80,0.01)
designs[[16]] = c(50000, 120,0.01)
designs[[17]] = c(40000, 120,0.01)
designs[[18]] = c(60000, 120,0.01)

# create designmatrix
test <- citypop
library(stringr)
test[,c("City", "State")] <- str_split_fixed(test$City, " - ", 2)
test$population <- test$totalAccidents <- NULL

#run function and save results in design matrix
costs <- c()
for(i in 1:length(designs)){
  print(i)
  final <- sensitivityanalysis(popmin = designs[[i]][1], distmax = designs[[i]][2], noncovmax = designs[[i]][3])
  result <- final[[1]]
  test <-cbind(test, result)
  colnames(test)[3+i] <- paste("design",i)
  costs[i] <- final[[2]]
}


write.csv(test,"H:/R/groupwork/FINAL ASSIGNMENT//designmatrix2.csv")
write.csv(costs,"H:/R/groupwork/FINAL ASSIGNMENT//costs2.csv")


#################################### only runned once ########################
## save initial solution
sugg<-matrix(1,9,habitantcities)
sugg<-as.data.frame(sugg)

for(i in 1:(nrow(sugg)-1)){
  sugg[i,]<- standardmodel$population[i,]
}

write.csv(sugg,"H:/R/groupwork/FINAL ASSIGNMENT//initialss.csv")

## determine centroids of occurred accidents per city in order to use these as coordinates to possibly build hospitals

avgLatLon <- as.data.frame(matrix(0, nrow=nrow(unique(DM["City"])), ncol = 3))
colnames(avgLatLon) <- c("City","Longitude", "Latitude")

for(i in 1:nrow(unique(DM["City"]))){
  avgLatLon[i,"City"] <- as.character(unique(DM["City"])[i,])
  df <- subset(DM, City == as.character(unique(DM["City"])[i,]))
  avgLatLon[i,"Latitude"] <- last(centroid(geojson_list(input = df, lat = "Start_Lat", lon = "Start_Lng")))
  avgLatLon[i,"Longitude"] <- first(centroid(geojson_list(input = df, lat = "Start_Lat", lon = "Start_Lng")))
}
write.csv(avgLatLon,"H:/R/groupwork/FINAL ASSIGNMENT//centroidss.csv")

################
