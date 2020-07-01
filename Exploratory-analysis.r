##################
#Group 14        #
#Willem Himpe    #
#Willem Lannoye  #
#Axelle Sels     #
#Thibault Verduyn#
##################

rm(list=ls())
library(ggplot2)
library(dplyr)
library(scales)
library(extrafont)
library(ggthemes)

#Load data
setwd(choose.dir())
DF <- read.csv("futureAccidentDATA.csv")
uscities <- read.csv("uscities.csv")
Crashcities <- merge(DF, uscities[,c('city', 'state_id', 'population', 'county_name')], by.x = c('City', 'State'), by.y= c('city', 'state_id'))
data <- Crashcities

#SEVERITY (total & over time)
df <- as.data.frame(table(data$Severity))
p <- ggplot(df,aes(x=Var1,y=Freq)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_minimal() +
  xlab("Severity") + 
  ylab("") + 
  ggtitle("Severity of accidents") +
  scale_y_continuous(labels = comma)
p + theme(text=element_text(size=16,  family="Clear Sans", colour = "#2a383f", face="bold")) +
  theme(plot.title = element_text(hjust = 0.5, colour = "#2a383f")) +
  theme(axis.text.x = element_text(colour = "#2a383f")) +
  scale_color_manual(values = c(rep("#2a383f", 4)))

df$ratio <- df$Freq/sum(df$Freq)

#SPEED BUMPS
df <- as.data.frame(table(DF$Bump))
df$Var1 <- gsub('False', 'No', df$Var1)
df$Var1 <- gsub('True', 'Yes', df$Var1)
df$Percentage <- NULL
df$Percentage <- paste(as.character(round((df$Freq/sum(df$Freq))*100,2)),"%")

p <- ggplot(df,aes(x=Var1,y=Freq)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_minimal() +
  ggtitle("Speed bump present") +
  xlab("") + 
  ylab("") + 
  scale_y_continuous(labels = comma) +
  geom_text(aes(label=Percentage), vjust=-0.3, color="#2a383f",size=5, family = "Clear Sans", fontface = "bold")
p + theme(text=element_text(size=16,  family="Clear Sans", colour = "#2a383f", face="bold")) +
  theme(plot.title = element_text(hjust = 0.5, colour = "#2a383f")) +
  theme(axis.text.x = element_text(colour = "#2a383f")) +
  scale_color_manual(values = c(rep("#2a383f", 2)))

#ACCIDENTS PER STATE
df <- as.data.frame(table(data$State))
df <- df %>% arrange(desc(Freq)) %>%  slice(1:5) 
#plot
p <- ggplot(df,aes(x=reorder(Var1, -Freq),y=Freq)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_minimal() +
  xlab("") + 
  ylab("") + 
  ggtitle("States with most accidents") +
  scale_y_continuous(labels = comma)
p + theme(text=element_text(size=16,  family="Clear Sans", colour = "#2a383f", face="bold")) +
  theme(plot.title = element_text(hjust = 0.5, colour = "#2a383f")) +
  theme(axis.text.x = element_text(colour = "#2a383f")) +
  scale_color_manual(values = c(rep("#2a383f", 5)))

#ACCIDENTS PER CITY
df <- aggregate(rep(1, nrow(data)), by = list(x = data$City, y = data$State), sum)
colnames(df) <- c("City","State","Freq")
df$City <- paste(df$City, df$State)
df$State <- NULL
df <- df %>% arrange(desc(Freq)) %>%  slice(1:5) 
#plot
p <- ggplot(df,aes(x=reorder(City,-Freq),y=Freq)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_minimal() +
  xlab("") + 
  ylab("") + 
  ggtitle("Cities with most accidents") +
  scale_y_continuous(labels = comma)
p + theme(text=element_text(size=16,  family="Clear Sans", colour = "#2a383f", face="bold")) +
  theme(plot.title = element_text(hjust = 0.5, colour = "#2a383f")) +
  theme(axis.text.x = element_text(colour = "#2a383f")) +
  scale_color_manual(values = c(rep("#2a383f", 5)))


#HIGHEST ACCIDENTS PER POPULATION
df <- aggregate(rep(1, nrow(data)), by = list(x = data$City, y = data$State), sum)
colnames(df) <- c("City","State","Freq")
df <- merge(df, uscities[,c('city', 'state_id', 'population')], by.x = c('City','State'), by.y= c('city','state_id'))
df <- subset(df, population > 50000)
df$ratio <- round(df$Freq/df$population,5)
df <- df %>% arrange(desc(ratio)) %>%  slice(1:5) 
df$City <- paste(df$City, df$State)
df$State <- NULL
#plot
p <- ggplot(df,aes(x=reorder(City, -ratio),y=ratio)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_minimal() +
  xlab("") + 
  ylab("") + 
  ggtitle("Most accidents per inhabitant") +
  scale_y_continuous(labels = comma)
p + theme(text=element_text(size=16,  family="Clear Sans", colour = "#2a383f", face="bold")) +
  theme(plot.title = element_text(hjust = 0.5, colour = "#2a383f")) +
  theme(axis.text.x = element_text(colour = "#2a383f")) +
  scale_color_manual(values = c(rep("#2a383f", 5)))



