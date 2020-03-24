  
library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)
library(syuzhet)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(forcats)

url <- paste("https://www.infoplease.com/us/us-cities/top-50-cities-us-population-and-rank",
    sep="")
html <- read_html(url) # reading the html code into memory
html
substr(html_text(html), 1, 1000)
tab <- html_table(html)
str(tab)
city <- tab[[1]]
city
names(city)[which(names(city)=="SIZE RANK 2014")] <- "rank"
names(city)[which(names(city)=="7/1/2014POPULATION ESTIMATE")] <- "14pop"
names(city)[which(names(city)=="7/1/2013POPULATION ESTIMATE")] <- "13pop"
names(city)[which(names(city)=="4/1/2010CENSUS POPULATION")] <- "10pop"
names(city)[which(names(city)=="7/1/2005POPULATION ESTIMATE")] <- "05pop"
names(city)[which(names(city)=="4/1/2000CENSUS POPULATION")] <- "00pop"
names(city)[which(names(city)=="4/1/1990CENSUS POPULATION")] <- "90pop"
city

city$City <- gsub('\\[.*\\]', '', city$CITY)
city$population <- as.numeric(gsub(",", "", city[,"14pop"]))
city$rank <- city[,"rank"]
head(city[,c("City", "population", "rank")])



library(ggplot2)
p <- ggplot(city, aes(x=rank, y=population, label=City))
p
pq <- p + geom_point() + geom_text(hjust=-.1, size=3) +
    scale_x_log10("log(rank)") + scale_y_log10("log(population)")
pq



city$City <- gsub('\\[.*\\]', '', city$CITY)
city$pop<- as.numeric(gsub(",", "", city[,"13pop"]))
head(city[,c("City", "13pop")])
top <- head(city[,c("City", "13pop")],num=10L)
top



ggplot(top, aes(x=as.factor(top$City), y= top$`13pop`)) + 
  geom_point(aes(color = factor(top$City)), size = 6)+
  ggtitle("13 top6 cities") +
  ylab("top$13pop") +
  xlab("top$city") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())



city$City <- gsub('\\[.*\\]', '', city$CITY)
city$population <- as.numeric(gsub(",", "", city[,"14pop"]))
city$rank <- city[,"rank"]
head(city[,c("City", "population", "rank")])
head <- head(city[,c("City", "population", "rank")],num=10L)

ggplot(top, aes(x=as.factor(head$City), y= head$`population`)) + 
  geom_point(aes(color = factor(head$City)), size = 6)+
  ggtitle("14 top6 cities") +
  ylab("head$population") +
  xlab("head$city") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

