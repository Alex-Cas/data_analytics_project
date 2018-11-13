library(stringr)
library(readr)
library(ggplot2)
library(plyr)
library(tidyverse)
library(dplyr)
library(scales)
library(lubridate)
library(readxl)


########################
#### INITIALIZATION ####
logs <- read_delim("../data/logs.csv", 
                   ";", escape_double = FALSE, locale = locale(encoding = "ASCII"), 
                   trim_ws = TRUE)

survey <- read_excel("../data/surveydataece.xlsx" )

# Special characters
logs$User <- gsub("\u000e" , "\351" ,logs$User)
logs$User <- gsub("\003" , "\311" ,logs$User)
logs$User <- gsub("\017" , "\350" ,logs$User)
logs$User <- gsub("\021" , "\353" ,logs$User)


#### INITIALIZATION ####
########################



########################
##### PRECOMPUTING #####

# Aggregate by type
logsTemp <- logs
logsTemp$Count = 1

logsByType <- aggregate(list(Count = logsTemp$Count), list(User = logsTemp$User, Type = logsTemp$Type), FUN = sum)
logsByType <- logsByType[order(logsByType$User),]

logsByType$User

TypeList<-unique(logsTemp$Type)
d.uniqueUsers <- unique(logs$User)

##### PRECOMPUTING #####
########################



########################
####### HELPERS ########

h_toHour <- function(d) {
  
  d["Time"] <- lapply(d["Time"], function(x) {dmy_hm(x, tz=Sys.timezone())})
  d["Time"] <- lapply(d["Time"], function(x) {as.integer(format(x, format="%H"))})
  
  return (d)
}

h_toDay <- function(d) {
  
  d["Time"] <- lapply(d["Time"], function(x) {dmy_hm(x, tz=Sys.timezone())})
  d["Time"] <- lapply(d["Time"], function(x) {format(x, format="%Y-%m-%d")})
  
  return (d)
}

h_toUnix <- function(d) {
  
  d["Time"] <- lapply(d["Time"], function(x) {dmy_hm(x, tz=Sys.timezone())})
  d["Time"] <- lapply(d["Time"], function(x) {format(x, format="%Y-%m-%d")})
  d["Time"] <- lapply(d["Time"], function(x) {as.numeric(as.POSIXct(x, format="%Y-%m-%d"))})
  
  return (d)
}

####### HELPERS ########
########################



########################
##### DATA ACCESS  #####


# Piechart of mode frequency
d.getModeFrequencyPie <- function(user) {
  
  data <- logsByType[logsByType$User == user,]
  bp <- ggplot(data, aes(x="", y=Count, fill=Type)) + geom_bar(width= 1, stat="identity")
  pie <- bp + coord_polar("y", start=0)
  return (pie)
}
d.getModeFrequencyPie_All <- function() {
  
  data <- aggregate(list(Count = logsByType$Count), list(Type = logsByType$Type), FUN = sum)
  bp <- ggplot(data, aes(x="", y=Count, fill=Type)) + geom_bar(width= 1, stat="identity")
  pie <- bp + coord_polar("y", start=0)
  return (pie)
}


# Hourly usage barchart
d.getDayUsageBar <- function(user, typeList) {
  
  data <- logs[logs$User == user,]
  data <- data[data$Type %in% typeList,]

  # Time in hour
  data <- h_toHour(data)
  
  data <- rename(count(data, data$Type, data$Time), Type = "data$Type", Time = "data$Time")
  
  g <- ggplot(data, aes(Time, n, fill=Type))
  bar <- g + geom_bar(stat="identity", width = 0.5) +
    labs(title="Total hourly usage") +
    theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
    scale_x_continuous(breaks=c(1:23), limits=c(0.5, 23.5), expand=c(0, 0)) +
    scale_y_continuous(expand=c(0, 0))

  return (bar)
}
d.getDayUsageBar_All <- function(typeList) {
  
  data <- logs
  data <- data[data$Type %in% typeList,]
  
  # Time in hour
  data <- h_toHour(data)
  
  data <- rename(count(data, data$Type, data$Time), Type = "data$Type", Time = "data$Time")
  
  g <- ggplot(data, aes(Time, n, fill=Type))
  bar <- g + geom_bar(stat="identity", width = 0.5) +
    labs(title="Total hourly usage for all users") +
    theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
    scale_x_continuous(breaks=c(1:23), limits=c(0.5, 23.5), expand=c(0, 0)) +
    scale_y_continuous(expand=c(0, 0))
  
  return (bar)
}

#Test
d.getDayUsageRatio <- function(user) {
  
  data <- logs[logs$User == user,]
  
  # Time in hour
  data <- h_toHour(data)
  
  g <- ggplot(data, aes(x=Time, y=1, fill=Type))
  bar <- g + geom_bar(stat="identity", width = 1, position=position_fill()) +
    labs(title="Hourly ratio") +
    theme(axis.text.x = element_text(angle=65, vjust=0.6),
          axis.text.y = element_blank()) +
    scale_x_continuous(breaks=c(1:23), limits=c(0.5, 23.5), expand=c(0, 0)) +
    scale_y_discrete(labels=c(0, 1), breaks=c(0, 1), expand=c(0, 0))

  return (bar)
}
d.getDayUsageRatio_All <- function() {
  
  data <- logs
  
  # Time in hour
  data <- h_toHour(data)
  
  g <- ggplot(data, aes(x=Time, y=1, fill=Type))
  bar <- g + geom_bar(stat="identity", width = 1, position=position_fill()) +
    labs(title="Hourly ratio for all users") +
    theme(axis.text.x = element_text(angle=65, vjust=0.6),
          axis.text.y = element_blank()) +
    scale_x_continuous(breaks=c(1:23), limits=c(0.5, 23.5), expand=c(0, 0)) +
    scale_y_discrete(labels=c(0, 1), breaks=c(0, 1), expand=c(0, 0))
  
  return (bar)
}

##### DATA ACCESS  #####
########################




# workspace fourre tout



#data <- h_toDay(data)


#data <- logs[logs$User == "Renaud Courbis",]


#data["Time"] <- lapply(data["Time"], function(x) {dmy_hm(x, tz=Sys.timezone())})
#data["Time"] <- lapply(data["Time"], function(x) {format(x, format="%Y-%m-%d")})
#data["Time"] <- lapply(data["Time"], function(x) {as.numeric(as.POSIXct(x, format="%Y-%m-%d"))})
#data <- rename(count(data, data$Type, data$Time), Type = "data$Type", Time = "data$Time")
#data



#g <- ggplot(data, aes(Time, n, color=Type))
#g

#bar <- g + geom_line(size=1) + stat_summary(fun.y = "sum",aes(Time,n,col = "sum"), size = 1, geom = "line") 
#bar

#bar <- g + geom_bar(stat="identity", width = 0.5) +
#  labs(title="Total hourly usage") +
#  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#bar




#moyenne age
#nombre de cigarette par user ? 

#library(ggmap)
#install.packages("leaflet", dependencies = TRUE)

library(leaflet)

#lyon=c(lon=4.842223 ,lat=45.759723 )
#lyon_map=get_map(location=lyon)
#ggmap(lyon_map)

#ggmap(lyon_map_3)+
#  geom_point(data=data_sp, 
#             aes(x=lon,y=lat), col="red", size=2)

# Définition d'un nouvel icône
mapUser <- function(user,typeList){
#Smoking <- makeIcon(
  #iconUrl = "smoking.png",
  #iconWidth = 40, iconHeight = 40)
  data <- logs[logs$User == user,]
  data <- data[data$Type %in% typeList,]
  m <- leaflet() %>%
  addTiles() %>%
 # setView(lng = 2.292551, lat = 48.858255, zoom = 16) %>%
  # addMarkers(lng = data$Longitude, lat = data$Latitude , color="red")
  addCircles(lng = data$Longitude, lat = data$Latitude , color="red",fill = TRUE, opacity = 0.3 , weight=12 ,fillColor = "red",
             fillOpacity = 0.2)
  
return(m)
}

mapAll <- function(typeList){
  #Smoking <- makeIcon(
  #iconUrl = "smoking.png",
  #iconWidth = 40, iconHeight = 40)
  data <- logs
  data <- data[data$Type %in% typeList,]
  m <- leaflet() %>%
    addTiles() %>%
    # setView(lng = 2.292551, lat = 48.858255, zoom = 16) %>%
    # addMarkers(lng = data$Longitude, lat = data$Latitude , color="red")
    addCircles(lng = data$Longitude, lat = data$Latitude , fill = TRUE,color="red" ,opacity = 0.3, weight=12,fillColor = "red",
               fillOpacity = 0.2)
  
  return(m)
}




userName <-unique(logs$User)
surveyName <- survey$Name



UserAndSurvey<-surveyName[surveyName %in% userName]

dataSurveyUser <-survey[survey$Name %in% UserAndSurvey,]



#table recap
#Proportion Homme femme
#proposer un recap par user
#


names(dataSurveyUser)


AgeAverage <- function(){
  AgeAvg <-round(mean(dataSurveyUser$Age))
  return(AgeAvg)
}

getAge <- function(user){
  data<-dataSurveyUser[dataSurveyUser$Name==user,]
  if (nrow(data) == 0) {Age <- "Unknown"}
  else{Age <-data$Age}
  
  return(Age)
}

getGender <- function(user){
  data<-dataSurveyUser[dataSurveyUser$Name==user,]
  if (nrow(data) == 0) {
    gender<-"Unknown"
  } else {
    gender <-data$Gender
  }
  return(gender)
}

getFamily <- function(user){
  data<-dataSurveyUser[dataSurveyUser$Name==user,]
  if (nrow(data) == 0) {
    family<-"Unknown"
  } else {
    family <-data$`Family status`
  }
  return(family)
}


getStarted <- function(user){
  data<-dataSurveyUser[dataSurveyUser$Name==user,]
  if (nrow(data) == 0) {
    started<-"Unknown"
  } else {
    started <-data$Started
  }
  return(started)
}

# Trend
d.getTrend <- function(user, typeList) {
  
  data <- logs[logs$User == user,]
  data <- data[data$Type %in% typeList,]
  
  data <- h_toUnix(data)
  
  data <- rename(count(data, data$Type, data$Time), Type = "data$Type", Time = "data$Time")
  
  g <- ggplot(data, aes(Time, n, color=Type))
  
  bar <- g + geom_line(size=1) +
    stat_summary(fun.y = "sum",aes(Time,n,col = "Total"), size = 1, geom = "line")
  
  return (bar)
}



# Reason per status
d.getReasonPerStatus <- function() {
  
  data <- cbind(survey[, 51:56], survey[, 9])
  
  dataSingle <- data[data$`Family status` == "Single", 1:6]
  dataMarried <- data[data$`Family status` == "Married", 1:6]
  
  dataSingle <- colSums(!is.na(dataSingle))
  dataSingle <- data.frame(dataSingle)
  dataSingle["Status"] <- "Single"
  dataSingle <- setNames(cbind(rownames(dataSingle), dataSingle, row.names = NULL), 
                         c("Reason", "Percentage", "Status"))
  dataSingle$Percentage <- dataSingle$Percentage / sum(dataSingle$Percentage) * 100
  
  
  dataMarried <- colSums(!is.na(dataMarried))
  dataMarried <- data.frame(dataMarried)
  dataMarried["Status"] <- "Married"
  dataMarried <- setNames(cbind(rownames(dataMarried), dataMarried, row.names = NULL), 
                          c("Reason", "Percentage", "Status"))
  dataMarried$Percentage <- dataMarried$Percentage / sum(dataMarried$Percentage) * 100
  
  
  data <- rbind(dataSingle, dataMarried)
  
  plot <- ggplot(data, aes(x = Reason, y=Percentage, fill = Status)) +
    geom_bar(data=subset(data, Status=="Single"), stat="identity") + 
    geom_bar(data=subset(data, Status=="Married"), stat="identity", aes(y=Percentage*(-1))) + 
    scale_y_continuous(
      breaks = seq(-50, 50, 5), 
      labels = as.character(c(seq(50, 0, -5), seq(5, 50, 5)))) +
    coord_flip() +
    labs(title="Reason to quit per family status") +
    theme(
      plot.title = element_text(hjust = .5), 
      axis.ticks = element_blank()) +
    scale_fill_brewer(palette = "Dark2")
  
  return (plot)
}







data <- cbind(survey[, 51:56], survey[, 9])

dataSingle <- data[data$`Family status` == "Single", 1:6]
dataMarried <- data[data$`Family status` == "Married", 1:6]

dataSingle <- colSums(!is.na(dataSingle))
dataSingle <- data.frame(dataSingle)
dataSingle["Status"] <- "Single"
dataSingle <- setNames(cbind(rownames(dataSingle), dataSingle, row.names = NULL), 
                       c("Reason", "Count", "Status"))
dataSingle$Count <- dataSingle$Count / sum(dataSingle$Count) * 100


dataMarried <- colSums(!is.na(dataMarried))
dataMarried <- data.frame(dataMarried)
dataMarried["Status"] <- "Married"
dataMarried <- setNames(cbind(rownames(dataMarried), dataMarried, row.names = NULL), 
                        c("Reason", "Count", "Status"))
dataMarried$Count <- dataMarried$Count / sum(dataMarried$Count) * 100

data <- rbind(dataSingle, dataMarried)

plot <- ggplot(data, aes(x = Reason, y=Count, fill = Status)) +
  geom_bar(data=subset(data, Status=="Single"), stat="identity") + 
  geom_bar(data=subset(data, Status=="Married"), stat="identity", aes(y=Count*(-1))) + 
  scale_y_continuous(
    breaks = seq(-50, 50, 5), 
    labels = as.character(c(seq(50, 0, -5), seq(5, 50, 5)))) +
  coord_flip() +
  labs(title="Reason to quit per family status") +
  theme(
    plot.title = element_text(hjust = .5), 
    axis.ticks = element_blank()) +
  scale_fill_brewer(palette = "Dark2")

plot



