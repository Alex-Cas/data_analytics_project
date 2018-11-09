library(stringr)
library(readr)
library(ggplot2)
library(plyr)
library(tidyverse)
library(dplyr)
library(scales)
library(lubridate)



########################
#### INITIALIZATION ####
logs <- read_delim("../data/logs.csv", 
                   ";", escape_double = FALSE, locale = locale(encoding = "ASCII"), 
                   trim_ws = TRUE)

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

##### DATA ACCESS  #####
########################




# workspace fourre tout


# data2 <- data
# data2 <- aggregate(data2$n, by=list(Time = data2$Time), FUN=sum)
# data2["Type"] = "Total"
# data2 <- rename(data2, "n" = "x")
# 
# da <- rbind(data, data2)
# da["isTotal"] <- with(da, ifelse(Type == "Total", 0, 1))
