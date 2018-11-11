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
#logs <- read_delim("../data/logs.csv", 
#                   ";", escape_double = FALSE, locale = locale(encoding = "ASCII"), 
#                   trim_ws = TRUE)

logs <- logs
survey <- surveydataece


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

# Reason per status
d.getReasonPerStatus <- function() {
  
  data <- cbind(survey[, 51:56], survey[, 9])
  
  dataSingle <- data[data$`Family status` == "Single", 1:6]
  dataMarried <- data[data$`Family status` == "Married", 1:6]
  
  dataSingle <- colSums(!is.na(dataSingle))
  dataSingle <- data.frame(dataSingle)
  dataSingle["Status"] <- "Single"
  dataSingle <- setNames(cbind(rownames(dataSingle), dataSingle, row.names = NULL), 
                    c("Reason", "Count", "Status"))
  
  dataMarried <- colSums(!is.na(dataMarried))
  dataMarried <- data.frame(dataMarried)
  dataMarried["Status"] <- "Married"
  dataMarried <- setNames(cbind(rownames(dataMarried), dataMarried, row.names = NULL), 
                     c("Reason", "Count", "Status"))
  
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
  
  return (plot)
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
# 
# data <- surveydataece
# data <- data[, 51:56]
# data
# 
# data2 <- colSums(!is.na(data))
# data3 <- data.frame(data2)
# 
# data3 <- setNames(cbind(rownames(data3), data3, row.names = NULL), 
#          c("Reason", "Value"))
# 
# data3
# 
# # , aes(x="", y=Count, fill=Type)
# bp <- ggplot(data3, aes(x="", y=Value, fill=Reason)) + geom_bar(width= 1, stat="identity")
# pie <- bp + coord_polar("y", start=0)
# pie
# 
# ##
# data4 <- surveydataece[, 9]
# data5 <- cbind(data, data4)
# data6 <- colSums(!is.na(data))
# data6
# 
# data7 <- data5[data5$`Family status` == "Single", 1:6]
# 
# data10 <- data5[data5$`Family status` == "Married", 1:6]
# 
# data8 <- colSums(!is.na(data7))
# data9 <- data.frame(data8)
# data9["Status"] <- "Single"
# data9 <- setNames(cbind(rownames(data9), data9, row.names = NULL), 
#                    c("Reason", "Count", "Status"))
# 
# 
# data11 <- colSums(!is.na(data10))
# data12 <- data.frame(data11)
# data12["Status"] <- "Married"
# data12 <- setNames(cbind(rownames(data12), data12, row.names = NULL), 
#                   c("Reason", "Count", "Status"))
# 
# data13 <- rbind(data9, data12)
# 
# data14 <- setNames(cbind(rownames(data13), data13, row.names = NULL), 
#                   c("Reason", "Count", "Status"))
# 
# # bp <- ggplot(data3, aes(x="", y=Value, fill=Reason)) + geom_bar(width= 1, stat="identity")
# # pie <- bp + coord_polar("y", start=0)
# # pie
# data13
# 
# ggplot(data13, aes(x = Reason, y=Count, fill = Status)) +
#   geom_bar(data=subset(data13, Status=="Single"), stat="identity") + 
#   geom_bar(data=subset(data13, Status=="Married"), stat="identity", aes(y=Count*(-1))) + 
#   scale_y_continuous(breaks = seq(-50, 50, 5), 
#                      labels = as.character(c(seq(50, 0, -5), seq(5, 50, 5)))) +
#   coord_flip() +
#   labs(title="Reason to quit per family status") +
#   theme(plot.title = element_text(hjust = .5), 
#         axis.ticks = element_blank()) +
#   scale_fill_brewer(palette = "Dark2")
