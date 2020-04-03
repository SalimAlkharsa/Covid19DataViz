#============Load DF for the General Overview
df <- read.csv("df.csv", header = TRUE)
#Fixing errors in the df
df[8, 6] = 1089.9
df[2, 6] = 827.7
#Libraries
library(tidyverse)
library(viridis)
library(mapdata)
library(rvest)
library(ggplot2)
library (httr) 
library (jsonlite)
library(plotly)
#======Data is Good Until March 22=======#
# Age
ggplot(df, aes(x = Median.Country.Age, y = Serious.Cases..)) +
  geom_bar(stat = "identity", width=0.4) +
  facet_wrap(~Class.Of.Testing..A.is.best.) + 
  ggtitle("Serious Cases By Age and Testing Intensity") +
  xlab("Median Age") +
  ylab("Serious case%")+
  xlim(35,50)+
  ylim(0,0.125)
#Smokers
ggplot(df, aes(x = Smokers.Capita, y = Serious.Cases..)) +
  geom_bar(stat = "identity", width=10) +
  facet_wrap(~Class.Of.Testing..A.is.best.) + 
  ggtitle("Serious Cases By Smoking and Testing Intensity") +
  xlab("Smokers Per Capita") +
  ylab("Serious case%")+
  xlim(800,2000)+
  ylim(0,0.125)
#Location Just for fun ###fix it more
world <- map_data("world")
lon <- c(126.3720,-1.522559,5.857520,-123.1696,9.524023,3.350097,6.627735,-4.762500,-9.235645)
lat <- c(33.22363,55.25952,51.03012,48.58672,47.52422,51.37769,45.11797,48.45024,43.03579)
df$lon <- lon
df$lat <- lat
ggplot(df, aes(x=lon, y=lat)) + 
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.5) +
  geom_bin2d(bins=100) +
  theme_void() +
  ggtitle("% Serious Wuhan Virus Cases By Country")+
  scale_fill_viridis(name= "% Serious Wuhan Virus Cases By Country")
#======New State by State Data=======#
#Web-Scrapping For State Data
url <- "https://covidtracking.com/api/states/daily"
df.state <- fromJSON(url) %>% as.data.frame
#Creating a graph on case increases
ggplot(df.state, aes(x = date, y = positive), y= "log") +
  geom_bar(stat = "identity", width=0.4) +
  facet_wrap(~state) + 
  ggtitle("Increases in Cases") +
  xlab("Date") +
  ylab("Cases")
#Creating a graph on testing increases
ggplot(df.state, aes(x = date, y = totalTestResultsIncrease)) +
  geom_bar(stat = "identity", width=0.4) +
  facet_wrap(~state) + 
  ggtitle("Increases in Testing") +
  xlab("Date") +
  ylab("Tests")
#========================For Tracking Growth rate
url.2 <- "https://covidtracking.com/api/us/daily"
us.df <- fromJSON(url.2) %>% as.data.frame
us.df$Day.Metric <- c(27:1) #update the left to account for time
us.df$Rate.of.Infection <- c(us.df$positive/us.df$Day.Metric)
#Removing dumb metrics
us.df$date <- NULL
us.df$states <- NULL
us.df$hash <- NULL
us.df$dateChecked <- NULL
#Graph US Rate of Growth
#Using Germany bc Germany is apparently doing really well
#Link for updating https://tradingeconomics.com/germany/coronavirus-cases
germany.df <- data.frame("Day.Metric" = (1:16), "positive" = c(4599,5813,7272,9367,12327,15320,19848,
                                                               22364,24873,29056,32991,37323,43938,50871,
                                                               56202,58247))
germany.df$Rate.of.Infection <- c(germany.df$positive/germany.df$Day.Metric)
#Plotting Germany and US Rate.of.Infection
us.plotly <- us.df %>%
  ggplot( aes(x = Day.Metric, y = Rate.of.Infection)) +
  geom_area(fill="navyblue", alpha=0.5) +
  geom_line(color="navyblue") +
  ylab("Rate of Growth") +
  xlab("Days Since Case 1")+
  ggtitle("Rate of growth of cases in the US")+
  xlim(0,25)+
  ylim(0,4000)
us.plotly <- ggplotly(us.plotly)
us.plotly
germany.plotly <- germany.df %>%
  ggplot(aes(x= Day.Metric, y= Rate.of.Infection)) +
  geom_area(fill="red4", alpha=0.5) +
  geom_line(color="red4") +
  ylab("Rate of Growth") +
  xlab("Days Since Case 1")+
  ggtitle("Rate of growth of cases in Germany")+
  xlim(0,25)+
  ylim(0,4000)
germany.plotly <- ggplotly(germany.plotly)
germany.plotly
#=================Scraping for stats for states and plotting them
#Web-Scrapping For State Data
url <- "https://covidtracking.com/api/states/daily"
df.state <- fromJSON(url) %>% as.data.frame
  #Creating a Texas df
  tx.df <- df.state[which(df.state$state=="TX"),]
  tx.df$Day.Metric <- c(27:1) #update the left to account for time
  tx.df$Rate.of.Infection <- c(tx.df$positive/tx.df$Day.Metric)
  #Removing dumb metrics
  tx.df$date <- NULL
  tx.df$states <- NULL
  tx.df$hash <- NULL
  tx.df$dateChecked <- NULL
  #Creating a NY df
  ny.df <- df.state[which(df.state$state=="NY"),]
  ny.df$Day.Metric <- c(27:1) #update the left to account for time
  ny.df$Rate.of.Infection <- c(ny.df$positive/ny.df$Day.Metric)
  #Removing dumb metrics
  ny.df$date <- NULL
  ny.df$states <- NULL
  ny.df$hash <- NULL
  ny.df$dateChecked <- NULL
#Merging the dfs for a plot
tx.ny.df <- rbind(ny.df,tx.df)
#Plotting them together
  #Rate of infection
  tx.ny.df %>%
    ggplot( aes(x=Day.Metric, y=Rate.of.Infection, group=state, color=state)) +
    geom_line() +
    scale_color_viridis(discrete = TRUE) +
    ggtitle("Rate of Infection Across States in the US") +
    ylab("Rate of Infection")+
    xlab("Days Since Case 1")+
    xlim(0,30)+
    ylim(0,2500)
#============Doing the analysis on many states
#Loading state stuff
url <- "https://covidtracking.com/api/states/daily"
df.state <- fromJSON(url) %>% as.data.frame
#Editing DF
df.state$date <- NULL
df.state$states <- NULL
df.state$hash <- NULL
df.state$dateChecked <- NULL
  #Creating a Texas df
  tx.df <- df.state[which(df.state$state=="TX"),]
  tx.df$Day.Metric <- c(nrow(tx.df):1) 
  #Creating a NY df
  ny.df <- df.state[which(df.state$state=="NY"),]
  ny.df$Day.Metric <- c(nrow(ny.df):1) 
  #Creating a Michigan df
  mi.df <- df.state[which(df.state$state=="MI"),]
  mi.df$Day.Metric <- c(nrow(mi.df):1) 
  #Creating a Was df
  wa.df <- df.state[which(df.state$state=="WA"),]
  wa.df$Day.Metric <- c(nrow(wa.df):1) 
  #Creating a California df
  ca.df <- df.state[which(df.state$state=="CA"),]
  ca.df$Day.Metric <- c(nrow(ca.df):1) 
  #Creating a Lousiana df
  la.df <- df.state[which(df.state$state=="LA"),]
  la.df$Day.Metric <- c(nrow(la.df):1) 
  #Creating a NewJersey df
  nj.df <- df.state[which(df.state$state=="NJ"),]
  nj.df$Day.Metric <- c(nrow(nj.df):1) 
  #Creating a Florida df
  fl.df <- df.state[which(df.state$state=="FL"),]
  fl.df$Day.Metric <- c(nrow(fl.df):1) 
  #Creating a Georgia df
  ga.df <- df.state[which(df.state$state=="GA"),]
  ga.df$Day.Metric <- c(nrow(ga.df):1) 
  #Creating an Ohio df
  oh.df <- df.state[which(df.state$state=="OH"),]
  oh.df$Day.Metric <- c(nrow(oh.df):1) 
  #Creating an Pen df
  pa.df <- df.state[which(df.state$state=="PA"),]
  pa.df$Day.Metric <- c(nrow(pa.df):1) 
  #Creating an Indiana df
  in.df <- df.state[which(df.state$state=="IN"),]
  in.df$Day.Metric <- c(nrow(in.df):1) 
  #Creating an Mass. df
  ma.df <- df.state[which(df.state$state=="MA"),]
  ma.df$Day.Metric <- c(nrow(ma.df):1) 
  #Creating an Illnois df
  il.df <- df.state[which(df.state$state=="IL"),]
  il.df$Day.Metric <- c(nrow(il.df):1) 
#Binding them
keystates.df <- rbind(ny.df,tx.df,ca.df,wa.df,mi.df,la.df,nj.df,fl.df,ga.df,oh.df,pa.df,in.df,ma.df,il.df)
  #Plotting them together
  #New Cases
  plotly.6 <- keystates.df %>%
    ggplot( aes(x=Day.Metric, y=positive, group=state, color=state)) +
    geom_line(size=1) +
    scale_color_viridis(discrete = TRUE) +
    ggtitle("Cases in the US") +
    ylab("Cases")+
    xlab("Days Since Case 1")+
    xlim(1,30)+
    ylim(-0.3,90000)
  plotly.6 <- ggplotly(plotly.6)
  plotly.6
#==============Foccusing on Vent. and Hospitalizations
  
