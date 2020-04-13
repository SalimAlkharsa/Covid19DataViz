#Libraries
library(tidyverse)
library(viridis)
library(mapdata)
library(rvest)
library(ggplot2)
library (httr) 
library (jsonlite)
library(plotly)
library(formattable)
#Loading state stuff
url <- "https://covidtracking.com/api/states/daily"
df.state <- fromJSON(url) %>% as.data.frame
#Editing DF
df.state$date <- NULL
df.state$states <- NULL
df.state$hash <- NULL
df.state$dateChecked <- NULL
df.state$posNeg <- NULL
df.state$fips <- NULL
df.state$pending <- NULL
df.state$total <- NULL
df.state$negative <- NULL
df.state$hospitalizedCumulative <- NULL
df.state$inIcuCumulative <- NULL
df.state$onVentilatorCumulative <- NULL
df.state$hospitalized <- NULL
df.state$negativeIncrease <- NULL
df.state <- df.state %>% 
  rename(
    State = state,
    Currently.Hospitalized = hospitalizedCurrently,
    Currently.In.ICU = inIcuCurrently,
    Currently.On.Ventilator = onVentilatorCurrently,
    Current.Cases = positive,
    Recovered = recovered,
    Deaths = death,
    Increase.In.Deaths = deathIncrease,
    Increase.In.Cases = positiveIncrease,
    Increase.In.Testing = totalTestResultsIncrease,
    Total.Test.Results = totalTestResults,
    Increase.In.Hospitalizations = hospitalizedIncrease
  )
  #Creating a Texas df
  tx.df <- df.state[which(df.state$State=="TX"),]
  tx.df$Day.Metric <- c(nrow(tx.df):1) 
  tx.df <- subset(tx.df, select=c(Day.Metric,State:Increase.In.Testing))
  #Creating a NY df
  ny.df <- df.state[which(df.state$State=="NY"),]
  ny.df$Day.Metric <- c(nrow(ny.df):1) 
  ny.df <- subset(ny.df, select=c(Day.Metric,State:Increase.In.Testing))
  #Creating a Michigan df
  mi.df <- df.state[which(df.state$State=="MI"),]
  mi.df$Day.Metric <- c(nrow(mi.df):1) 
  mi.df <- subset(mi.df, select=c(Day.Metric,State:Increase.In.Testing))
  #Creating a Was df
  wa.df <- df.state[which(df.state$State=="WA"),]
  wa.df$Day.Metric <- c(nrow(wa.df):1) 
  wa.df <- subset(wa.df, select=c(Day.Metric,State:Increase.In.Testing))
  #Creating a California df
  ca.df <- df.state[which(df.state$State=="CA"),]
  ca.df$Day.Metric <- c(nrow(ca.df):1) 
  ca.df <- subset(ca.df, select=c(Day.Metric,State:Increase.In.Testing))
  #Creating a Lousiana df
  la.df <- df.state[which(df.state$State=="LA"),]
  la.df$Day.Metric <- c(nrow(la.df):1) 
  la.df <- subset(la.df, select=c(Day.Metric,State:Increase.In.Testing))
  #Creating a NewJersey df
  nj.df <- df.state[which(df.state$State=="NJ"),]
  nj.df$Day.Metric <- c(nrow(nj.df):1) 
  nj.df <- subset(nj.df, select=c(Day.Metric,State:Increase.In.Testing))
  #Creating a Florida df
  fl.df <- df.state[which(df.state$State=="FL"),]
  fl.df$Day.Metric <- c(nrow(fl.df):1) 
  fl.df <- subset(fl.df, select=c(Day.Metric,State:Increase.In.Testing))
  #Creating a Georgia df
  ga.df <- df.state[which(df.state$State=="GA"),]
  ga.df$Day.Metric <- c(nrow(ga.df):1) 
  ga.df <- subset(ga.df, select=c(Day.Metric,State:Increase.In.Testing))
  #Creating an Ohio df
  oh.df <- df.state[which(df.state$State=="OH"),]
  oh.df$Day.Metric <- c(nrow(oh.df):1) 
  oh.df <- subset(oh.df, select=c(Day.Metric,State:Increase.In.Testing))
  #Creating an Pen df
  pa.df <- df.state[which(df.state$State=="PA"),]
  pa.df$Day.Metric <- c(nrow(pa.df):1) 
  pa.df <- subset(pa.df, select=c(Day.Metric,State:Increase.In.Testing))
  #Creating an Indiana df
  in.df <- df.state[which(df.state$State=="IN"),]
  in.df$Day.Metric <- c(nrow(in.df):1) 
  in.df <- subset(in.df, select=c(Day.Metric,State:Increase.In.Testing))
  #Creating an Mass. df
  ma.df <- df.state[which(df.state$State=="MA"),]
  ma.df$Day.Metric <- c(nrow(ma.df):1) 
  ma.df <- subset(ma.df, select=c(Day.Metric,State:Increase.In.Testing))
  #Creating an Illnois df
  il.df <- df.state[which(df.state$State=="IL"),]
  il.df$Day.Metric <- c(nrow(il.df):1) 
  il.df <- subset(il.df, select=c(Day.Metric,State:Increase.In.Testing))
#Binding them
keystates.df <- rbind(ny.df,tx.df,ca.df,wa.df,mi.df,la.df,nj.df,fl.df,ga.df,oh.df,pa.df,in.df,ma.df,il.df)
keystates.df <- keystates.df %>% arrange(desc(Day.Metric))
  #Plotting them together
  #Cases
  plotly.6 <- keystates.df %>%
    ggplot( aes(x=Day.Metric, y=Current.Cases, group=State, color=State)) +
    geom_line(size=1) +
    scale_color_viridis(discrete = TRUE) +
    ggtitle("Cases in the US") +
    ylab("Cases")+
    xlab("Days Since Case 1")+
    xlim(1,70)+
    ylim(-0.3,190000)
  plotly.6 <- ggplotly(plotly.6)
  plotly.6
#==============Creating a Visually Appealling Table for Cases
formattable(keystates.df, list(
  Current.Cases = color_tile("white", "orange"), Currently.Hospitalized = color_tile("white", "orange"),
  Currently.In.Icu = color_tile("white", "orange"), Currently.On.Ventilator = color_tile("white", "orange"),
  Recovered = color_tile("white", "green"), Deaths = color_tile("white", "red"),
  Total.Test.Results = color_tile("white", "green"), Increase.In.Hospitalizations = color_tile("white", "orange"),
  Increase.In.Testing = color_tile("white", "green"), Currently.In.ICU = color_tile("white", "orange"),
  Increase.In.Deaths = color_tile("white", "red"), Increase.In.Cases = color_tile("white", "orange")))
#==============Creating a Graph for Projections (under construction)
