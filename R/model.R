# Library
library(randomForest)
library(dplyr)
library(purrr)
library(here)
library(broom)
library(lubridate)

source(here::here('R/varsel_regression_rf.R'))

##Reading in and creating data sets
nla<-read.csv(here::here('data/nla_base.csv'))
x<-complete.cases(nla)
nla.cc<-nla[x,]
nla.cc$year<-factor(nla.cc$year)
nla.cc$date <- lubridate::yday(lubridate::ymd(nla.cc$date))

nla.all <-  filter(nla.cc, tmean_2m < 70) %>%
              dplyr::select(tmean_2m, year,date, longitude, latitude, tmean,
              tmean_avg30,percent_impervious,surface_area,shoreline_length,
              shoreline_dev, max_length, max_width, mean_width, max_depth,
              max_depth, volume, elevation)

nla.all<-rename(nla.all,Date=date,Average.Temperature=tmean,Longitude=longitude,
                 Thirty.Day.Average.Temperature=tmean_avg30,Elevation=elevation,
                 Latitude=latitude,Surface.Area=surface_area,
                 Shoreline.Length=shoreline_length)

##Variable selection for 2007&2012 model
set.seed(42)
var_sel_tmean <- varsel_regression_rf(nla.all$tmean_2m, select(nla.all, -tmean_2m),                ntree=1000, prog=TRUE, importance=TRUE)


nla_select <- select(nla.all, tmean_2m,var_sel_tmean$vars[[7]])
set.seed(42)
RFAll <- randomForest(tmean_2m ~ ., data = nla_select, importance = TRUE,
               ntree = 10000)


###Predict RF values for all points
RFAll.predict<- predict(RFAll, data=nla_select[,2:9])
RFAll.combined<-cbind(nla_select,RFAll.predict)
write.csv(file="RFAlllPredict.csv",RFAll.combined)
reg <- lm(RFAll.combined[,10]~RFAll.combined[,1], data = RFAll.combined)

## Calculate partial dependencies




