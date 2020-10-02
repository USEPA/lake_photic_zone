# Library
library(randomForest)
library(dplyr)
library(purrr)
library(here)
library(broom)
library(lubridate)
library(readr)

source(here::here('R/varsel_regression_rf.R'))

##Reading in and creating data sets
nla<-read.csv(here::here('data/nla_base.csv'))
x<-complete.cases(nla)
nla.cc<-nla[x,]
nla.cc$year<-factor(nla.cc$year)
nla.cc$date <- lubridate::yday(lubridate::ymd(nla.cc$date))

nla.all <-  filter(nla.cc, tmean_2m < 70) %>%
              dplyr::select(nla_id, tmean_2m, year,date, longitude, latitude, tmean,
              tmean_avg30,percent_impervious,surface_area,shoreline_length,
              shoreline_dev, max_length, max_width, mean_width, max_depth,
              max_depth, volume, elevation)

nla.all<-rename(nla.all, nla_id = nla_id, Date=date,Average.Temperature=tmean,Longitude=longitude,
                 Thirty.Day.Average.Temperature=tmean_avg30,Elevation=elevation,
                 Latitude=latitude,Surface.Area=surface_area,
                 Shoreline.Length=shoreline_length)

##Variable selection for 2007&2012 model
set.seed(42)
var_sel_tmean <- varsel_regression_rf(nla.all[,-1]$tmean_2m, select(nla.all[,-1], -tmean_2m),                ntree=1000, prog=TRUE, importance=TRUE)


nla_select <- select(nla.all, nla_id, tmean_2m,var_sel_tmean$vars[[7]])
write_csv(nla_select, here::here("data/nla_select.csv"))
set.seed(42)
RFAll <- randomForest(tmean_2m ~ ., data = nla_select[,-1], importance = TRUE,
               ntree = 10000)


###Predict RF values for all points
RFAll.combined<-cbind(nla_select,RFAll$predicted)
write.csv(file="RFAlllPredict.csv",RFAll.combined)
reg <- lm(RFAll.combined[,11]~RFAll.combined[,2], data = RFAll.combined)

## Predict for all RF Trees
rf_all_trees <- predict(RFAll, newdata = nla_select[,-1], predict.all = TRUE)
rf_all_trees <- data.frame(rf_all_trees$individual)
rf_all_trees <- mutate(rf_all_trees, nla_id = nla_select$nla_id, 
                       tmean_2m = nla_select$tmean_2m)
rf_all_trees <- select(rf_all_trees, nla_id, tmean_2m, everything())
rf_all_trees <- mutate(rf_all_trees, rmse = apply(rf_all_trees, 1, function(x) 
  sqrt(mean((as.numeric(x[3:length(x)]) - as.numeric(x[2]))^2))))
rf_all_rmse <- select(rf_all_trees, nla_id, tmean_2m, rmse) %>%
  left_join(nla)
#plot(density(rf_all_rmse$rmse))
#peak <- rmse_dens$x[which.max(rmse_dens$y)]
#abline(v = peak)
