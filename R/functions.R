# Installations
pkgs <- c("lubridate","broom", "stringr", "dplyr", "readr", "readxl","tidyr", 
          "here","ggplot2", "hrbrthemes", "Kendall","sf", "USAboundaries",
          "USAboundariesData","knitr","captioner")

for(i in pkgs){
  if(!i %in% installed.packages()){
    install.packages(i, repos = c("http://packages.ropensci.org", 
                                  "https://cran.rstudio.com"))
  }
}

# Library
library(lubridate)
library(broom)
library(stringr)
library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(here)
library(ggplot2)
library(hrbrthemes)
library(Kendall)
library(LAGOSNE)
library(sf)
library(USAboundaries)
library(USAboundariesData)
library(knitr)
library(captioner)
library(cowplot)

# https://raw.githubusercontent.com/USEPA/LakeTrophicModelling/master/R/varsel_plot.R

#' Plot varsel_regression output
#' 
#' @param vs_reg_obj Object from output of \link{varsel_regression_rf}.
#' @export
#' @import ggplot2
varsel_plot <- function(vs_reg_obj){
  dat <- data.frame(num = vs_reg_obj$num_var, mse = vs_reg_obj$mse)
  browser()
  plot_out<-ggplot(dat, aes(x=num,y=mse))+geom_point(size=2.5)+
    theme(text = element_text(family="sans"),
          panel.background = element_blank(), #panel.grid = element_blank(), 
          panel.border = element_rect(fill = NA), 
          plot.title  = element_text(family="sans",size=15,face="bold",vjust=1.1),
          legend.key = element_rect(fill = 'white'),
          legend.text = element_text(family="sans",size=15), legend.title = element_text(size=15),
          axis.title.x = element_text(family="sans",vjust = -0.5, size = 12),
          axis.title.y = element_text(family="sans",vjust = 1.5, size = 12),
          axis.text.x = element_text(family="sans",size = 11),
          axis.text.y = element_text(family="sans",size = 11))+
    labs(x="Number of variables in model", y="Root Mean Square Error")
  return(plot_out)
}