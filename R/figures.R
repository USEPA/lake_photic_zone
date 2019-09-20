#' script to generate figures
#' 
source(here::here("R/varsel_plot.R"))
source(here::here("R/varimp_plot.R"))
source(here::here("R/partial_plot.R"))

#extrafont::loadfonts(device="win")
library(ggplot2)
library(hrbrthemes)

###Variable selection figure 
varsel_fig<-varsel_plot(var_sel_tmean)
ggsave(here::here("figures/varSelFig.jpg"), varsel_fig, width = 7.5, 
       height = 5.625, units = "in", dpi = 600)


###Variable importance figure
variable_names <- c("Avg. temperature", "Date", "Longitude", 
                    "30-day avg. temperature", "Elevation", "Latitude", 
                    "Lake shoreline length", "Lake area")
varimp_fig <- varimp_plot(RFAll, variable_names)
varimp_fig
ggsave(here::here("figures/varImpPlot.jpg"), varimp_fig, width = 7.5, 
       height = 5.625, units = "in", dpi = 600)



###Partial Dependence Plots 
jpeg("figures/partPlot.jpg")
par(mfrow=c(4,2))

partialPlot(RFAll, nla.all,Date,main="")
partialPlot(RFAll, nla.all,Average.Temperature,main="")
partialPlot(RFAll, nla.all,Longitude,main="")
partialPlot(RFAll, nla.all,Thirty.Day.Average.Temperature,main="")
partialPlot(RFAll, nla.all,Elevation,main="")
partialPlot(RFAll, nla.all,Latitude,main="")
partialPlot(RFAll, nla.all,Surface.Area,main="")
partialPlot(RFAll, nla.all,Shoreline.Length,main="")
dev.off()




