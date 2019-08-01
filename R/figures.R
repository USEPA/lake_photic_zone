#' script to generate figures
#' load up functions and packages
#' 
source(here::here("R/functions.R"))

###Variable selection figure 
varsel_fig<-varsel_plot(var_sel_tmean)

ggsave(here::here("figures/varSelFig.jpg"), varsel_fig, width = 7.5, height = 5.625, 
       units = "in", dpi = 600)


###Variable importance figure

jpeg("figures/varImpPlot.jpg")
varImpPlot(RFAll,type=1,main="")
dev.off()



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




