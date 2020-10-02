#' script to generate figures
#' 
source(here::here("R/varsel_plot.R"))
source(here::here("R/varimp_plot.R"))
source(here::here("R/partial_plot.R"))
#source(here::here("R/model.R"))

#extrafont::loadfonts(device="win")
library(ggplot2)
library(hrbrthemes)
library(readr)
nla_select <- read_csv(here::here("data/nla_select.csv"))

###Variable selection figure 
varsel_fig<-varsel_plot(var_sel_tmean)
#varsel_fig
ggsave(here::here("figures/varSelFig.jpg"), varsel_fig, width = 7.5, 
       height = 5.625, units = "in", dpi = 600)


###Variable importance figure
variable_names <- c("Avg. temperature", "Date", "Longitude", 
                    "30-day avg. temperature", "Elevation", "Latitude", 
                    "Lake shoreline length", "Lake area")
varimp_fig <- varimp_plot(RFAll, variable_names)
#varimp_fig
ggsave(here::here("figures/varImpPlot.jpg"), varimp_fig, width = 7.5, 
       height = 5.625, units = "in", dpi = 600)



###Partial Dependence Plots 
partplot_date <- partialPlot(RFAll, nla.all, Date, plot = FALSE)
partplot_avg_temp <- partialPlot(RFAll, nla.all, Average.Temperature, plot = FALSE)
partplot_long <- partialPlot(RFAll, nla.all,Longitude, plot = FALSE)
partplot_thirty_day <- partialPlot(RFAll, nla.all,Thirty.Day.Average.Temperature, plot = FALSE)
partplot_elev <- partialPlot(RFAll, nla.all,Elevation, plot = FALSE)
partplot_lat <- partialPlot(RFAll, nla.all,Latitude, plot = FALSE)
partplot_surf_area <- partialPlot(RFAll, nla.all,Surface.Area, plot = FALSE)
partplot_shoreline_leng <- partialPlot(RFAll, nla.all,Shoreline.Length, plot = FALSE)
pp_date <- data.frame(partplot_date, variable = "Date",
                      stringsAsFactors = FALSE)
pp_avg_temp <- data.frame(partplot_avg_temp, variable = "Avg. temperature",
                          stringsAsFactors = FALSE)
pp_long <- data.frame(partplot_long, variable = "Longitude",
                      stringsAsFactors = FALSE)
pp_thirty_day <- data.frame(partplot_thirty_day, 
                            variable = "30-day avg. temperature",
                            stringsAsFactors = FALSE)
pp_elev <- data.frame(partplot_elev, variable = "Elevation",
                      stringsAsFactors = FALSE)
pp_lat <- data.frame(partplot_lat, variable = "Latitude",
                     stringsAsFactors = FALSE)
pp_surf_area <- data.frame(partplot_surf_area, variable = "Lake area",
                           stringsAsFactors = FALSE)
pp_shoreline_length <- data.frame(partplot_shoreline_leng, 
                                  variable = "Lake shoreline length",
                                  stringsAsFactors = FALSE)
pp_data <- rbind(pp_date, pp_avg_temp, pp_long, pp_thirty_day, pp_elev, pp_lat,
                 pp_surf_area, pp_shoreline_length)
write_csv(pp_data, here::here("figures/pp_data.csv"))

pp_data <- pp_data %>%
  mutate(variable = factor(variable, levels = c("Date", "Avg. temperature",
                                                "Longitude", 
                                                "30-day avg. temperature",
                                                "Elevation", "Latitude",
                                                "Lake area", 
                                                "Lake shoreline length")))

pp_fig <- partial_plot(pp_data)
#pp_fig
ggsave(here::here("figures/partPlot.jpg"), pp_fig, width = 8, 
       height = 10, units = "in", dpi = 300)

# Obs v Pred
obs_v_pred_gg <- data.frame(observed = nla_select$tmean_2m, 
                            predicted = RFAll$predicted) %>%
  ggplot(aes(x = observed, y = predicted)) +
  geom_point(alpha = 0.35) +
  geom_abline(slope = 1, intercept = 0, size = 1.25, color = "darkblue") +
  theme_ipsum_rc() +
  scale_x_continuous(limits = c(8,38), breaks = seq(10, 35, 5)) +
  scale_y_continuous(limits = c(8,38), breaks = seq(10, 35, 5)) +
  labs(y = "Predicted photic zone temperature (°C)", 
       x = "Measured photic zone temperature (°C)") +
  theme(axis.title.x = element_text(size = 12, vjust = -1),
        axis.title.y = element_text(size = 12, vjust = 4))

ggsave(here::here("figures/obs_v_pred.jpg"), obs_v_pred_gg, width = 7.5, 
                  height = 5.625, units = "in", dpi = 600)

