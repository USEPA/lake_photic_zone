#' script to generate figures
#' 
source(here::here("R/varsel_plot.R"))
source(here::here("R/varimp_plot.R"))
source(here::here("R/partial_plot.R"))
source(here::here("R/model.R"))

extrafont::loadfonts(device="win")
library(ggplot2)
library(hrbrthemes)
library(readr)
library(dplyr)
library(sf)
library(USAboundaries)
library(colorspace)
library(classInt)

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
                            predicted = RFAll$predicted,
                            area = nla_select$Surface.Area) %>%
  mutate(area_class = case_when(area <= quantile(area, 0.75) ~ "class_1", 
                                area > quantile(area, 0.75) ~ "class_2")) %>%
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


# Map of prediction error
# Note that mdev is equivalent to predicted - observed, so some unecesary code
# here.
temp_error_data <- read_csv(here::here("data/rf_all_tree_rmse.csv")) %>%
  select(nla_id, rmse, mdev, longitude, latitude, year, surface_area) %>%
  mutate(error_class = case_when(mdev >= -1 & mdev <= 1 ~
                                   "-1 - 1",
                                 mdev > 1 & mdev <= 2 ~
                                   "1 - 2",
                                 mdev > 2 & mdev <= 3 ~
                                   "2 - 3",
                                 mdev > 3 ~
                                   "> 3",
                                 mdev < -1 & mdev >= -2 ~
                                   "-1 - -2",
                                 mdev < -2 & mdev >= -3 ~
                                   "-2 - -3",
                                 mdev < -3 ~
                                   "< -3",
                                 TRUE ~ "")) %>%
  mutate(error_class = factor(error_class, levels = c("> 3", "2 - 3", "1 - 2", "-1 - 1", "-1 - -2", "-2 - -3", "< -3"), ordered = TRUE)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = 5072)

usa <- us_states() %>%
  filter(!state_abbr %in% c("AK", "HI", "PR")) %>%
  st_transform(crs = 5072)

col <- diverging_hcl(n = 7, h = c(260, 0), c = 80, l = c(30, 90), power = 1.5)
col <- col[length(col):1]
error_map_gg <- ggplot(usa) +
  #geom_sf(data = temp_error_data, aes(color = error_class), size = 2) +
  geom_sf(data = temp_error_data, aes(color = error_class, size = error_class), alpha = 0.5) +
  geom_sf(fill = NA, size = 0.65) +
  scale_color_manual(values = col, name = "Error (°C)") +
  scale_size_manual(values = c(2.5, 1.75 , 1, 0.25, 1, 1.75, 2.5), name = "Error (°C)") +
  theme_ipsum_rc() +
  theme(legend.position = "bottom") +
  guides(col = guide_legend(nrow = 1, byrow = TRUE))
ggsave(here::here("figures/error_map.jpg"), error_map_gg, width = 7.5, 
       height = 5.625, units = "in", dpi = 600)

nla_map_gg <- ggplot(usa) + 
  geom_sf(data = temp_error_data, aes(color = as.factor(year), 
                                      shape = as.factor(year)), alpha = 0.6) +
  geom_sf(fill = NA, size = 0.65) +
  scale_color_manual(values = c("darkblue", "darkred"), name = "Year") +
  scale_shape_manual(values = c(16,17), name = "Year") +
  theme_ipsum_rc() +
  theme(legend.position = "bottom") +
  guides(col = guide_legend(nrow = 1, byrow = TRUE))
ggsave(here::here("figures/nla_map.jpg"), nla_map_gg, width = 7.5, 
       height = 5.625, units = "in", dpi = 600)
  
  