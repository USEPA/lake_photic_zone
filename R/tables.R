# Libraries
library(readr)
library(dplyr)
library(tidyr)
library(officer)
library(flextable)
library(lubridate)

var_dat <- read_csv(here::here("data/rf_all_tree_rmse.csv")) %>%
  mutate(error = tmean_2m - tmean_2m_rf_pred,
         date = ymd(date)) %>%
  mutate(date = lubridate::yday(date),
         shoreline_length = as.numeric(units::set_units(units::set_units(shoreline_length, 
                                                              "m"), "km")),
         surface_area = as.numeric(units::set_units(units::set_units(surface_area, 
                                                          "m2"), "km2"))) %>%
  select(nla_id, tmean, date, longitude, tmean_avg30, elevation, 
         latitude, shoreline_length, surface_area) %>%
  pivot_longer(tmean:surface_area, "variable", "value")

var_summary <- var_dat %>%
  group_by(variable) %>%
  summarize("Min." = min(value, na.rm = TRUE),
            "25th" = quantile(value, 0.25, na.rm = TRUE),
            "Median" = median(value, na.rm = TRUE),
            "Mean" = mean(value, na.rm = TRUE),
            "75th" = quantile(value, 0.75, na.rm = TRUE),
            "Max" = max(value, na.rm = TRUE)) %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate(variable = factor(variable, labels = c("Date (Day of year)", 
                                                "Elevation (meters)",
                                                "Latitude (decimal degrees)",
                                                "Longitude (decimal degrees)",
                                                "Shoreline length (km)",
                                                "Surface area (km²)",
                                                "Day of temperature (°C)", 
                                                "Average temperature 30 day prior (°C)")))

#df to word table
#https://stackoverflow.com/questions/25425993/data-frame-to-word-table/25427314

var_summary_table <- flextable(data = var_summary) %>%
  autofit()

read_docx() %>%
  body_add_flextable(var_summary_table) %>%
  print(target = here::here("manuscript/summary_table.docx"))


# error vs variables
err_var_dat <- read_csv(here::here("data/rf_all_tree_rmse.csv")) %>%
  mutate(error = tmean_2m - tmean_2m_rf_pred,
         date = ymd(date),
         shoreline_length = log(shoreline_length),
         surface_area = log(surface_area)) %>%
  select(nla_id, error, tmean, date, longitude, tmean_avg30, elevation, 
         latitude, shoreline_length, surface_area) %>%
  mutate(date = lubridate::yday(date)) %>% 
  pivot_longer(tmean:surface_area, "variable", "value")

err_cor_table <- err_var_dat %>%
  group_by(variable) %>%
  summarise(cor = cor(value,error)) %>%
  mutate_if(is.numeric, round, 3) %>%
  flextable() %>%
  autofit()

read_docx() %>%
  body_add_flextable(err_cor_table) %>%
  print(target = here::here("manuscript/cor_table.docx"))
