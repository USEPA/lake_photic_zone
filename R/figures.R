#' script to generate figures
#' load up functions and packages
#' 
source(here::here("R/functions.R"))


varsel_fig<-varsel_plot(var_sel_tmean)

ggsave(here::here("figures/valselfig.jpg"), varsel_fig, width = 7.5, height = 5.625, 
       units = "in", dpi = 600)


