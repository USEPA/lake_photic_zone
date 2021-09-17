#' Random Forest Partial Dependency Plots
#' 
#' This function creates the partial dependency plots used in the Kreakie et al. 
#' paper on random forest modeling of photic zone temperature
#' 
#' @param partial_data an data frame with partial dependency calcs and variable names
#'              
partial_plot <- function(partial_data){
  
  partial_data$variable <- factor(partial_data$variable, 
                                  labels = c("Day~of~the~year", 
                                             expression("Avg.~temperature~( degree*C)"), 
                                             "Longitude~(decimal~degrees)", 
                                             expression("30-day~avg.~temperature~( degree*C)"), 
                                             "Elevation~(m)", 
                                             "Latitude~(decimal~degrees)", 
                                             expression("Lake~area~(m^2)"), 
                                             "Lake~shoreline~length~(m)"))
  part_plot <- partial_data %>%
    ggplot(aes(x = x, y = y)) +
    geom_line() +
    facet_wrap(variable ~ ., ncol = 2, 
               scales = "free_x", 
               strip.position = "bottom",
               labeller = label_parsed) +
    theme_ipsum_rc(base_size = 12, axis_text_size = 10, axis_title_size = 12) +
    theme(axis.title.y=element_text(vjust = 5, hjust = 0.5),
          title = element_blank(), strip.placement = "outside") +
    labs(y = expression(paste("Temperature (", degree~C, ")")))
 part_plot
}