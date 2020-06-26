#' Random Forest Partial Dependency Plots
#' 
#' This function creates the partial dependency plots used in the Kreakie et al. 
#' paper on random forest modeling of photic zone temperature
#' 
#' @param partial_data an data frame with partial dependency calcs and variable names
#'              
partial_plot <- function(partial_data){
  #browser()
  part_plot <- partial_data %>%
    ggplot(aes(x = x, y = y)) +
    geom_line() +
    facet_wrap(variable ~ ., ncol = 2, scales = "free") +
    theme_ipsum_rc(base_size = 12, axis_text_size = 10, axis_title_size = 10) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank())
 part_plot
}