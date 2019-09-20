#' Random Forest Variable Importance Plots
#' 
#' This function creates the variable importance plots used in the Kreakie et al. 
#' paper on random forest modeling of photic zone temperature
#' 
#' @param rfobj A random forest object. see \link{\code{randomForest}} for 
#'              details on creating this object.
#' @param var_names Optional variable names for the y axis.  Must be in same 
#'                  order as row.names(rfobj$importance).
#'              
varimp_plot <- function(rfobj, var_names = NULL){
  if(is.null(var_names)){var_names <- row.names(rfobj$importance)}
  perc_inc_mse <- data.frame(rfobj$importance)$X.IncMSE/rfobj$importanceSD
  var_names <- reorder(factor(var_names), perc_inc_mse)
  varimp_df <- data.frame(var_names, perc_inc_mse, stringsAsFactors = FALSE)
  plot_out <- ggplot(varimp_df, aes(y = perc_inc_mse, x = var_names)) +
    geom_bar(stat = "identity", width = 0.5) +
    coord_flip() +
    labs(x = "Independent variables", 
         y = "Percent increase mean square error") +
    theme_ipsum_rc(base_size = 12, axis_title_size = 12) +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 0.3, b = 0, 
                                                      l = 0, "cm")),
          axis.title.x = element_text(margin = margin(t = 0.3, r = 0, b = 0, 
                                                      l = 0, "cm")))
  plot_out
}