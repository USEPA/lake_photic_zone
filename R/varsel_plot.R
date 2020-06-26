# https://raw.githubusercontent.com/USEPA/LakeTrophicModelling/master/R/varsel_plot.R

#' Plot varsel_regression output
#' 
#' @param vs_reg_obj Object from output of \link{varsel_regression_rf}.
#' @export
#' @import ggplot2
varsel_plot <- function(vs_reg_obj){
  dat <- data.frame(num = vs_reg_obj$num_var, mse = vs_reg_obj$mse)
  #browser()
  plot_out<-ggplot(dat, aes(x=num,y=mse)) +
    geom_point(size = 3) +
    theme_ipsum_rc(base_size = 12, axis_title_size = 12) +
    labs(x="Number of variables in model", y="Root Mean Square Error") +
    theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 0.3, b = 0, 
                                                      l = 0, "cm")),
          axis.title.x = element_text(margin = ggplot2::margin(t = 0.3, r = 0, b = 0, 
                                                      l = 0, "cm")))
  plot_out
}