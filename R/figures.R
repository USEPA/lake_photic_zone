#' script to generate figures
#' load up functions and packages
#' 
source(here::here("R/functions.R"))


varsel_fig<-varsel_plot(var_sel_tmean)

ggsave(here("figures/valselfig.jpg"), varsel_fig, width = 7.5, height = 5.625, 
       units = "in", dpi = 600)


```{r all_var_sel_figure, eval=TRUE,results="asis",echo=FALSE, fig.cap="Variable selection plot for all variables.  Shows percent increase in mean squared error as a function of the number of variables.  \\label{fig:all_varsel_figure}",cache=FALSE}
varsel_fig<-varsel_plot(var_sel_tmean)

#Height maximum is 6.8 inches
ggsave(filename = "ecosphere_final/hollisterES15-00703R_fig3.tiff",
       plot = fig3,
       dpi = 450,
       #width = 6.1,
       height = 6.8,
       units = "in")
x<-autocrop("ecosphere_final/hollisterES15-00703R_fig3.tiff",border = 5, 
            outfile = "ecosphere_final/hollisterES15-00703R_fig3.tiff",
            width = 6.1,
            res = 450,
            compression = "lzw")
fig3
```