library(ggplot2)
library(grid)
library(dplyr)
library(lubridate)
library(gridExtra)

rm(list=ls())
load("CA_r2_plots.rda")
load("TY_r2_plots.rda")
load("UL_r2_plots.rda")
load("LL_r2_plots.rda")


reso <- 300
length <- 3.25*reso/72

png(file="FigS1.png",width=length/1.2, height=length/1.8, res = reso, units = "in");
grid.arrange(r2_plot_CA + labs(x = "", title = "CAi (intact)"),
             r2_plot_LL + labs(x = "", y = "", title = "LLi (intact)"),
             r2_plot_TY + labs(title = "TYt (thinned)"),
             r2_plot_UL + labs(y = "", title = "ULt (thinned)"),
             ncol = 2);
dev.off()