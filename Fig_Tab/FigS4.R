require(ggplot2)
require(gridExtra)
library(ggpubr)
library(patchwork)

rm(list=ls())

#### Load Data ####
load("dNNdt_N_CA.rda")
load("dNNdt_N_LL.rda")
load("dNNdt_N_UL.rda")
load("dNNdt_N_TY.rda")

####### Putting all Together #######

reso <- 300
length <- 3.25*reso/72


png(file="FigS4_260903.png",width=length/1.6, height=length/2, res = reso, units = "in");
ca_plot + ll_plot +
  ty_plot + ul_plot +
  
  plot_layout(axis_titles = "collect",axes = "collect", ncol = 2, nrow =2);
dev.off()
