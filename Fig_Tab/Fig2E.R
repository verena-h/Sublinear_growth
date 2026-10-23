require(BayesianTools)
require(pttstability)
require(ggplot2)
require(gridExtra)
library(ggpubr)
library(patchwork)
rm(list=ls())

#Load Guppy data
summary <- read.csv("data.csv")

####### TY 
TY<- summary[summary$stream=="TY",]
TY_FM <- TY[, c("F.N", "M.N")]
TY_FM$y<- rowSums(TY_FM)

y_ty <- TY_FM$y
y_ty_noy = data.frame(matrix(data =NA, nrow =144, ncol =1))
y_ty_noy[1:12,1] = NA #create empty year to align all 4 timeseries
y_ty_noy[13:144,1] = y_ty
colnames(y_ty_noy)[1] = "n"

plot_ty = ggplot(y_ty_noy, aes(y=n, x=1:144)) + 
  geom_point(color = "#00BFC4", shape = 16) +
  geom_line(color = "#00BFC4") +
  geom_vline(xintercept = 90, linetype = 2) +
  labs(x="Time (Month)",y="Abundance [N]", title = "TYt (thinned)") +
  theme_linedraw() +
  expand_limits(x = c(0,144), y = c(0,3300)) +
  
  theme(plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "inches"))
plot_ty

###### UL
UL<- summary[summary$stream=="UL",]
UL_FM <- UL[, c("F.N", "M.N")]
UL_FM$y<- rowSums(UL_FM)

y_ul <- UL_FM$y
y_ul = as.data.frame(y_ul)

#t1 = 1:83
#t2 = 84:144 90 August 2015 used

plot_UL = ggplot(y_ul, aes(y=y_ul, x=1:144)) + 
  geom_point(color = "#C77CFF", shape = 16) +
  geom_line(color = "#C77CFF") +
  geom_vline(xintercept = 90, linetype = 2) +
  labs(x="Time (Month)",y="Abundance [N]", title = "ULt (thinned)") +
  theme_linedraw() +
  expand_limits(x = c(0,144), y = c(0,3300)) +
 
  theme(plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "inches"))
 
plot_UL
######## LL 

LL<- summary[summary$stream=="LL",]
LL_FM <- LL[, c("F.N", "M.N")]
LL_FM$y<- rowSums(LL_FM)

y_ll <- LL_FM$y
y_ll = as.data.frame(y_ll)

#t1 = 1:81
#t2 = 82:144 90 August 2015 used

plot_LL = ggplot(y_ll, aes(y=y_ll, x=1:144)) + 
  geom_point(color = "#7CAE00", shape = 16) +
  geom_line(color = "#7CAE00") +
  geom_vline(xintercept = 90, linetype = 2) +
  labs(x="Time (Month)",y="Abundance [N]", title = "LLi (intact)") +
  theme_linedraw() +
  expand_limits(x = c(0,144), y = c(0,3300)) +
 
  theme(plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "inches"))

plot_LL
##### CA

CA<- summary[summary$stream=="CA",]
CA_FM <- CA[, c("F.N", "M.N")]
CA_FM$y<- rowSums(CA_FM)

y_ca <- CA_FM$y

y_ca_noy = data.frame(matrix(data =NA, nrow =144, ncol =1))
y_ca_noy[1:12,1] = NA #create empty year to align all 4 timeseries
y_ca_noy[13:144,1] = y_ca
colnames(y_ca_noy)[1] = "n"

plot_ca = ggplot(y_ca_noy, aes(y=n, x=1:144)) + 
  geom_point(color = "#F8766D", shape = 16) +
  geom_line(color = "#F8766D") +
  geom_vline(xintercept = 90, linetype = 2) +
  labs(x="Time (Month)",y="Abundance [N]", title = "CAi (intact)") +
  theme_linedraw() +
  expand_limits(x = c(0,144), y = c(0,3300)) +
  
  theme(plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "inches"))
        
plot_ca

reso <- 300
length <- 3.25*reso/72


png(file="Fig2E_260827.png",width=length/2, height=length/3, res = reso, units = "in");

plot_ca + plot_LL +
  plot_ty  + plot_UL +
  plot_layout(axis_titles = "collect",axes = "collect", ncol = 2, nrow =2);

dev.off()


