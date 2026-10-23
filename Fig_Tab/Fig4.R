require(pttstability)
require(ggplot2)
require(gridExtra)
library(ggpubr)
library(patchwork)

rm(list=ls())

#### Load Data ####
#load CA
load("pttstability/rda_varProc/filterout_CA.rda")
load("Theta_est_CA.rda")
tm = c(1:132)
df_CA = as.data.frame(cbind(tm,coefmat))
df_CA$n_st = y_m
df_CA$n = y
df_CA_noy = data.frame(matrix(data =NA, nrow =144, ncol =7))
df_CA_noy[1:12,2:7] = NA
df_CA_noy[13:144,2:7] = df_CA[,2:7]
df_CA_noy[,1] = rownames(df_CA_noy)
colnames(df_CA_noy) = c("Time","Alpha","Theta","Alpha_sd","Theta_sd","stN","N")
df_CA_noy$Time = as.numeric(df_CA_noy$Time)

#load TY
load("filterout_TY.rda")
load("Theta_est_TY.rda")
tm = c(1:132)
df_TY = as.data.frame(cbind(tm,coefmat))
df_TY$n_st = y_m
df_TY$n = y
df_TY_noy = data.frame(matrix(data =NA, nrow =144, ncol =7))
df_TY_noy[1:12,2:7] = NA
df_TY_noy[13:144,2:7] = df_TY[,2:7]
df_TY_noy[,1] = rownames(df_TY_noy)
colnames(df_TY_noy) = c("Time","Alpha","Theta","Alpha_sd","Theta_sd","stN","N")
df_TY_noy$Time = as.numeric(df_TY_noy$Time)
#load LL
load("filterout_LL.rda")
load("Theta_est_LL.rda")
tm = c(1:144)
df_LL = as.data.frame(cbind(tm,coefmat))
df_LL$n_st = y_m
df_LL$n = y
colnames(df_LL) = c("Time","Alpha","Theta","Alpha_sd","Theta_sd","stN","N")
df_LL$Time = as.numeric(df_LL$Time)
#load UL
load("filterout_UL.rda")
load("Theta_est_UL.rda")
tm = c(1:144)
df_UL = as.data.frame(cbind(tm,coefmat))
df_UL$n_st = y_m
df_UL$n = y
colnames(df_UL) = c("Time","Alpha","Theta","Alpha_sd","Theta_sd","stN","N")
df_UL$Time = as.numeric(df_UL$Time)

##### Plotting CA ######

theta_plot_CA = ggplot(df_CA_noy, aes(y=(Theta), x = Time)) +
  #geom_errorbar(aes(ymin = (Theta-Theta_sd), ymax = (Theta+Theta_sd)), width = 1.5) +
  #geom_point(color = "#F8766D") +
  geom_line(color = "#F8766D", linewidth = 0.6) +
  geom_ribbon(aes(ymin=Theta-Theta_sd, ymax=Theta+Theta_sd), fill = "#F8766D", alpha = 0.3 ) +
  
  geom_vline(xintercept = 90, linetype = 2) +
  geom_hline(yintercept = 1) +
  labs(x="Time (Month)", y = "Fitted \u03b8", title = "CAi (intact)") +
  expand_limits(x = c(0,144), y = c(-10,5.5)) +
  scale_x_continuous(
                     sec.axis = sec_axis(~./12 + 2008,
                                         breaks = c(2008, 2010, 2012,2014,2016,2018,2020),
                                         name = "Time (Year)")) +
  #annotate("text", x = 13, y = 2, label="Superlinear") +
  #annotate("text", x = 10, y = 0, label="Sublinear") +
  #annotate("text", x = 14, y = -6, label="CAi (intact)") +
  coord_cartesian(ylim = c(-10,5.5), clip = "on") +
  theme_linedraw() + 
  theme(plot.margin = unit(c(0.05, 0.05, 0.05, 0.05),"inches")) 

theta_plot_CA

t_abundance_CA= ggplot(df_CA_noy, aes(y=Theta, x = N)) +
  
  geom_errorbar(aes(ymin=Theta-Theta_sd, ymax=Theta+Theta_sd), width=7, position=position_dodge(.9),
                colour="black", alpha=0.5, size=0.5) + 
  geom_point(color = "#F8766D") +
  geom_hline(yintercept = 1) +
  labs(x="Abundance [N]", y = "Fitted \u03b8") +
  #annotate("text", x = 140, y = 2, label="Superlinear") +
  #annotate("text", x = 140, y = 0, label="Sublinear") +
  scale_x_continuous(limits = c(0, 3300),sec.axis = dup_axis()) +
  scale_y_continuous(limits = c(-10, 5.5),sec.axis = dup_axis()) +
  coord_cartesian(ylim = c(-10,5.5), clip = "on") +
  theme_linedraw() +
  
  
  #theme(axis.title.y = element_text(vjust = 3))+ 
  theme(plot.margin = unit(c(0.05, 0.05, 0.05, 0.05),"inches"))

t_abundance_CA


##### Plotting TY ######

theta_plot_TY = ggplot(df_TY_noy, aes(y=(Theta), x = Time)) +
  #geom_errorbar(aes(ymin = (Theta-Theta_sd), ymax = (Theta+Theta_sd)), width = 1.5) +
  #geom_point(color = "#00BFC4") +
  geom_line(color = "#00BFC4", linewidth = 0.6) +
  geom_ribbon(aes(ymin=Theta-Theta_sd, ymax=Theta+Theta_sd), fill = "#00BFC4", alpha = 0.3 ) +
  geom_vline(xintercept = 90, linetype = 2) +
  
  labs(x="Time (Month)", y = "Fitted \u03b8", title = "TYt (thinned)") +
  expand_limits(x = c(0,144), y = c(-10,5.5)) +
  ylim(-10,5.5) +
  #scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150), name = "Month",
  #                   sec.axis = sec_axis(~./12 + 2008,
  #                                       breaks = c(2008, 2010, 2012,2014,2016,2018,2020),
  #                                       name = "Year")) +
  
  geom_hline(yintercept = 1) +
  coord_cartesian(ylim = c(-10,5.5), clip = "on") +
  #annotate("text", x = 14, y = -6, label="TYt (thinned)") +
  theme_linedraw() + 
  theme(plot.margin = unit(c(0.05, 0.05, 0.05, 0.05),"inches")) 
theta_plot_TY

t_abundance_TY= ggplot(df_TY_noy, aes(y=Theta, x = N)) +
  
  geom_errorbar(aes(ymin=Theta-Theta_sd, ymax=Theta+Theta_sd), width=15, position=position_dodge(.9),
                colour="black", alpha=0.5, size=0.5) + 
  geom_point(color = "#00BFC4") +
  geom_hline(yintercept = 1) +
  #annotate("text", x = 125, y = 2, label="Superlinear") +
  #annotate("text", x = 125, y = 0, label="Sublinear") +
  labs(x="Abundance [N]", y = "Fitted \u03b8") +
  coord_cartesian(ylim = c(-10,5.5), clip = "on") +
  theme_linedraw() +
  scale_y_continuous(limits = c(-10, 5.5),sec.axis = dup_axis()) +
  xlim(0,3300)+
  #theme(axis.title.y = element_text(vjust = 3))+ 
  theme(plot.margin = unit(c(0.05, 0.05, 0.05, 0.05),"inches"))

t_abundance_TY
##### Plotting UL ######

theta_plot_UL = ggplot(df_UL, aes(y=(Theta), x = Time)) +
  #geom_errorbar(aes(ymin = (Theta-Theta_sd), ymax = (Theta+Theta_sd)), width = 1.5) +
  #geom_point(color = "#C77CFF") +
  geom_line(color = "#C77CFF", linewidth = 0.6) +
  geom_ribbon(aes(ymin=Theta-Theta_sd, ymax=Theta+Theta_sd), fill = "#C77CFF", alpha = 0.3 ) +
  geom_vline(xintercept = 90, linetype = 2) +
  
  labs(x="Time (Month)", y = "Fitted \u03b8", title = "ULt (thinned)") +
  expand_limits(x = c(0,144), y = c(-10,5.5)) +
  
  #scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150), name = "Month",
  #                   sec.axis = sec_axis(~./12 + 2008,
   #                                      breaks = c(2008, 2010, 2012,2014,2016,2018,2020),
   #                                      name = "Year")) +
  ylim(-10,5.5) +
  geom_hline(yintercept = 1) +
  coord_cartesian(ylim = c(-10,5.5), clip = "on") +
  #annotate("text", x = 14, y = -6, label="ULt (thinned)") +
  theme_linedraw() + 
  theme(plot.margin = unit(c(0.05, 0.05, 0.05, 0.05),"inches"))
theta_plot_UL

t_abundance_UL= ggplot(df_UL, aes(y=Theta, x = N)) +
  
  geom_errorbar(aes(ymin=Theta-Theta_sd, ymax=Theta+Theta_sd), width=30, position=position_dodge(.9),
                colour="black", alpha=0.5, size=0.5) + 
  geom_point(color = "#C77CFF") +
  geom_hline(yintercept = 1) +
  labs(x="Abundance [N]", y = "Fitted \u03b8") +
  coord_cartesian(ylim = c(-10,5.5), clip = "on") +
  theme_linedraw() +
  #annotate("text", x = 160, y = 2, label="Superlinear") +
  #annotate("text", x = 160, y = 0, label="Sublinear") +
  scale_y_continuous(limits = c(-10, 5.5),sec.axis = dup_axis()) +
  xlim(0,3300)+
  #theme(axis.title.y = element_text(vjust = 3))+ 
  theme(plot.margin = unit(c(0.05, 0.05, 0.05, 0.05),"inches"))

t_abundance_UL
##### Plotting LL ######

theta_plot_LL = ggplot(df_LL, aes(y=(Theta), x = Time)) +
  #geom_errorbar(aes(ymin = (Theta-Theta_sd), ymax = (Theta+Theta_sd)), width = 1.5) +
  #geom_point(color = "#7CAE00") +
  geom_line(color = "#7CAE00", linewidth = 0.6) +
  geom_ribbon(aes(ymin=Theta-Theta_sd, ymax=Theta+Theta_sd), fill = "#7CAE00", alpha = 0.3 ) +
  geom_vline(xintercept = 90, linetype = 2) +
  
  labs(x="Time (Month)", y = "Fitted \u03b8", title = "LLi (intact)") +
  expand_limits(x = c(0,144), y = c(-10,5.5)) +
  
  #scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150), name = "Month",
  #                   sec.axis = sec_axis(~./12 + 2008,
  #                                       breaks = c(2008, 2010, 2012,2014,2016,2018,2020),
  #                                       name = "Year")) +
  #annotate("text", x = 14, y = -6, label="LLi (intact)") +
  geom_hline(yintercept = 1) +
  coord_cartesian(ylim = c(-10,5.5), clip = "on") +
  theme_linedraw() + 
  theme(plot.margin = unit(c(0.05, 0.05, 0.05, 0.05),"inches"))
theta_plot_LL

t_abundance_LL= ggplot(df_LL, aes(y=Theta, x = N)) +
  
  geom_errorbar(aes(ymin=Theta-Theta_sd, ymax=Theta+Theta_sd), width=15, position=position_dodge(.9),
                colour="black", alpha=0.5, size=0.5) + 
  geom_point(color = "#7CAE00") +
  geom_hline(yintercept = 1) +
  labs(x="Abundance [N]", y = "Fitted \u03b8") +
  coord_cartesian(ylim = c(-10,5.5), clip = "on") +
  theme_linedraw() +
  scale_y_continuous(limits = c(-10, 5.5),sec.axis = dup_axis()) +
  xlim(0,3300)+
  #annotate("text", x = 125, y = 2, label="Superlinear") +
  #annotate("text", x = 125, y = 0, label="Sublinear") +
  #theme(axis.title.y = element_text(vjust = 3))+ 
  theme(plot.margin = unit(c(0.05, 0.05, 0.05, 0.05),"inches"))

t_abundance_LL

####### Putting all Together #######

reso <- 300
length <- 3.25*reso/72


png(file="Fig4_260826.png",width=length/1.6, height=length/2, res = reso, units = "in");
theta_plot_CA + t_abundance_CA +
  theta_plot_LL + t_abundance_LL +
  theta_plot_TY  + t_abundance_TY +
  theta_plot_UL + t_abundance_UL +
  plot_layout(axis_titles = "collect",axes = "collect", ncol = 2, nrow =4);
dev.off()


