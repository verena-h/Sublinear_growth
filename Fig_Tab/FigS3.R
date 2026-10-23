
require(ggplot2)
library(ggpubr)
library(patchwork)
library(ggbreak)

rm(list=ls())

#### Load and Prep Data ####
#load CA
load("filterout_CA.rda")
load("Theta_est_CA.rda")
tm = c(1:132)
df_CA = as.data.frame(cbind(tm,coefmat))
df_CA_noy = data.frame(matrix(data =NA, nrow =144, ncol =5))
df_CA_noy[1:12,2:5] = NA
df_CA_noy[13:144,2:5] = df_CA[,2:5]
df_CA_noy[,1] = rownames(df_CA_noy)
colnames(df_CA_noy) = c("Time","Alpha","Theta","Alpha_sd","Theta_sd")
df_CA_noy$Time = as.numeric(df_CA_noy$Time)
#load TY
load("filterout_TY.rda")
load("Theta_est_TY.rda")
tm = c(1:132)
df_TY = as.data.frame(cbind(tm,coefmat))
df_TY_noy = data.frame(matrix(data =NA, nrow =144, ncol =5))
df_TY_noy[1:12,2:5] = NA
df_TY_noy[13:144,2:5] = df_TY[,2:5]
df_TY_noy[,1] = rownames(df_TY_noy)
colnames(df_TY_noy) = c("Time","Alpha","Theta","Alpha_sd","Theta_sd")
df_TY_noy$Time = as.numeric(df_TY_noy$Time)
#load LL
load("filterout_LL.rda")
load("Theta_est_LL.rda")
tm = c(1:144)
df_LL = as.data.frame(cbind(tm,coefmat))
colnames(df_LL) = c("Time","Alpha","Theta","Alpha_sd","Theta_sd")
df_LL$Time = as.numeric(df_LL$Time)
#load UL
load("filterout_UL.rda")
load("Theta_est_UL.rda")
tm = c(1:144)
df_UL = as.data.frame(cbind(tm,coefmat))
colnames(df_UL) = c("Time","Alpha","Theta","Alpha_sd","Theta_sd")
df_UL$Time = as.numeric(df_UL$Time)

#### Function for population growth at abundance 0.5 (N/K) ####
alpha_p = function(x,y,n) {
  alpha = (y/x)*(1-(n/1)^x)
  return(alpha)
}

df_CA_noy$growth_05 = alpha_p(x=df_CA_noy$Theta, y = exp(df_CA_noy$Alpha), n = 0.5)

df_TY_noy$growth_05 = alpha_p(x=df_TY_noy$Theta, y = exp(df_TY_noy$Alpha), n = 0.5)

df_LL$growth_05 = alpha_p(x=df_LL$Theta, y = exp(df_LL$Alpha), n = 0.5)

df_UL$growth_05 = alpha_p(x=df_UL$Theta, y = exp(df_UL$Alpha), n = 0.5)

##### Plotting CA ######

alpha_plot_CA = ggplot(df_CA_noy, aes(y=exp(Alpha), x = Time)) +
  #geom_errorbar(aes(ymin = exp(Alpha-Alpha_sd), ymax = exp(Alpha+Alpha_sd)), width = 1.5) +
  #geom_point(color = "#F8766D") +
  geom_line(color = "#F8766D", linewidth = 0.6) +
  geom_ribbon(aes(ymin=exp(Alpha-Alpha_sd), ymax=exp(Alpha+Alpha_sd)), fill = "#F8766D", alpha = 0.3 ) +
  
  geom_vline(xintercept = 90, linetype = 2) +
  
  labs(x="Month", y = "Fitted \u03b1", title = "CAi (intact)") +
  expand_limits(x = c(0,144), y = c(0,2)) +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150), name = "Month",
                     sec.axis = sec_axis(~./12 + 2008,
                                         breaks = c(2008, 2010, 2012,2014,2016,2018,2020),
                                       name = "Year")) +
  
  
  theme_linedraw() + 
  coord_cartesian(ylim = c(0,2), clip = "on") +
  theme(plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "inches")) 

alpha_plot_CA

alpha_p_CA = ggplot(df_CA_noy, aes(y= growth_05, x = Time)) +
  #geom_point(color = "#F8766D") +
  geom_line(color = "#F8766D", linewidth = 0.6) +
  
  geom_vline(xintercept = 90, linetype = 2) +
  
  labs(x="Month", y = "Population Growth Rate") +
  
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150), name = "Month",
                     sec.axis = sec_axis(~./12 + 2008,
                                         breaks = c(2008, 2010, 2012,2014,2016,2018,2020),
                                         name = "Year")) +
  
  coord_cartesian(ylim = c(0,9), clip = "on") +
  theme_linedraw() + 
  theme(plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "inches")) 
alpha_p_CA


##### Plotting TY ######

alpha_plot_TY = ggplot(df_TY_noy, aes(y=exp(Alpha), x = Time)) +
  #geom_errorbar(aes(ymin = exp(Alpha-Alpha_sd), ymax = exp(Alpha+Alpha_sd)), width = 1.5) +
  #geom_point(color = "#00BFC4") +
  geom_line(color = "#00BFC4", linewidth = 0.6) +
  geom_ribbon(aes(ymin=exp(Alpha-Alpha_sd), ymax=exp(Alpha+Alpha_sd)), fill = "#00BFC4", alpha = 0.3 ) +
  geom_vline(xintercept = 90, linetype = 2) +
  
  labs(x="Month", y = "Fitted \u03b1", title = "TYt (thinned)") +
  expand_limits(x = c(0,144), y = c(0,2)) +
  ylim(0,2.20)+
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150), name = "Month",
                     sec.axis = sec_axis(~./12 + 2008,
                                         breaks = c(2008, 2010, 2012,2014,2016,2018,2020),
                                         name = "Year")) +
  
  
  theme_linedraw() + 
  coord_cartesian(ylim = c(0,2), clip = "on") +
  theme(plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "inches")) 
alpha_plot_TY

alpha_p_TY = ggplot(df_TY_noy, aes(y= growth_05, x = Time)) +
  #geom_point(color = "#00BFC4") +
  geom_line(color = "#00BFC4", linewidth = 0.6) +
  geom_vline(xintercept = 90, linetype = 2) +
  
  labs(x="Month", y = "Population Growth Rate") +
  
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150), name = "Month",
                     sec.axis = sec_axis(~./12 + 2008,
                                         breaks = c(2008, 2010, 2012,2014,2016,2018,2020),
                                         name = "Year")) +
  
  coord_cartesian(ylim = c(0,9), clip = "on") +
  theme_linedraw() + 
  theme(plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "inches")) 
alpha_p_TY

##### Plotting UL ######

alpha_plot_UL = ggplot(df_UL, aes(y=exp(Alpha), x = Time)) +
  #geom_errorbar(aes(ymin = exp(Alpha-Alpha_sd), ymax = exp(Alpha+Alpha_sd)), width = 1.5) +
  #geom_point(color = "#C77CFF") +
  geom_line(color = "#C77CFF", linewidth = 0.6) +
  geom_ribbon(aes(ymin=exp(Alpha-Alpha_sd), ymax=exp(Alpha+Alpha_sd)), fill = "#C77CFF", alpha = 0.3 ) +
  geom_vline(xintercept = 90, linetype = 2) +
  
  labs(x="Month", y = "Fitted \u03b1", title = "ULt (thinned)") +
  expand_limits(x = c(0,144), y = c(0,2)) +
  
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150), name = "Month",
                     sec.axis = sec_axis(~./12 + 2008,
                                         breaks = c(2008, 2010, 2012,2014,2016,2018,2020),
                                         name = "Year")) +
  
  theme_linedraw() + 
  coord_cartesian(ylim = c(0,2), clip = "on") +
  theme(plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "inches")) 
alpha_plot_UL

alpha_p_UL = ggplot(df_UL, aes(y= growth_05, x = Time)) +
  #geom_point(color = "#C77CFF") +
  geom_line(color = "#C77CFF",linewidth = 0.6) +
  geom_vline(xintercept = 90, linetype = 2) +
  
  labs(x="Month", y = "Population Growth Rate") +
  
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150), name = "Month",
                     sec.axis = sec_axis(~./12 + 2008,
                                         breaks = c(2008, 2010, 2012,2014,2016,2018,2020),
                                         name = "Year")) +
  
  coord_cartesian(ylim = c(0,9), clip = "on") +
  theme_linedraw() + 
  theme(plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "inches")) 
alpha_p_UL

##### Plotting LL ######

alpha_plot_LL = ggplot(df_LL, aes(y=exp(Alpha), x = Time)) +
  #geom_errorbar(aes(ymin = exp(Alpha-Alpha_sd), ymax = exp(Alpha+Alpha_sd)), width = 1.5) +
  #geom_point(color = "#7CAE00") +
  geom_line(color = "#7CAE00", linewidth = 0.6) +
  geom_ribbon(aes(ymin=exp(Alpha-Alpha_sd), ymax=exp(Alpha+Alpha_sd)), fill = "#7CAE00", alpha = 0.3 ) +
  geom_vline(xintercept = 90, linetype = 2) +
  
  labs(x="Month", y = "Fitted \u03b1", title = "LLi (intact)") +
  expand_limits(x = c(0,144), y = c(0,2)) +
  ylim(0,2)+
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150), name = "Month",
                     sec.axis = sec_axis(~./12 + 2008,
                                         breaks = c(2008, 2010, 2012,2014,2016,2018,2020),
                                         name = "Year")) +
  coord_cartesian(ylim = c(0,2), clip = "on") +
  theme_linedraw() + 
  theme(plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "inches")) 
alpha_plot_LL

alpha_p_LL = ggplot(df_LL, aes(y= growth_05, x = Time)) +
  #geom_point(color = "#7CAE00") +
  geom_line(color = "#7CAE00",linewidth = 0.6) +
  geom_vline(xintercept = 90, linetype = 2) +
  
  labs(x="Month", y = "Population Growth Rate") +
  
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150), name = "Month",
                     sec.axis = sec_axis(~./12 + 2008,
                                         breaks = c(2008, 2010, 2012,2014,2016,2018,2020),
                                         name = "Year")) +
  
  coord_cartesian(ylim = c(0,9), clip = "on") +
  theme_linedraw() + 
  theme(plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "inches")) 
alpha_p_LL


####### Putting all Together #######

reso <- 300
length <- 3.25*reso/72


png(file="FigS3_260826.png",width=length/1.5, height=length/2, res = reso, units = "in");

alpha_plot_CA + alpha_p_CA +
  alpha_plot_LL  + alpha_p_LL +
  alpha_plot_TY + alpha_p_TY +
  alpha_plot_UL +  alpha_p_UL +
  plot_layout(axis_titles = "collect",axes = "collect",
              ncol = 2, nrow =4);
dev.off()


