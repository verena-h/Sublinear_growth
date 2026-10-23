
require(ggplot2)
require(gridExtra)


rm(list=ls())

### load rdata for each stream and create the dataframe ###

##CA
load("filterout_CA_whole.rda")

var_CA =rbind(vardata$proc,
              vardata$obs,
              vardata$detvar,
              vardata$pdetvar,
              vardata$pproc,
              vardata$pobs,
              vardata$proc_2,
              vardata$obs_2,
              vardata$detvar_2,
              vardata$pdetvar_2,
              vardata$pproc_2,
              vardata$pobs_2)

var_CA = t(var_CA)
colnames(var_CA) = c("proc","obs","detvar","pdetvar","pproc","pobs",
                     "proc_2","obs_2","detvar_2","pdetvar_2","pproc_2","pobs_2")

mean_CA = c(colMeans(var_CA))
sd_CA = c(rep(NA,12))

for (i in 1:length(sd_CA)){
  tmp = sd(var_CA[,i])
  sd_CA[i] = tmp
}

q1_CA = c(rep(NA,12))
q2_CA = c(rep(NA,12))
q3_CA = c(rep(NA,12))

for (i in 1:length(q1_CA)){
  tmp = quantile(var_CA[,i], c(0.025, pnorm(-1), 0.5, pnorm(1), 0.975))[1]
  q1_CA[i] = tmp
  
  tmp = quantile(var_CA[,i], c(0.025, pnorm(-1), 0.5, pnorm(1), 0.975))[3]
  q2_CA[i] = tmp
  
  tmp = quantile(var_CA[,i], c(0.025, pnorm(-1), 0.5, pnorm(1), 0.975))[5]
  q3_CA[i] = tmp
  
}

df_CA = as.data.frame(t(rbind(mean_CA,sd_CA, q1_CA, q2_CA, q3_CA))) 

##TY
load("filterout_TY_whole.rda")

var_TY =rbind(vardata$proc,
              vardata$obs,
              vardata$detvar,
              vardata$pdetvar,
              vardata$pproc,
              vardata$pobs,
              vardata$proc_2,
              vardata$obs_2,
              vardata$detvar_2,
              vardata$pdetvar_2,
              vardata$pproc_2,
              vardata$pobs_2)

var_TY = t(var_TY)
colnames(var_TY) = c("proc","obs","detvar","pdetvar","pproc","pobs",
                     "proc_2","obs_2","detvar_2","pdetvar_2","pproc_2","pobs_2")

mean_TY = c(colMeans(var_TY))
sd_TY = c(rep(NA,12))

for (i in 1:length(sd_TY)){
  tmp = sd(var_TY[,i])
  sd_TY[i] = tmp
}

q1_TY = c(rep(NA,12))
q2_TY = c(rep(NA,12))
q3_TY = c(rep(NA,12))

for (i in 1:length(q1_TY)){
  tmp = quantile(var_TY[,i], c(0.025, pnorm(-1), 0.5, pnorm(1), 0.975))[1]
  q1_TY[i] = tmp
  
  tmp = quantile(var_TY[,i], c(0.025, pnorm(-1), 0.5, pnorm(1), 0.975))[3]
  q2_TY[i] = tmp
  
  tmp = quantile(var_TY[,i], c(0.025, pnorm(-1), 0.5, pnorm(1), 0.975))[5]
  q3_TY[i] = tmp
  
}
df_TY = as.data.frame(t(rbind(mean_TY, sd_TY, q1_TY, q2_TY, q3_TY))) 

##LL

load("filterout_LL_whole.rda")

var_LL =rbind(vardata$proc,
              vardata$obs,
              vardata$detvar,
              vardata$pdetvar,
              vardata$pproc,
              vardata$pobs,
              vardata$proc_2,
              vardata$obs_2,
              vardata$detvar_2,
              vardata$pdetvar_2,
              vardata$pproc_2,
              vardata$pobs_2)

var_LL = t(var_LL)
colnames(var_LL) = c("proc","obs","detvar","pdetvar","pproc","pobs",
                     "proc_2","obs_2","detvar_2","pdetvar_2","pproc_2","pobs_2")

mean_LL = c(colMeans(var_LL))
sd_LL = c(rep(NA,12))

for (i in 1:length(sd_LL)){
  tmp = sd(var_LL[,i])
  sd_LL[i] = tmp
}

q1_LL = c(rep(NA,12))
q2_LL = c(rep(NA,12))
q3_LL = c(rep(NA,12))

for (i in 1:length(q1_LL)){
  tmp = quantile(var_LL[,i], c(0.025, pnorm(-1), 0.5, pnorm(1), 0.975))[1]
  q1_LL[i] = tmp
  
  tmp = quantile(var_LL[,i], c(0.025, pnorm(-1), 0.5, pnorm(1), 0.975))[3]
  q2_LL[i] = tmp
  
  tmp = quantile(var_LL[,i], c(0.025, pnorm(-1), 0.5, pnorm(1), 0.975))[5]
  q3_LL[i] = tmp
  
}
df_LL = as.data.frame(t(rbind(mean_LL, sd_LL, q1_LL, q2_LL, q3_LL))) 

##UL

load("filterout_UL_whole.rda")

var_UL =rbind(vardata$proc,
              vardata$obs,
              vardata$detvar,
              vardata$pdetvar,
              vardata$pproc,
              vardata$pobs,
              vardata$proc_2,
              vardata$obs_2,
              vardata$detvar_2,
              vardata$pdetvar_2,
              vardata$pproc_2,
              vardata$pobs_2)

var_UL = t(var_UL)
colnames(var_UL) = c("proc","obs","detvar","pdetvar","pproc","pobs",
                     "proc_2","obs_2","detvar_2","pdetvar_2","pproc_2","pobs_2")

mean_UL = c(colMeans(var_UL))

sd_UL = c(rep(NA,12))


for (i in 1:length(sd_UL)){
  tmp = sd(var_UL[,i])
  sd_UL[i] = tmp
}

q1_UL = c(rep(NA,12))
q2_UL = c(rep(NA,12))
q3_UL = c(rep(NA,12))

for (i in 1:length(q1_UL)){
  tmp = quantile(var_UL[,i], c(0.025, pnorm(-1), 0.5, pnorm(1), 0.975))[1]
  q1_UL[i] = tmp
  
  tmp = quantile(var_UL[,i], c(0.025, pnorm(-1), 0.5, pnorm(1), 0.975))[3]
  q2_UL[i] = tmp
  
  tmp = quantile(var_UL[,i], c(0.025, pnorm(-1), 0.5, pnorm(1), 0.975))[5]
  q3_UL[i] = tmp
  
}
df_UL = as.data.frame(t(rbind(mean_UL, sd_UL, q1_UL, q2_UL, q3_UL))) 

#### Bind everything 

df_whole = cbind(df_CA, df_TY, df_LL, df_UL)

write.csv(df_whole,"df_Variability_Sources_Fig_3.csv")
