library(htmlTable)
library(htmltools)
rm(list=ls())

#### create data frame for all streams ####

# #CA
load("Theta_est_CA.rda")
df_CA = df
co_CA = coefmat

#LL
load("Theta_est_LL.rda")
df_LL = df 
co_LL = coefmat
#UL

load("Theta_est_UL.rda")
df_UL = df 
co_UL = coefmat

#TY
load("Theta_est_TY.rda")
df_TY = df
co_TY = coefmat



# #bind everything 
df_sum=cbind(df_CA,df_LL,
             df_UL,df_TY)

rownames(df_sum) = c("Variance Alpha", "Variance Theta")

write.csv(df_sum,"Var_alpha_theta.csv")


