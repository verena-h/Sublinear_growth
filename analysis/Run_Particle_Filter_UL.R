
require(BayesianTools)
require(pttstability)

rm(list=ls())

#guppy data
summary <- read.csv("data.csv")

unique(summary$stream)
UL<- summary[summary$stream=="UL",]
plot(1:length(UL$F.N),UL$F.N)
points(1:length(UL$M.N), UL$M.N, col="red")

UL_FM <- UL[, c("F.N", "M.N")]
UL_FM$y<- rowSums(UL_FM)

y <- UL_FM$y

plot(1:length(y), y)

t1 = 1:90
t2 = 91:144
abline(v=max(t1), lty = 2)

y_m <- y/mean(y) #standardize

#S-Mapping
sout<-data.frame(E = 2:4, theta = NA, RMSE = NA)
for(i in 1:nrow(sout)) {
  optout = optimize(f = function(x) {S_map_Sugihara1994(Y = y, E = sout$E[i],
                                                        theta = x)$RMSE}, interval = c(0,10))
  sout$theta[i] = optout$minimum
  sout$RMSE[i] = optout$objective
}
tuse<-sout$theta[which.min(sout$RMSE)] # find theta (nonlinerity) parameter
Euse<-sout$E[which.min(sout$RMSE)] # find embedding dimension
spred<-S_map_Sugihara1994(Y = y, E = Euse, theta = tuse) # fit S-mapping for best paramter set
plot(spred$Y, spred$Y_hat); abline(a=0, b=1, lty=2) # observed vs. predicted


## Run optimizers

#create priors

minvUSE_edm <- c(-7, -3.5, -4, -7, -3.5, -4) #minimum interval for obs and proc, in time 1 and 2
maxvUSE_edm <- c(0, 0, 1, 0, 0, 1) #maximum interval for obs and proc, in time 1 and 2


#density, sampler, and prior functions for EDM function
density_fun_USE_edm<-function(param) density_fun0(param = param,
                                                  minv = minvUSE_edm, maxv=maxvUSE_edm)
sampler_fun_USE_edm<-function(x) sampler_fun0(n = 1, minv = minvUSE_edm, maxv=maxvUSE_edm)
prior_edm <- createPrior(density = 
                           density_fun_USE_edm, sampler = sampler_fun_USE_edm,
                         lower = minvUSE_edm, upper = maxvUSE_edm)
## Run filter


niter<-20000 #number of steps for the MCMC sampler
N<-3e4 #number of particles
Euse<-Euse #number of embedding dimensions

## Load updated particle filter functions
source("Functions.R")


#likelihood and bayesian set-ups for EDM functions
likelihood_EDM<-function(x) {
  likelihood0(param = x, y=y_m, parseparam = parseparam0, procfun = procfun0, obsfun = obsfun0,
              detfun = EDMfun0, edmdat = list(E=Euse, theta=tuse), N = N)
}

bayesianSetup_EDM <- createBayesianSetup(likelihood = likelihood_EDM, prior = prior_edm)


# #run MCMC optimization
# out_EDM <- runMCMC(bayesianSetup = bayesianSetup_EDM,
#                      settings = list(iterations=niter, consoleUpdates=1))
#  
# 
# save(out_EDM, file="out_EDM_UL_TOTAL_varProc.RData") #use the same niter and N as remaining streams but is useless, new save with extended runs
# load("out_EDM_UL_TOTAL_varProc.RData")
# out_EDM_2 <- runMCMC(bayesianSetup = out_EDM,
#                    settings = list(iterations=niter, consoleUpdates=1))
#  
# save(out_EDM_2, file="out_EDM_2_UL_TOTAL_varProc.RData")
# load("out_EDM_2_UL_TOTAL_varProc.RData")
# 
# out_EDM_3 <- runMCMC(bayesianSetup = out_EDM_2,
#                      settings = list(iterations=niter, consoleUpdates=1))
# 
# save(out_EDM_3, file="out_EDM_3_UL_TOTAL_varProc.RData")

#Load the MCMC chain 
load("out_EDM_3_UL_TOTAL_varProc.RData")



## extract and plot parameter distributions
start <- 5000
thin = 2

## extract and plot parameter distributions
smp_EDM<-getSample(out_EDM_3, start=start, thin = thin) #decided to take 3 

hist(exp(smp_EDM[,1]), main="EDM function", xlab="obs, t1", breaks = 20)
hist(exp(smp_EDM[,2]), main="EDM function", xlab="proc, t1", breaks = 20)

hist(exp(smp_EDM[,4]), main="EDM function", xlab="obs, t2", breaks = 20)
hist(exp(smp_EDM[,5]), main="EDM function", xlab="proc, t2", breaks = 20)
marginalPlot(out_EDM_2, prior = TRUE)


summary(out_EDM)


pars_est = parseparam0(unname(colMeans(smp_EDM)))
filterout_est<-particleFilterLL(y=y_m, pars=pars_est,procfun = procfun0,
                                detfun = EDMfun0, edmdat = list(E=Euse, theta=tuse), N = N,
                                dotraceback = TRUE, fulltraceback = TRUE)

#calculate variance 
var1 = (sd(y_m[1:90]))^2
var2 = (sd(y_m[91:144]))^2

##### before flooding #######

#calculate process noise, realized and theoretical 
proc_t = matrix(data=NA, nrow = length(smp_EDM[,2]), ncol = 1)

for(i in 1:length(smp_EDM[,2])) {
  proc_t[i,] = sqrt(mean(exp(smp_EDM[i,2])*filterout_est$Nest_noproc[1:90]^exp(smp_EDM[i,3]), na.rm=TRUE))
}

proc = var(filterout_est$Nest[1:90]-filterout_est$Nest_noproc[1:90], na.rm=TRUE) # realised process noise
hist(proc_t)
proc_t = (mean(proc_t^2)) # theoretical process noise



#calculate obs error 
obs = var(filterout_est$Nest[1:90]-y_m[1:90], na.rm=TRUE) # realised observation error
obs_t = (mean(exp(smp_EDM[,1])))^2 #theoretical 

#calculate det 
detvar = var1-proc-obs

#calculate percent 
pdetvar = detvar/var1*100
pproc = proc/var1*100
pobs = obs/var1*100

###### after flooding #########

#calculate process noise, realized and theoretical 
proc_2t = matrix(data=NA, nrow = length(smp_EDM[,5]), ncol = 1)

for(i in 1:length(smp_EDM[,5])) {
  proc_2t[i,] = sqrt(mean(exp(smp_EDM[i,5])*filterout_est$Nest_noproc[91:144]^exp(smp_EDM[i,6]), na.rm=TRUE))
}

proc_2 = var(filterout_est$Nest[91:144]-filterout_est$Nest_noproc[91:144], na.rm=TRUE) # realised process noise
hist(proc_2t)
proc_2t = (mean(proc_2t^2)) # theoretical process noise, not sure about the sqrt but ok 



#calculate obs error 
obs_2 = var(filterout_est$Nest[91:144]-y_m[91:144], na.rm=TRUE) # realised observation error
obs_2t = (mean(exp(smp_EDM[,4])))^2 #theoretical 

#calculate det 
detvar_2 = var2-proc_2-obs_2

#calculate percent 
pdetvar_2 = detvar_2/var2*100
pproc_2 = proc_2/var2*100
pobs_2 = obs_2/var2*100

vardata = as.data.frame(c(var1,proc,obs,detvar,pdetvar,pproc,pobs,
                          var2,proc_2,obs_2,detvar_2,pdetvar_2,pproc_2,pobs_2),
                        row.names = c("var1","proc","obs","detvar","pdetvar","pproc","pobs",
                                      "var2","proc_2","obs_2","detvar_2","pdetvar_2","pproc_2","pobs_2"))
colnames(vardata)="UL"

save(list=c("filterout_est","pars_est","y_m","y","vardata","smp_EDM"),file="filterout_UL.rda")

