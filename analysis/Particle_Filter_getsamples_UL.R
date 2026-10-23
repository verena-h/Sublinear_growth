
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
## No need to Run filter
#Just load rda

niter<-20000 #number of steps for the MCMC sampler
N<-2e4 #number of particles
Euse<-Euse #number of embedding dimensions

## Load updated particle filter functions
source("Functions.R")

#likelihood and bayesian set-ups for EDM functions
likelihood_EDM<-function(x) {
  likelihood0(param = x, y=y_m, parseparam = parseparam0, procfun = procfun0, obsfun = obsfun0,
              detfun = EDMfun0, edmdat = list(E=Euse, theta=tuse), N = N)
}

bayesianSetup_EDM <- createBayesianSetup(likelihood = likelihood_EDM, prior = prior_edm)


#load MCMC optimization

load("out_EDM_3_UL_TOTAL_varProc.RData")

#calculate variance 
var1 = (sd(y_m[1:90]))^2
var2 = (sd(y_m[91:144]))^2

## extract and plot parameter distributions
start <- 5000
thin = 2


smp_EDM<-getSample(out_EDM, start=start, thin = thin)
filterout_est = list(LL = rep(NA,nrow(smp_EDM)),
                     LLlst = matrix(data =NA, nrow = nrow(smp_EDM), ncol = 144),
                     Nest = matrix(data =NA, nrow = nrow(smp_EDM), ncol = 144),
                     Nsd = matrix(data =NA, nrow = nrow(smp_EDM), ncol = 144),
                     Nest_noproc = matrix(data =NA, nrow = nrow(smp_EDM), ncol = 144),
                     Nsd_noproc = matrix(data =NA, nrow = nrow(smp_EDM), ncol = 144))

obs = rep(NA,nrow(smp_EDM))
proc = rep(NA,nrow(smp_EDM))

obs_2 = rep(NA,nrow(smp_EDM))
proc_2 = rep(NA,nrow(smp_EDM))

detvar = rep(NA,nrow(smp_EDM))
detvar_2 = rep(NA,nrow(smp_EDM))

pdetvar = rep(NA,nrow(smp_EDM))
pproc = rep(NA,nrow(smp_EDM))
pobs = rep(NA,nrow(smp_EDM))

pdetvar_2 = rep(NA,nrow(smp_EDM))
pproc_2 = rep(NA,nrow(smp_EDM))
pobs_2 = rep(NA,nrow(smp_EDM))

for(i in 1:nrow(smp_EDM)){
  tmp<-particleFilterLL(y=y_m, pars=parseparam0(smp_EDM[i,]), procfun = procfun0,
                        detfun = EDMfun0, edmdat = list(E=Euse, theta=tuse), N = N,
                        dotraceback = TRUE, fulltraceback = TRUE)
  
  
  tobs = var(tmp$Nest[1:83]-y_m[1:83], na.rm=TRUE) # realised observation error
  tproc = var(tmp$Nest[1:83]-tmp$Nest_noproc[1:83], na.rm=TRUE) # realised process noise
  
  tdetvar = var1-tproc-tobs
  
  #calculate percent
  tpdetvar = tdetvar/var1*100
  tpproc = tproc/var1*100
  tpobs = tobs/var1*100
  
  tproc_2 = var(tmp$Nest[84:144]-tmp$Nest_noproc[84:144], na.rm=TRUE) # realised process noise
  tobs_2 = var(tmp$Nest[84:144]-y_m[84:144], na.rm=TRUE) # realised observation error
  
  tdetvar_2 = var2-tproc_2-tobs_2
  
  #calculate percent
  tpdetvar_2 = tdetvar_2/var2*100
  tpproc_2 = tproc_2/var2*100
  tpobs_2 = tobs_2/var2*100
  
  obs[i] = tobs
  proc[i] = tproc
  
  obs_2[i] = tobs_2
  proc_2[i] = tproc_2
  
  detvar[i] = tdetvar
  detvar_2[i] = tdetvar_2
  
  pdetvar[i] = tpdetvar
  pproc[i] = tpproc
  pobs[i] = tpobs
  
  pdetvar_2[i] = tpdetvar_2
  pproc_2[i] = tpproc_2
  pobs_2[i] = tpobs_2
  
  filterout_est$LL[i]=tmp$LL
  filterout_est$LLlst[i,]=tmp$LLlst
  filterout_est$Nest[i,] = tmp$Nest
  filterout_est$Nsd[i,] = tmp$Nsd
  filterout_est$Nest_noproc[i,] = tmp$Nest_noproc
  filterout_est$Nsd_noproc[i,] = tmp$Nsd_noproc
  
  print(paste(i,"out of",nrow(smp_EDM),sep=" "))
}


#save var sources in a list 
vardata = list(
  var1 = var1,
  proc = proc,
  obs = obs,
  detvar = detvar,
  pdetvar = pdetvar,
  pproc = pproc,
  pobs = pobs,
  var2 = var2,
  proc_2 = proc_2,
  obs_2 = obs_2,
  detvar_2 = detvar_2,
  pdetvar_2 = pdetvar_2,
  pproc_2 = pproc_2,
  pobs_2 = pobs_2) 


save(list=c("filterout_est","y_m","y","vardata","smp_EDM"),file="filterout_UL_whole.rda")

