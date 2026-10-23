library(htmlTable)
library(htmltools)
rm(list=ls())

### load rdata for each stream and make dataframe ###

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
# make function
get_pvalue = function(X, Y) {
  nrows = min(c(nrow(X), nrow(Y)))
  X = X[seq(nrow(X)-nrows+1, nrow(X)),]
  Y = Y[seq(nrow(Y)-nrows+1, nrow(Y)),]
  
  p = colMeans(X < Y)
  p[p>0.5] = 1- p[p>0.5]
  p[p==0] = 1/nrows
  p
}


CA_TY = round(get_pvalue(var_CA, var_TY), 5)
CA_LL = round(get_pvalue(var_CA, var_LL), 5)
CA_UL = round(get_pvalue(var_CA, var_UL), 5)

LL_TY = round(get_pvalue(var_LL, var_TY), 5)
UL_TY = round(get_pvalue(var_UL, var_TY), 5)
UL_LL = round(get_pvalue(var_UL, var_LL), 5)

### make the tables 

tab2a <- dplyr::bind_rows(CA_LL[1:6], CA_UL[1:6], CA_TY[1:6], LL_TY[1:6], UL_LL[1:6], UL_TY[1:6])
tab2b <- dplyr::bind_rows(CA_LL[7:12], CA_UL[7:12], CA_TY[7:12], LL_TY[7:12], UL_LL[7:12], UL_TY[7:12])

colnames(tab2a) = c("Process Noise", "Observation Error", "Deterministic Variation","Deterministic Variation", "Process Noise", "Observation Error")
colnames(tab2b) = c("Process Noise", "Observation Error", "Deterministic Variation","Deterministic Variation", "Process Noise", "Observation Error")

tab2 = rbind(tab2a,tab2b)

# Define CSS styles for each column

tab2 = htmlTable::htmlTable(tab2,
                            #caption ="Note: p-values for the parameter estimates before and after the flooding event between streams",
                            rnames = rep(c("CAi with LLi ","CAi with ULt ","CAi with TYt ","LLi with TYt ", "ULt with LLi ","ULt with TYt "),2),
                            cgroup = c("Absolute Variability", "Relative Variability"),
                            n.cgroup = c(3,3),
                            rgroup = c("Before Flooding Event","After Flooding Event"),
                            n.rgroup = c(6,6),
                            
                            pos.caption ="bottom",
                            align = "cccc",
                            align.header = "cccc",
                            #spacer.celltype = "double_cell",
                            css.cell = "padding: 3px",
                            css.header = "padding: 6px; font-weight: normal",
                            file="Tab_S3.html")
tab2

