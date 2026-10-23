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


comp_CA = round(get_pvalue(var_CA[,1:6], var_CA[,7:12]), 4)
comp_TY = round(get_pvalue(var_TY[,1:6], var_TY[,7:12]), 4)
comp_LL = round(get_pvalue(var_LL[,1:6], var_LL[,7:12]), 4)
comp_UL = round(get_pvalue(var_UL[,1:6], var_UL[,7:12]), 4)

 
tab1 <- dplyr::bind_rows(comp_CA, comp_LL, comp_TY, comp_UL)

colnames(tab1) = c("Process Noise", "Observation Error", "Deterministic Variation","Deterministic Variation", "Process Noise", "Observation Error")
tab1 = htmlTable::htmlTable(tab1,
                            #caption ="Note: p-values for the sources of variability before and after the flooding event within streams",
                            rnames = c("CAi","LLi","TYt","ULt"),
                            cgroup = c("Absolute Variability", "Relative Variability"),
                            n.cgroup = c(3,3),
                            
                            align = "|ccc|c",
                            align.header = "|ccc|c",
                            spacer.celltype = "double_cell",
                            css.cell = "padding: 3px; padding-left: .5em; padding-right: .5em;",
                            css.header = "padding: 6px; font-weight: normal",
                            
                            pos.caption ="bottom",
                            
                            file="Tab_S2.html")
tab1
