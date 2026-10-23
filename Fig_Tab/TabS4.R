require(BayesianTools)
require(pttstability)
library(htmlTable)
library(htmltools)


rm(list=ls())

#### create data frame for all streams ####

# #CA

load("out_EDM_2_CA_TOTAL_varProc.RData")
out_CA = out_EDM_2
rm(out_EDM_2)
#LL
load("out_EDM_LL_TOTAL_varProc.RData")
out_LL = out_EDM
rm(out_EDM)
#UL

load("out_EDM_3_UL_TOTAL_varProc.RData")
out_UL=out_EDM_3
rm(out_EDM_3)
#TY
load("out_EDM_4_TY_TOTAL_varProc.RData")
out_TY=out_EDM_4
rm(out_EDM_4)

#get samples 
smp_CA = getSample(out_CA, start = 2e3)
smp_LL = getSample(out_LL, start = 2e3)
smp_UL = getSample(out_UL, start = 2e3)
smp_TY = getSample(out_TY, start = 2e3)

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

# check p-value for before vs. after disturbance
comp_CA = round(get_pvalue(smp_CA[,1:3], smp_CA[,4:6]), 5)
comp_LL = round(get_pvalue(smp_LL[,1:3], smp_LL[,4:6]), 5)
comp_UL = round(get_pvalue(smp_UL[,1:3], smp_UL[,4:6]), 5)
comp_TY = round(get_pvalue(smp_TY[,1:3], smp_TY[,4:6]), 5)


tab1 <- dplyr::bind_rows(comp_CA, comp_LL, comp_TY, comp_UL)

colnames(tab1) = c("Beta 3", "Beta 0", "Beta 1")
tab1 = htmlTable::htmlTable(tab1,
                            #caption ="Note: p-values for the parameter estimates before and after the flooding event within streams",
                            rnames = c("CAi","LLi","TYt","ULt"),
                            
                            pos.caption ="bottom",
                            css.cell = "padding: 3px;",
                            
                            file="Tab_S5.html")
tab1
