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


# check p-value for comparing streams
CA_LL = round(get_pvalue(smp_CA, smp_LL), 5)
CA_UL = round(get_pvalue(smp_CA, smp_UL), 5)
CA_TY = round(get_pvalue(smp_CA, smp_TY), 5)

LL_UL = round(get_pvalue(smp_LL, smp_UL), 5)
LL_TY = round(get_pvalue(smp_LL, smp_TY), 5)

UL_TY = round(get_pvalue(smp_UL, smp_TY), 5)


tab5 <- dplyr::bind_rows(CA_LL, CA_UL, CA_TY, LL_UL, LL_TY, UL_TY)
colnames(tab5) = rep(c("Beta 3","Beta 0","Beta 1"),2)

# Define CSS styles for each column

tab5 = htmlTable::htmlTable(tab5,
                            #caption ="Note: p-values for the parameter estimates before and after the flooding event between streams",
                            rnames = c("CAi with LLi ","CAi with ULt ","CAi with TYt ","LLi with ULt ", "LLi with TYt ","ULt with TYt "),
                            cgroup = c("Before Flooding Event", "After Flooding Event"),
                            n.cgroup = c(3,3),
                            
                            pos.caption ="bottom",
                            align = "|ccc|c",
                            align.header = "|ccc|c",
                            spacer.celltype = "double_cell",
                            css.cell = "padding: 3px; padding-left: .5em; padding-right: .5em;",
                            css.header = "padding: 6px; font-weight: normal",
                            file="Tab_S6.html")
tab5
