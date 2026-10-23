library(ggplot2)
library(grid)
library(dplyr)
library(lubridate)
library(htmlTable)
library(htmltools)

rm(list=ls())
load("Theta_est_CA.rda")
coefmat_ca = data.frame(coefmat)
coefmat_ca$Stream = "CA"
df_ca = df 

load("Theta_est_TY.rda")
coefmat_ty = data.frame(coefmat)
df_ty = df
coefmat_ty$Stream = "TY"

load("Theta_est_LL.rda")
coefmat_ll = data.frame(coefmat[13:144,])
df_ll = df
coefmat_ll$Stream = "LL"

load("Theta_est_UL.rda")
coefmat_ul = data.frame(coefmat[13:144,])
df_ul = df
coefmat_ul$Stream = "UL"

coefmat = rbind(coefmat_ca,coefmat_ll,coefmat_ty, coefmat_ul)
colnames(coefmat)[1:4] = c("Alpha","Theta","AlphaSD", "ThetaSD")

## alpha 
boxplot(Alpha ~ Stream, data=coefmat) 
alp_ca_ty = wilcox.test(Alpha ~ Stream, data=coefmat[coefmat$Stream == "CA" |coefmat$Stream == "TY",], paired = TRUE) 

alp_ca_ll = wilcox.test(Alpha ~ Stream, data=coefmat[coefmat$Stream == "CA" |coefmat$Stream == "LL",], paired = TRUE, na.action = getOption("na.pass"))  
 
alp_ca_ul = wilcox.test(Alpha ~ Stream, data=coefmat[coefmat$Stream == "CA" |coefmat$Stream == "UL",], paired = TRUE, na.action = getOption("na.pass")) 

alp_ll_ty = wilcox.test(Alpha ~ Stream, data=coefmat[coefmat$Stream == "LL" |coefmat$Stream == "TY",], paired = TRUE, na.action = getOption("na.pass")) 

alp_ul_ty = wilcox.test(Alpha ~ Stream, data=coefmat[coefmat$Stream == "UL" |coefmat$Stream == "TY",], paired = TRUE, na.action = getOption("na.pass")) 

alp_ll_ul = wilcox.test(Alpha ~ Stream, data=coefmat[coefmat$Stream == "LL" |coefmat$Stream == "UL",], paired = TRUE, na.action = getOption("na.pass")) 


## theta 
boxplot(Theta ~ Stream, data=coefmat) 
th_ca_ty = wilcox.test(Theta ~ Stream, data=coefmat[coefmat$Stream == "CA" |coefmat$Stream == "TY",], paired = TRUE, na.action = getOption("na.pass")) 

th_ca_ll = wilcox.test(Theta ~ Stream, data=coefmat[coefmat$Stream == "CA" |coefmat$Stream == "LL",], paired = TRUE, na.action = getOption("na.pass")) 

th_ca_ul = wilcox.test(Theta ~ Stream, data=coefmat[coefmat$Stream == "CA" |coefmat$Stream == "UL",], paired = TRUE, na.action = getOption("na.pass")) 

th_ll_ty = wilcox.test(Theta ~ Stream, data=coefmat[coefmat$Stream == "LL" |coefmat$Stream == "TY",], paired = TRUE, na.action = getOption("na.pass")) 
#not significant
th_ul_ty = wilcox.test(Theta ~ Stream, data=coefmat[coefmat$Stream == "UL" |coefmat$Stream == "TY",], paired = TRUE, na.action = getOption("na.pass")) 

th_ll_ul = wilcox.test(Theta ~ Stream, data=coefmat[coefmat$Stream == "LL" |coefmat$Stream == "UL",], paired = TRUE, na.action = getOption("na.pass")) 



tab8a = c(alp_ca_ll[["p.value"]], alp_ca_ul[["p.value"]], alp_ca_ty[["p.value"]],
          alp_ll_ul[["p.value"]], alp_ll_ty[["p.value"]], alp_ul_ty[["p.value"]])

tab8th = c(th_ca_ll[["p.value"]], th_ca_ul[["p.value"]], th_ca_ty[["p.value"]],
            th_ll_ul[["p.value"]], th_ll_ty[["p.value"]], th_ul_ty[["p.value"]])

tab8 = data.frame(matrix(nrow = 6, ncol = 2))
colnames(tab8) = c("Alpha","Theta")
tab8$Alpha = tab8a
tab8$Theta = tab8th

rownames(tab8) = c("CAi with LLi ","CAi with ULt ","CAi with TYt ","LLi with ULt ", "LLi with TYt ","ULt with TYt ")
tab8 = round(tab8,3)
tab8$Alpha[tab8$Alpha == 0] = "<0.001"
tab8$Theta[tab8$Theta == 0] = "<0.001"
# Define CSS styles for each column

tab8_html = htmlTable::htmlTable(tab8,
                            #caption ="Note: p-values for the parameter estimates before and after the flooding event between streams",
                            rnames = c("CAi with LLi ","CAi with ULt ","CAi with TYt ","LLi with ULt ", "LLi with TYt ","ULt with TYt "),
                            #cgroup = c("Before Flooding Event", "After Flooding Event"),
                            #n.cgroup = c(3,3),
                            
                            #pos.caption ="bottom",
                            align = "|c|c",
                            align.header = "|c|c",
                            spacer.celltype = "double_cell",
                            css.cell = "padding: 3px; padding-left: .5em; padding-right: .5em;",
                            css.header = "padding: 6px",
                            file="Tab_S8.html")
tab8_html
