require(ggplot2)
require(gridExtra)


rm(list=ls())

df_whole = read.csv("df_Variability_Sources_Fig_3.csv")
row.names(df_whole) = df_whole$X
df_whole = df_whole[,-1]
df_red = df_whole[,c(-2,-4,-7,-9,-12,-14,-17,-19)]

colnames(df_red) = c("Mean", "q1", "q3",
                     "Mean", "q1", "q3",
                     "Mean", "q1", "q3",
                     "Mean", "q1", "q3")

#df_red = round(as.data.frame(df_red),3)

df_red[c(1,3:5,7,9:11),] = round(as.data.frame(df_red[c(1,3:5,7,9:11),]),3)
df_red[c(2,6,8,12),] = round(as.data.frame(df_red[c(2,6,8,12),]),6) 


tab1 = htmlTable::htmlTable(df_red,
                            rnames = rep(c("Process Noise","Observation Error","Deterministic Variation",
                                           "Deterministic Variation [%]", "Process Noise [%]","Observation Error [%]"),2),
                            
                            n.rgroup = c(6,6), 
                            rgroup = c("Before Flooding Event","After Flooding Event"),
                                
                            n.cgroup = c(3,3,3,3),
                            
                            cgroup = c("CAi", "TYt", "LLi", "ULt"),
                            
                            align = "|ccc|ccc|ccc|ccc",
                            align.header = "|ccc|ccc|ccc|ccc",
                            
                            css.cell = "padding: 3px; padding-left: .5em; padding-right: .5em;",
                            css.header = "padding: 3px; font-weight: normal",
                            
                            file="Tab1.html")
tab1
