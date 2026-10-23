library(htmlTable)
library(htmltools)
rm(list=ls())


df_sum = read.csv("Var_alpha_theta.csv")
rownames(df_sum) = c("Variance Alpha","Variance Theta")
colnames(df_sum) = c("CAi","LLi", "ULt", "TYt")
df_sum = df_sum[,-1]
df_sum = round(df_sum,3)

tab = htmlTable::htmlTable(df_sum,
                           
                           css.cell = "padding: 3px; padding-left: .5em; padding-right: .5em;",
                           file="tab6.html")
tab
