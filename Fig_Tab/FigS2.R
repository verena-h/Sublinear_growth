
require(ggplot2)
require(gridExtra)


rm(list=ls())

### load rdata for each stream and make dataframe ###

##CA
load("filterout_CA_whole.rda")
smp_ca = smp_EDM
smp_ca = data.frame(smp_ca)
smp_ca$stream = "CAi"
##TY
load("filterout_TY_whole.rda")
smp_ty = smp_EDM
smp_ty = data.frame(smp_ty)
smp_ty$stream = "TYt"
##LL
load("filterout_LL_whole.rda")
smp_ll = smp_EDM
smp_ll = data.frame(smp_ll)
smp_ll$stream = "LLi"
##UL
load("filterout_UL_whole.rda")
smp_ul = smp_EDM
smp_ul = data.frame(smp_ul)
smp_ul$stream = "ULt"

df = rbind(smp_ca,smp_ty,smp_ll,smp_ul)
colnames(df)

parf = function(x,y) {
  tmp = x[,c(y,7)]
  tmpn = paste("Par",y,sep="_")
  tmp$Par = tmpn
  colnames(tmp)[1:2] = c("Est","Stream")
  return(tmp)
}

df1 = parf(df,1)
df2 = parf(df,2)
df3 = parf(df,3)
df4 = parf(df,4)
df5 = parf(df,5)
df6 = parf(df,6)

df_par = rbind(df1,df2,df3,df4,df5,df6)

par1 = ggplot(data = df[,c(1,7)], aes(x = par.1)) + 
  geom_bar(position = "dodge",color = "grey35") + 
  
  facet_wrap(~stream, scales = "free", dir = "v", 
             strip.position = "top") +
  scale_x_binned(n.breaks = 15, show.limit = TRUE) +
  labs( y = "Frequency", x = "Estimation par1")
par1

boxpar1 = ggplot(data = df[,c(1,7)], aes(y = par.1, x = stream)) + 
  geom_boxplot(position = "dodge",color = "black", notch = TRUE, orientation = "x", aes(fill = stream), alpha = 0.85) + 
  scale_color_viridis_d("Stream", aesthetics = "fill") +
  labs( y = "Estimation par1", x = "")
boxpar1

colnames(df_par)
box = ggplot(data = df_par, aes(y = Est, x = Par)) + 
  geom_boxplot(position = "dodge",color = "black", notch = TRUE, orientation = "x", aes(fill = Stream), alpha = 0.85) + 
  #scale_color_viridis_d("Stream", aesthetics = "fill") +
  facet_wrap(~Par, scales = "free", dir = "v", 
             labeller = as_labeller(
               c(Par_1 = "Beta 3 Before Flooding Event", Par_2 = "Beta 0 Before Flooding Event", Par_3 = "Beta 1 Before Flooding Event",
                 Par_4 = "Beta 3 After Flooding Event" , Par_5 = "Beta 0 After Flooding Event", Par_6 = "Beta 1 After Flooding Event" )), 
             strip.position = "top") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs( y = "Estimation", x = "Parameter")
box

reso <- 300
length <- 3.25*reso/72
### combine maps with distance and year 

png(file="Fig_S2_260408.png",width=length/1.5, height=length/2, res = reso, units = "in");

box;

dev.off()