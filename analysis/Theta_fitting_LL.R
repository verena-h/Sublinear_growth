#### fit with S-mapping
require(pttstability)
require(ggplot2)
require(gridExtra)
require(grid)

load("filterout_LL.rda")

tm = c(1:144)
Nt = filterout_est$Nest
Nt_noproc = filterout_est$Nest_noproc
N0 = rep(NA, length(tm))
N0[1] = NA
N0[2:144] = Nt[1:(length(Nt)-1)]
dt = 1
## fit initial model to get tuning parameter theta_Smap (not related to regular theta)
Y = Nt_noproc
X = cbind(1, N0)#, c(NA, N0[-length(N0)]))
E = 1

# run initial S-map to get theta_Smap
theta_Smap =0.3 #optout$minimum

Sout = S_map_Sugihara1994(Y, E, theta_Smap, X)
Sout$R2


## use S-mapping to make synthetic time-series with small deviations around each observation

m = 12 # samples per regression
dN = diff(range(N0, na.rm = TRUE))*0.01 # deviation size
dN_seq = seq((-dN*m/2), (dN*m/2), length=m)

N0d_mat = Ntd_mat = matrix(nrow = length(N0), ncol = m)

# for each deviation, get estimate of Nt from N0
for(i in 1:m) {
  N0d = N0 + dN_seq[i]
  tmp = X
  tmp[,2] = N0d
  X_new = rbind(X, tmp)
  Y_new = c(Y, rep(NA, length(Y)))
  Sout_new = S_map_Sugihara1994(Y_new, E, theta_Smap, X_new)
  
  N0d_mat[,i] = N0d
  Ntd_mat[,i] = Sout_new$Y_hat[-c(1:length(Y))]
}

## use estimates of Nt and N0 to estimate theta
coefmat = matrix(nrow = length(N0), ncol = 4, data = NA)
dNNdt_N_list = rep(NA, length(N0))
Nmean_list = rep(NA, length(N0))
for(i in 1:length(N0)) {
  Nmean = (Ntd_mat[i,]+N0d_mat[i,])/2
  dNNdt_N = (Ntd_mat[i,]-N0d_mat[i,])/dt/Nmean # per-capita growth rate
  
  dNNdt_N_list[i] = dNNdt_N[6]
  Nmean_list[i] = Nmean[6]
  #plot(Nmean, dNNdt_N)
  
  dNNdt_N_dN = diff(dNNdt_N)/diff(Nmean) # d(per-capita growth rate)/dN
  Nmean2 = (Nmean[-1]+Nmean[-length(Nmean)])/2
  
  mod = "error"
  mod = try(nls(dNNdt_N_dN ~ -exp(lalpha)*(Nmean2^theta)/Nmean2, # fit analytical expectation
                start = c(lalpha = log(0.3), theta = 0.5)))
  if(!is.character(mod)) {
    tmp = summary(mod)
    
    coefmat[i,] = c(tmp$coefficients[,1:2])
  }
}

var_a = var(exp(coefmat[,1]), na.rm = TRUE)
var_th = var(coefmat[,2],na.rm = TRUE)
df = rbind(var_a,var_th)
colnames(df) = "LL" 

data_figS4 = data.frame(cbind(Nmean_list,dNNdt_N_list)) #to Plot dNNdt vs. N

save(list=c("df","coefmat"),file="Theta_est_LL.rda")
#load("Theta_est_LL.rda")
# example of theta_Smap effect
theta_Smap_List = c(0, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 2, 5, 10)
fitout = data.frame(R2 = rep(NA, length(theta_Smap_List)), RMSE = NA)
for(i in 1:length(theta_Smap_List)) {
  Sout_tmp = S_map_Sugihara1994(Y, E, theta_Smap_List[i], X)
  fitout$R2[i] = Sout_tmp$R2
  fitout$RMSE[i] = Sout_tmp$RMSE
}
fitout$theta = theta_Smap_List
plot(R2~theta, fitout, type = "l")
abline(v = 0.3, lty = 2) #chosen theta
abline(v = 0.57, lty = 2, col = "red") #opt theta
plot(RMSE~theta, fitout, type = "l")

## RS as ggplot
fitout = as.data.frame(fitout)

r2_plot_LL = ggplot(fitout, aes(y=R2, x = theta)) +
  ylim(0,1) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 0.3, linetype = 2) +
  geom_vline(xintercept = 0.57, linetype = 2, col = "red") +
  labs(x="Nonlinearity Parameter", y = "R2", title = "LL") +
  theme_linedraw() +
  theme(plot.title = element_text(hjust = 0.5, size = 18))

save(list=c("r2_plot_LL"),file="LL_r2_plots.rda")

#Plot dNNdt vs. N
dNNdt_N_LL = ggplot(data_figS4, aes(y=dNNdt_N_list, x=Nmean_list)) + 
  geom_point() +
  labs(x="Standardized Abundance [N]",y="dN/Ndt", title = "LLi") +
  geom_vline(xintercept =1, linetype = 2) +
  geom_hline(yintercept =0, linetype = 2) +
  theme_linedraw()
dNNdt_N_LL

save(list=c("dNNdt_N_LL"),file="dNNdt_N_LL.rda")