rm(list=ls())

alpha = c(0.5, 1, 1.95) #rep(0.5,3) #c(0.1,1,2)
theta_list = alpha #c(-0.5, 1, 2)

sublin_Nt = function(N0, K, theta, alpha, tm) {
  x = (N0/K)^theta
  xt = 1/(1+(1-x)/x*exp(-alpha*tm))
  K*(xt^(1/theta))
}

tm = seq(0, 5, by = 0.01)
tm2 = seq(0, 10, by= 0.01)

Nt_pre = Nt_post = NULL
Klst = c(1, 1, 1)
for(i in 1:3) {
  # pre-disturbance
  Nt_pre = cbind(Nt_pre, sublin_Nt(N0 = Klst[i], K = Klst[i], theta = theta_list[i], alpha = alpha[i], tm = tm))
  
  # disturbance
  Nt_post = cbind(Nt_post, sublin_Nt(N0 = 0.5, K = Klst[i], theta = theta_list[i], alpha = alpha[i], tm = tm2))
}


collst = viridis::viridis(3)

reso <- 300
length <- 3.25*reso/72

png(file="ConcFig1C_260828.png",width=length/2.8, height=length/3, res = reso, units = "in");
matplot(tm, Nt_pre, type = "l", lty = 1, lwd =2, col = collst,
        xlab = "Time", ylab = "Abundance [N]",
        xlim = c(0,max(tm2)+max(tm)), ylim = c(0.5,1));

matlines(tm2+max(tm), Nt_post, lty = 1, lwd =2, col = collst,
        xlim = c(0,20), ylim = c(0,1));
legend("bottomright", legend = "Disturbance", bty = "n", lty = 2, lwd = 2, col = "black");

abline(v=max(tm), lty = 2, lwd = 2);

dev.off()



# get derivatives
dNt_pre = diff(Nt_pre)/diff(tm)
dNt_post = diff(Nt_post)/diff(tm)


png(file="ConcFig1d_260828.png",width=length/2.8, height=length/3, res = reso, units = "in");
matplot(tm[-1], dNt_pre, type = "l", lty = 1,lwd = 2, col = collst,
        xlab = "Time", ylab = "Growth Rate [dN/dt]",
        xlim = c(0,max(tm2)+max(tm)), ylim = c(0, 0.25));

matlines(tm2[-1]+max(tm), dNt_post, lty = 1,lwd = 2, col = collst,
         xlim = c(0,20), ylim = c(0,1));

abline(v=max(tm), lty = 2, lwd = 2);
legend("topright", legend = c("Sublinear", "Logistic","Superlinear"), title = "Regulation Pattern", bty = "n", lty = 1, lwd = 2, col = collst);

dev.off()

# get dNNdt
sublin_dNNdt = function(N, K, theta, alpha) {
  (alpha/theta*(1-(N/K)^theta))
}

Nlst = seq(0,1, length=1e3)
dNNdt = NULL

for(i in 1:3) {
  dNNdt = cbind(dNNdt, sublin_dNNdt(N=Nlst, K = 1, theta = theta_list[i], alpha = alpha[i]))
}


png(file="ConcFig1b_260828.png",width=length/2.4, height=length/3.2, res = reso, units = "in");
matplot(Nlst, dNNdt, type = "l", lty = 1,lwd = 2, col = collst,
        ylim = c(0, 1),
        xlab = "Abundance [N]", ylab = "Per Capita Growth [dN/Ndt]");


#legend("topright", legend = c("Sublinear", "Logistic","Superlinear"), title = "Regulation Pattern", bty = "n", lty = 1,lwd = 2, col = collst);
dev.off()

