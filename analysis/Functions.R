## update Particle Filter Functions
parseparam0 = function (param, colparam = c(logit(1e-4), log(0.1)), detparam = c(log(1.2), 
                                                                                 log(1))) {
  if (length(param) == 2) {
    pars <- list(obs = c(param[1]), proc = c(param[2]), pcol = colparam[c(1:2)], 
                 det = detparam[c(1:2)])
  }  else if (length(param) == 4) {
    pars <- list(obs = c(param[c(1,3)]), proc = c(param[c(2,4)]), 
                 pcol = colparam[c(1:2)], det = detparam[c(1:2)])
  } else {
    pars <- list(obs = c(param[c(1,4)]), proc = c(param[c(2:3,5:6)]), 
                 pcol = colparam[c(1:2)], det = detparam[c(1:2)])
  }
  return(pars)
}

obsfun0 = function (so, yt, xt = NULL, inverse = FALSE, N = NULL, minsd = 0.01, 
                    time = NULL) {
  if (inverse) {
    if (length(so) == 1) {
      std_tmp <- exp(so[1]) * yt
    }
    else {
      if(time < min(t2)) {
        std_tmp <- exp(so[1]) * yt
      } else {
        std_tmp <- exp(so[2]) * yt
      }
    }
    std_tmp[std_tmp < minsd] <- minsd
    pmax(0, rnorm(n = N, mean = yt, sd = std_tmp))
  }
  else {
    if (length(so) == 1) {
      std_tmp <- exp(so[1]) * xt
    }
    else {
      if(time < min(t2)) {
        std_tmp <- exp(so[1]) * yt
      } else {
        std_tmp <- exp(so[2]) * yt
      }
    }
    std_tmp[std_tmp < minsd] <- minsd
    if (yt == 0) {
      tiny <- 1e-05
      lo = -Inf
      hi = 0
      q1 <- pnorm(lo, xt, std_tmp)
      q2 <- pnorm(hi, xt, std_tmp)
      z <- runif(n = length(xt), min = q1, max = q2)
      z <- qnorm(z, xt, std_tmp)
      z[is.infinite(z) & sign(z) > 0] <- lo + tiny
      z[is.infinite(z) & sign(z) < 0] <- hi - tiny
      LL = dnorm(z, xt, std_tmp, log = TRUE)
    }
    else {
      LL = dnorm(yt, xt, std_tmp, log = TRUE)
    }
    return(LL)
  }
}


procfun0 = function (sp, xt, inverse = FALSE, time = NULL){
  if (length(sp) == 1) {
    std_tmp <- exp(sp[1])
  } else if(length(sp)==2) {
    if(time < min(t2)) {
      std_tmp <- exp(sp[1])
    } else {
      std_tmp <- exp(sp[2])
    }
  } else {
    if(time < min(t2)) {
      std_tmp <- sqrt(exp(sp[1]) * xt^exp(sp[2]))
    } else {
      std_tmp <- sqrt(exp(sp[3]) * xt^exp(sp[4]))
    }
  }
  if (!inverse) {
    sm <- length(xt)
    xt = pmax(0, xt + rnorm(sm, 0, std_tmp)) * (xt > 0)
    return(xt)
  }
  else {
    pd <- pnorm(0, xt, std_tmp)
    return(pd)
  }
}
