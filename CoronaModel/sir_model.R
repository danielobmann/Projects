# ------------------------------------------------- 
# Real data 
y_tirol = c(2,2,2,2,2,2,2,2,3,4,7,8,16,32,57,109,167,206,254,254,328,382,464,508,575,644,803,1253,1393,1623,1752,1874,1975,2222)
y_aut = c(2,2,4,5,10,10,18,29,41,55,79,99,131,182,246,361,504,655,860,1016,1332,1646,2013,2388,2814,3244,3924,4876,5560,6398,7399,8122,8672,9541)

maximum_tirol = 751140 
maximum_aut = 8822000
# ------------------------------------------------- 
# SIR Model
library(ggplot2)
source('sir_implementation.R')

infection_rate = function(t, u, param) param[1] + param[2]*t + param[3]*t^2 + param[4]*t^3
recovery_rate = function(t, u, param) param[5] + param[6]*t + param[7]*t^2 + param[8]*t^3

model = function(parameter, u0, delta_t=1e-1) {
  r = function(t, u) infection_rate(t, u, param = parameter)
  rho = function(t, u) recovery_rate(t, u, param = parameter)
  T = parameter[length(parameter)-1]
  ret = sir_nonlinear(u0, T=T, r=r, rho=rho, delta_t=delta_t)
  ret
}

# Take data to guess the infection rate

I0 = y_tirol[1]/maximum_tirol # Initial value must be chosen such that it reflects the first time numer of people infected
u0 = c(1-I0, I0, 0)

logprior = function(parameter) {
  coeff_prior = sum(dnorm(parameter[-length(parameter)], mean=0, sd=100, log = TRUE)) 
  T_prior = dunif(parameter[length(parameter)-1], min=1, max = 1e4, log = TRUE)
  sigma_prior = dunif(parameter[length(parameter)], log = TRUE)
  coeff_prior + T_prior + sigma_prior
} 
loglikelihood = function(y, parameter, delta_t=1e-1) {
  T = parameter[length(parameter) - 1]
  tx = seq(0, T, by=delta_t)
  idx = seq(1, length(tx), by = as.integer(1/delta_t))[1:length(y)]
  I = (model(parameter, u0, delta_t=delta_t)$I)[idx]
  ret = sum(dnorm(y, mean=I, sd=parameter[length(parameter)], log = TRUE))
  if (T < length(y)) {
    ret = -500
  }
  return(ret)
}
logposterior = function(parameter, y, delta_t=1e-1){
  logprior(parameter) + loglikelihood(y = y, parameter = parameter, delta_t = delta_t)
}
proposal = function(parameter, proposal_sd){
  rnorm(length(parameter), mean=parameter, sd=proposal_sd)
}

run_MH = function(parameter0, prop_sd, y, niter=1e4, delta_t=1/24){
  parameter.save = matrix(NA, nrow=niter, ncol=length(parameter0)+1)
  
  for (i in 1:niter) {
    parameter.star = proposal(parameter = parameter0, proposal_sd = prop_sd)
    alpha = logposterior(parameter.star, y, delta_t) - logposterior(parameter0, y, delta_t)
    if (!is.na(alpha) && runif(1) <= exp(alpha)) {
      parameter.save[i,] = c(parameter.star, 1)
      parameter0 = parameter.star
    } else {
      parameter.save[i,] = c(parameter.star, 0)
    }
    
    if (i %% 100 == 0) {
      print(paste0("Finished ", i, " iterations!", sep=""))
      print(paste0("Percentage of accepted samples ", mean(parameter.save[, ncol(parameter.save)], na.rm = TRUE)))
    }
  }
  
  parameter.save = data.frame(parameter.save)
  colnames(parameter.save) = c(paste("beta", 0:3, sep = ""), paste("gamma", 0:3, sep = ""), "T", "Sigma", "Accepted") 
  parameter.save
}


# --------------------------------------------------------------
# Run simulations
y = y_tirol/maximum_tirol
delta_t = 1e-1

# PARAMETER: C(infection_rate_coefficients, recovery_rate_coefficients, T, sigma)
prop_sd = c(rep(0.1, 8), 1, 0.01)
parameter0 = c(rep(0.1, 8), 50, 0.1)

samples = run_MH(parameter0, prop_sd, y = y, niter = 1e4, delta_t = delta_t)
accepted_samples = samples[samples$Accepted == 1, -ncol(samples)]

# TODO: Write function to plot the results

plot_results = function(samples, y, u0, delta_t=1e-1){
  m = as.numeric(apply(samples, 2, mean))
  T = m[length(m)-1]
  tx = seq(0, T, by=delta_t)
  out = model(m, u0, delta_t = delta_t)
  matplot(tx, out$I, type = 'l', col = 'blue', lwd = 2)
  idx = seq(1, length(y)/delta_t, by = as.integer(1/delta_t))
  points(tx[idx], y, col='red')
}

plot_results(accepted_samples, y, u0)



