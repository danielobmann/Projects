# ------------------------------------------------- 
# Load data and libraries
library(ggplot2)
source('sir_implementation.R')
df = read.csv("data/infected.csv", sep = ";")
df$Datum = NULL

maximum_tirol = 751140 
maximum_aut = 8822000
# ------------------------------------------------- 
# Set up model for the infection and recovery rate

# Plot the rate to get an idea how to model it
rate = function(y) y[2:(length(y))]/y[1:(length(y) - 1)]
n = 3
rate_y = na.omit(filter(rate(df$T), rep(1/n, n), sides = 2))
plot(rate_y, type='l', lwd=2, ylab="Rate with running average")

logistic_deriv = function(t, a=0.1, b=5, c=0.95, d=1.5){
  d*a*exp(-a*t + b)/(1 + exp(-a*t + b))^2 + c
}

tx = seq(1, 100, by=0.1)
plot(tx, logistic_deriv(tx))

infection_rate = function(t, u, param) logistic_deriv(t, a=param[1], b=param[2], c=param[3], d=param[4])
recovery_rate = function(t, u, param) logistic_deriv(t, a=param[5], b=param[6], c=param[7], d=param[8])

# Output of complete model for given rate models
model = function(parameter, u0, delta_t=1e-1) {
  r = function(t, u) infection_rate(t, u, param = parameter)
  rho = function(t, u) recovery_rate(t, u, param = parameter)
  sir_nonlinear(u0, T=parameter[length(parameter)-1], r=r, rho=rho, delta_t=delta_t)
}


# TODO: Make parameters more flexible and adjust logprior
logprior = function(parameter) {
  coeff_prior = sum(dnorm(parameter[1:8], mean=0, sd=100, log = TRUE)) 
  T_prior = dunif(parameter[length(parameter)-1], min=1, max = 1e4, log = TRUE)
  sigma_prior = dunif(parameter[length(parameter)], log = TRUE)
  coeff_prior + T_prior + sigma_prior
}
loglikelihood = function(y, parameter, u0, delta_t=1e-1) {
  if (parameter[length(parameter) - 1] < length(y)) {
    return(-500)
  }
  
  idx = seq(1, length(y)/delta_t, by = 1/delta_t)
  I = model(parameter, u0, delta_t=delta_t)[idx, 2]
  return(sum(dnorm(y, mean=I, sd=parameter[length(parameter)], log = TRUE)))
}
logposterior = function(parameter, y, u0, delta_t=1e-1){
  logprior(parameter) + loglikelihood(y = y, parameter = parameter, u0=u0, delta_t = delta_t)
}
proposal = function(parameter, proposal_sd){
  rnorm(length(parameter), mean=parameter, sd=proposal_sd)
}

run_MH = function(parameter0, prop_sd, y, niter=1e4, delta_t=1e-1){
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

plot_results = function(samples, y, u0, delta_t=1e-1){
  m = apply(accepted_samples, 2, function(x) c(quantile(x, probs = 0.1), mean(x), quantile(x, probs = 0.9)))
  m[, ncol(m)-1] = max(m[, ncol(m) - 1])
  
  fit = apply(m, 1, function(x) as.numeric(model(x, u0 = u0, delta_t = delta_t)[, 2]))
  tx = seq(0, as.integer(m[1, ncol(m)-1]), by=delta_t)
  
  matplot(tx, fit, type = 'l', col = c(1,2,1), lwd = c(1,2,1), lty = c(2,1,2))
  idx = seq(1, length(y)/delta_t, by = as.integer(1/delta_t))
  points(tx[idx], y, col='blue')
}


# --------------------------------------------------------------
# Initial value must be chosen such that it reflects the first time numer of people infected
I0 = df$T[1]/maximum_tirol
u0 = c(1-I0, I0, 0)

# Run simulations
y = df$T/maximum_tirol
delta_t = 0.5

# PARAMETER: C(infection_rate_coefficients, recovery_rate_coefficients, T, sigma)
prop_sd = c(rep(0.01, 8), 1, 0.01)
parameter0 = c(rep(0.1, 8), 50, 0.1)

samples = run_MH(parameter0, prop_sd, y = y, niter = 1e3, delta_t = delta_t)
accepted_samples = samples[samples$Accepted == 1, -ncol(samples)]
summary(accepted_samples)

plot_results(accepted_samples, y, u0, delta_t = delta_t)


params = as.numeric(accepted_samples[2,])
idx = seq(1, length(y)/delta_t, by = 1/delta_t)
I = model(params, u0, delta_t)[idx,2]

plot(y)
points(I, col='red')
