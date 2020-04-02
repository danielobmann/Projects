# -----------------------------------------
# Metropolis Hasting sampling for logistic regression

logistic = function(t, param){
  t0 = param[1]
  k = param[2]
  L = param[3]
  L/(1 + exp(-k*(t - t0)))
}

sample_logistic = function(t, param){
  t0 = param[1]
  k = param[2]
  L = param[3]
  sigma = param[4]
  return(L/(1 + exp(-k*(t-t0))) + rnorm(1, sd=sigma))
}

likelihood = function(param, data){
  sigma = param[4]
  
  T = 1:length(data)
  
  pred = logistic(T, param)
  sum(dnorm(data, mean = pred, sd=sigma, log = TRUE))
}

prior = function(param, maximum = 1e6){
  t0 = param[1]
  k = param[2]
  L = param[3]
  sigma = param[4]
  
  p_t0 = as.integer(t0 > 0)
  p_k = 1/k
  p_L = dunif(L, 0, max = maximum)
  p_sd = 1/sigma**2
  
  log(p_t0) + log(p_k) + log(p_L) + log(p_sd)
}

posterior = function(param, data, maximum_L=1e6){
  return(likelihood(param, data=data) + prior(param, maximum = maximum_L))
}

proposal = function(param, sd = c(2.0, 0.01, 1e3, 0.5)){
  rnorm(4, mean = param, sd = sd)
}

run_MCMC = function(param0, iterations, data, proposal_sd = c(2.0, 0.01, 1e3, 0.5), maximum_L=1e6){
  chain = array(dim = c(iterations+1, length(param0) + 2))
  chain[1, ] = c(param0, 0, 0)
  
  for (i in 1:iterations) {
    prop = proposal(chain[i, ], sd = proposal_sd)
    alpha = min(1,exp(posterior(prop, data=data, maximum_L=maximum_L) - posterior(chain[i, ], data=data, maximum_L=maximum_L)))
    
    if (runif(1) < alpha) {
      chain[i+1, ] = c(prop, 1, alpha)
    } else {
      chain[i+1, ] = c(chain[i, 1:length(param0)], 0, alpha)
    }
  }
  colnames(chain) = c("t0", "k", "L", "sigma", "Accepted", "Alpha")
  return(data.frame(chain))
}

plot_result = function(params, data, day=100){
  EVAL = data.frame(t(apply(params, 1, sample_logistic, t=1:day)))
  
  days = seq(as.Date("2020-02-26"), as.Date("2020-02-26") + (day - 1), by = "day")
  ymax = mean(EVAL[, day])*1.2
  
  plot(days, apply(EVAL, 2, mean), type = 'l', lwd=2, 
       col = rgb(red = 0, green = 0, blue = 1, alpha = 1.0), 
       ylim = c(0, ymax), 
       ylab = "Number of infected people", 
       xlab = " ",
       main = "Corona model Tyrol")
  
  points(days, apply(EVAL, 2, function(x) {quantile(x, probs = 0.75)}), type = 'l', lwd=2, col = rgb(red = 0, green = 1, blue = 1, alpha = 0.5))
  points(days, apply(EVAL, 2, function(x) {quantile(x, probs = 0.25)}), type = 'l', lwd=2, col = rgb(red = 0, green = 1, blue = 1, alpha = 0.5))

  points(days, apply(EVAL, 2, function(x) {quantile(x, probs = 0.9)}), type = 'l', lwd=2, col = rgb(red = 0, green = 0.5, blue = 1, alpha = 0.25))
  points(days, apply(EVAL, 2, function(x) {quantile(x, probs = 0.1)}), type = 'l', lwd=2, col = rgb(red = 0, green = 0.5, blue = 1, alpha = 0.25))

  points(days[1:length(data)], data, col = ifelse(weekdays(days[1:length(data)]) == "Sonntag", "green", "red"), lwd = 2, pch="+")
  
  legend("topleft", 
         legend=c("Real data", "Mean", "25/75% Quantile", "10/90% Quantile"), 
         col=c("red", "blue", rgb(red = 0, green = 1, blue = 1, alpha = 0.5), rgb(red = 0, green = 0.5, blue = 1, alpha = 0.25)),
         lty = c(0, 1, 1, 1), lwd = c(2, 2, 2, 2), pch = c("+"," ", " ", " "),
         cex = 0.75)
}

evaluate_model = function(EVAL, func, day = 40){
  ret = data.frame(apply(EVAL, 2, func), row.names = c("10%", "Mean", "Median", "90%"))
  colnames(ret) = seq(as.Date("2020-02-26"), as.Date("2020-02-26") + (day - 1), by = "day")
  ret
}

values_function = function(x){
  c(quantile(x, probs = 0.1), mean(x), median(x), quantile(x, probs = 0.9))
}


# ------------------------------------------------- 
# Real data & logistic model
y_tirol = c(2,2,2,2,2,2,2,2,3,4,7,8,16,32,57,109,167,206,254,254,328,382,464,508,575,644,803,1253,1393,1623,1752,1874,1975,2222)
y_aut = c(2,2,4,5,10,10,18,29,41,55,79,99,131,182,246,361,504,655,860,1016,1332,1646,2013,2388,2814,3244,3924,4876,5560,6398,7399,8122,8672,9541)
df = data.frame("x" = 1:length(y_tirol), "y_tirol" = y_tirol, "y_aut"=y_aut)

y = df$y_tirol
#y = y[1:(length(y)-10)]
maximum_real = 751140 # 8822000

param0 = c(60, 0.5, 1e4, 13.0)
prop_sd = c(0.1, 0.01, 100, 0.1)
niter = 1e6

samples = run_MCMC(param0=param0, iterations=niter, data=y, proposal_sd=prop_sd, maximum_L = maximum_real)
summary(samples)

# Results of the simulations
accepted_samples = samples[samples$Accepted==1, ]
burn_in = as.integer(0.5*nrow(accepted_samples))
accepted_samples = accepted_samples[burn_in:nrow(accepted_samples),]

summary(accepted_samples)

par(mfrow=c(2,2))
hist(accepted_samples$L, freq=FALSE)
hist(accepted_samples$t0, freq=FALSE)
hist(accepted_samples$k, freq=FALSE)
hist(accepted_samples$sigma, freq=FALSE)
par(mfrow=c(1,1))

plot(accepted_samples[, c("k", "t0", "L")])

day = 60


fit = apply(accepted_samples[1:4], 2, function(x){
  q = quantile(x, probs = c(0.025, 0.1, 0.25, 0.75, 0.9, 0.975))
  return(c("2.5%" = q[1], "10%" = q[2], "25%" = q[3], "Mean" = mean(x), "Median" = median(x), "75%" = q[4], "90%" = q[5], "97.5%"=q[6]))
})

fit
T = seq(0, 100)
matplot(T, logistic(T, fit))



plot_result(accepted_samples, data = y, day = day)

EVAL = data.frame(t(apply(accepted_samples, 1, sample_logistic, t=1:day)))
pred = evaluate_model(EVAL, values_function, day = day)
pred

# Calculating effective samplesize
nrow(accepted_samples)/sum(acf(accepted_samples$t0, plot=F, lag.max = 100)$acf)

