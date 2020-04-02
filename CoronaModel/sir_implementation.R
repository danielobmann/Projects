# ------------------------------------------------- 
# Nonlinear SIR Model

# Using explicit Euler for solving SIR differential equations
.sir_step_nonlinear = function(u0, t, r, rho, delta_t=1e-1){
  u0 + delta_t*c(-r(t, u0)*u0[1]*u0[2], r(t, u0)*u0[1]*u0[2] - rho(t, u0)*u0[2], rho(t, u0)*u0[2])
}

# Complete calculation of SIR model results with starting values u0 and nonlinearities r and rho
sir_nonlinear = function(u0, T, r, rho, delta_t=1e-1){
  ret = data.frame("S" = c(u0[1]), "I" = c(u0[2]), "R" = c(u0[3]))
  steps = T/delta_t
  for (step in 1:steps) {
    t = step*delta_t
    u0 = .sir_step_nonlinear(u0, t, r=r, rho = rho, delta_t = delta_t)
    ret = rbind(ret, list("S" = u0[1], "I"=u0[2], "R"=u0[3]))
  }
  ret
}


# Example for possible nonlinearities
r_constant = function(t, u, r = 2){
  return(r)
}

rho_constant = function(t, u, rho=1){
  return(rho)
}

r_linear = function(t, u, t0=1.5, r=2.5){
  max(r*(t0-t)/t0, 0)
}

rho_linear = function(t, u, t0=1.5, r=1.25){
  max((t-t0)*r, 0)
}

r_decaying = function(t, u, t0=1.0, r=1.2){
  pmax((r+t0)/(t+t0), r)
}

rho_increasing = function(t, u, t0=2.5, rho=0.05){
  pmin(rho*pmax(t^2 - t0, 0), 1.4)
}