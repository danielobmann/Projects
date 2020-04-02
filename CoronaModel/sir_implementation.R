# ------------------------------------------------- 
# Nonlinear SIR Model

# Using explicit Euler for solving SIR differential equations
.sir_step_nonlinear = function(u0, t, r, rho, delta_t=1e-1){
  reval = r(t, u0)
  rhoeval = rho(t, u0)
  u0 + delta_t*c(-reval*u0[1]*u0[2], reval*u0[1]*u0[2] - rhoeval*u0[2], rhoeval*u0[2])
}

# Complete calculation of SIR model results with starting values u0 and nonlinearities r and rho
sir_nonlinear = function(u0, T, r, rho, delta_t=1e-1){
  steps = as.integer(T)/delta_t
  ret = matrix(NA, nrow = steps+1, ncol=3)
  ret[1,] = u0
  for (step in 1:steps) {
    ret[step+1, ] = .sir_step_nonlinear(ret[step,], t = step*delta_t, r=r, rho=rho, delta_t=delta_t)
  }
  ret
}


# Example for possible nonlinearities
.r_constant = function(t, u, r = 2){
  return(r)
}

.rho_constant = function(t, u, rho=1){
  return(rho)
}

.r_linear = function(t, u, t0=1.5, r=2.5){
  max(r*(t0-t)/t0, 0)
}

.rho_linear = function(t, u, t0=1.5, r=1.25){
  max((t-t0)*r, 0)
}

.r_decaying = function(t, u, t0=1.0, r=1.2){
  pmax((r+t0)/(t+t0), r)
}

.rho_increasing = function(t, u, t0=2.5, rho=0.05){
  pmin(rho*pmax(t^2 - t0, 0), 1.4)
}