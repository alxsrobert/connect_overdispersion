## Define a stochastic SEIR compartmental model. The model allows for one level
## of stratification (which will be used as a combination of age group, region,
## and transmitter group).
## Inputs:
## - N (vector): Population size per strata
## - E0 (vector): Number of exposed at t=0 per strata
## - beta0 (numeric): Transmissibility rate 
## - gamma (numeric): 1 / Infectious period
## - delta (numeric): 1 / Latent period
## - per_cap_matrix (Matrix): Per capita contact matrix.
## - n_tot (integer): Number of strata.

seir_stoch_strat <- odin2::odin({
  ## Update compartments (per strata)
  update(S[]) <- S[i] - n_SE[i]
  update(E[]) <- E[i] - n_EI[i] + n_SE[i]
  update(I[]) <- I[i] - n_IR[i] + n_EI[i]
  update(R[]) <- R[i] + n_IR[i]
  update(new_cases[]) <- new_cases[i] + n_EI[i]
  
  ## Compute the force of infection for each strata
  ## The force of infection is equal to the average number of contacts multiplied 
  ## by the number of per capita contact across all infected individuals
  lambda[] <- beta0 * sum(s_ij[i,])
  s_ij[,] <- per_cap_matrix[i,j] * I[j]
  
  ## Probability of movement between compartments for 1 individual
  p_SE[] <- 1 - exp(-lambda[i] * dt)
  p_EI[] <- 1 - exp(-delta * dt)
  p_IR[] <- 1 - exp(-gamma * dt)
  
  ## Draw the number of movements
  n_SE[] <- Binomial(S[i], p_SE[i])
  n_EI[] <- Binomial(E[i], p_EI[i])
  n_IR[] <- Binomial(I[i], p_IR[i])
  
  ## Initial values of each compartment
  initial(S[]) <- N[i] - E0[i]
  initial(E[]) <- E0[i]
  initial(I[]) <- 0
  initial(R[]) <- 0
  initial(new_cases[], zero_every = 1) <- 0
  
  ## List parameters
  N <- parameter()
  E0 <- parameter()
  beta0 <- parameter()
  gamma <- parameter()
  delta <- parameter()
  per_cap_matrix <- parameter()
  n_tot <- parameter()
  
  ## Dimension of objects
  dim(per_cap_matrix, s_ij) <- c(n_tot, n_tot)
  dim(S, E, I, R, lambda, N, E0, p_SE, p_EI, p_IR, n_SE, 
      n_EI, n_IR, new_cases) <- c(n_tot)
})
