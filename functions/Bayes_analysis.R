bayes_analysis <- function(scores){
  
  #scale variables
  scores$CUSADOS_scaled <- scale(scores$CUSADOS)
  scores$SIAS_scaled <- scale(scores$SIAS)
  
  # Define the formula - designing the model. We have a between-study design. 
  SAD_f0 <- bf(SIAS_scaled ~ 1 + CUSADOS_scaled)
  
  #if you scale y and x see riccardos video for assignment 3. If not, intercept should be the avarage of y and I don't know about the betas
  # Design the priors
  prior_f0 <- get_prior(SAD_f0, family = gaussian, scores) # The family = Guassian, because the outcome is a continuous variable
  # We see that we need a beta prior and a sigma prior
  summary(scores$SIAS_scaled) 
  priorSAD <- c(
    prior(normal(0, 1), class = Intercept), # the intercept prior needs to reflect the average y-value (where SIAS is 0, threshold is supposed to be average)
    prior(normal(0, 0.3), class = b), # beta is the expectation of the difference between schizophrenia and controls. We say that the beta is normally distributed, and that the mean is 4 and the standard deviation is 1.
    prior(normal(0.5, 1), class = sigma) # sigma is the average error that we expect. 
  ) 
  
  # Test the priors. We want to check whether the priors make any sense.
  SAD_PriorCheck_m <- brm( # m stands for model
    formula = SAD_f0,
    data = scores,
    family = gaussian, # Gaussian because the outcome is continuous
    prior = priorSAD,
    sample_prior = "only",# we sample the prior in order to test the prior
    control = list(adapt_delta = 0.9)
  )
  
  # We check the what the predictions look like given the prior and not the data. We set the number of simulations to 100.
  pp_check(SAD_PriorCheck_m, nsamples = 100)
  
  # What we see is that the prior has a very long tail. In order to fix this we make a prior for the sigma - in order to expect an error (we edited the first one)
  
  ## Fitting the model
  SAD_m <- brm(
    formula = SAD_f0,
    data = scores,
    family = gaussian,
    prior = priorSAD,
    sample_prior = T,
    control = list(adapt_delta = 0.9)
  )
  
  # Posterior predictive check. # We want to look at whether the posterior has learned from the prior, which is what we expect when we look at the posterior predictive check
  pp_check(SAD_m, nsamples = 100)
  # The data looks good!
  # The light blue is the prior for the difference between schizophrenia and control (it is very spread, which means that it is very uncertain). 
  # The dark blue is the posterior (this is much more certain - must less variance), which tells us that it has actually learned from the data, and makes more confident predictions. 
  
  # Conclusion: the prior is not off, and the posterior has learned from the data. This is good!
  return(SAD_m)
}
