# Optimization code over parameters

# Pseudo code for calculating SSE for time-series
calculate_sse_time_series <- function(ram_ssb, model_ssb)
{
  
  # Temporarily remove spin up years
  start_year = which(rownames(model_ssb) == "1970")
  model_ssb = model_ssb[start_year:dim(model_ssb)[1],]
  
  total_sse = 0
  
  # Loop through species
  for (species in 1:dim(model_ssb)[2])
  {
    not_na_years = !is.na(ram_ssb[, species])
    
    # Calculate sum of squares and add to the running total
    total_sse = total_sse + sum((ram_ssb[not_na_years, species] - model_ssb[not_na_years, species])^2)
    
  }
  return(total_sse)
}


# Run model just inputting rMax
runModel <- function(rMax, params, effort, t_max)
{
  # Put new vector back into species params
  params@species_params$R_max = rMax
  
  # Run the model
  sim <- project(params, t_max = t_max, effort = effort)
  
  # Extract final biomasses
  biomasses_through_time = getBiomass(sim)
  
  # Calculate SSE
  sse_final <- calculate_sse_time_series(obs_SSB, biomasses_through_time)
  return(sse_final)
}



# Run model inputting rMax and kappa
runModelMultiOptim <- function(initialParameterValues)
{
  # Put new vector back into species params
  params@species_params$R_max = initialParameterValues[1:9]
  params@species_params$erepro = initialParameterValues[10:18]
  params <- setParams(params, kappa = initialParameterValues[19])
  
  # Run the model
  sim <- project(params, effort = relative_effort)
  
  # Extract final biomasses
  biomasses_through_time = getBiomass(sim)
  
  
  # Calculate SSE
  sse_final <- calculate_sse_time_series(obs_SSB, biomasses_through_time)
  return(sse_final)
}

bb = optim(c(new_Rmax, rep(1,9), 1e+11), runModelMultiOptim, method = "L-BFGS-B", lower = c(rep(0,9), rep(0.5,9), 1e+8), upper = c(rep(1000000000000,9), rep(1,9), 1e+14))
