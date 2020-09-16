#Ok this script is to try and get time varying fishing mortality
#We want to try and reproduce the cod collapse of the 1990s so that we can feel more confirdent in our model before we begin future projections

library(mizer)
setwd("c:/users/derekt/work/isabellefishery/")

# First let's import the f_history CSV
f_history <- as(read.csv("c:/users/derekt/work/isabellefishery/f_history.csv", row.names = 1), "matrix")
colnames(f_history) <- c('AMERICAN PLAICE', 'COD(ATLANTIC)','HADDOCK', 'HERRING(ATLANTIC)', 'REDFISH UNSEPARATED', 'SPINY DOGFISH', 'WITCH FLOUNDER', 'TURBOT,GREENLAND HALIBUT', 'YELLOWTAIL FLOUNDER')

species_params = read.csv("species_params_species_with_ram_SSB.csv")
names(species_params)[1] = "species"

# Read in the time-varying SSB against which to compare the model
ram_ssb = as(read.csv("c:/users/derekt/work/isabellefishery/SSB_total.csv", row.names = 1), "matrix")
colnames(ram_ssb) <- c('AMERICAN PLAICE', 'COD(ATLANTIC)','HADDOCK', 'HERRING(ATLANTIC)', 'REDFISH UNSEPARATED', 'SPINY DOGFISH', 'WITCH FLOUNDER', 'TURBOT,GREENLAND HALIBUT', 'YELLOWTAIL FLOUNDER')
ram_ssb <- ram_ssb[,1:9]

selectivity_function <- function(w ,...)
{
  return(1)
}


# Set up gear params data fram
gear_params <- 
  data.frame(species = species_params$species,
             gear = species_params$species,
             sel_func = "selectivity_function")

gear_params$catchability <- rep(1,dim(gear_params)[2])

# Set up parameters data frame
params <- newMultispeciesParams(species_params, 
                                kappa = 1e11,
                                gear_params = gear_params)

row1995 = which(rownames(f_history) == "1995")

# Create a temporal effort matrix
relative_effort <- f_history #sweep(f_history, 2, f_history["1995",],"/")
initial_effort <- matrix(relative_effort[1,], byrow=TRUE, nrow = 100, ncol = ncol(relative_effort), dimnames = list(1870:1969))
relative_effort <- rbind(initial_effort, relative_effort)

# Run the simulation
sim<- project(params, effort = relative_effort, dt = 0.25)
params@catchability
params@gear_params$gear

biomasses_through_time = getBiomass(sim)

new_Rmax = rep(8.26e+09, length(params@species_params$R_max))
params@species_params$R_max = new_Rmax

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
        # Loop through years
    for (biolyear in 1:dim(model_ssb)[1])
    {
      # Check to see if the empirical SSB is NA
      if (is.na(ram_ssb[biolyear,species]))
      {
        
      } else
      {
        total_sse = total_sse + (ram_ssb[biolyear, species] - model_ssb[biolyear, species])^2
      }
    }
  }
  return(total_sse)
}

# Run model just inputting rMax
runModel <- function(rMax)
{
  # Put new vector back into species params
  params@species_params$R_max = rMax
  
  # Run the model
  sim <- project(params, effort = relative_effort)
  
  # Extract final biomasses
  biomasses_through_time = getBiomass(sim)
  
  
  # Calculate SSE
  sse_final <- calculate_sse_time_series(ram_ssb, biomasses_through_time)
  return(sse_final)
}

# Optimize rMax parameter estimates
aa = optim(new_Rmax, runModel)


# Run model inputting rMax and kappa
runModelMultiOptim <- function(initialParameterValues)
{
  # Put new vector back into species params
  params@species_params$R_max = initialParameterValues[1:9]
  params <- setParams(params, kappa = initialParameterValues[10])
  
  # Run the model
  sim <- project(params, effort = relative_effort)
  
  # Extract final biomasses
  biomasses_through_time = getBiomass(sim)
  
  
  # Calculate SSE
  sse_final <- calculate_sse_time_series(ram_ssb, biomasses_through_time)
  return(sse_final)
}

# initial value for kappa
kappa_temp = 1.0e11

# Optimize rMax and kappa parameter estimates
bb = optim(c(new_Rmax, kappa_temp), runModelMultiOptim)
