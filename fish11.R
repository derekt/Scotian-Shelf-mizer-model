#Ok this script is to try and get time varying fishing mortality
#We want to try and reproduce the cod collapse of the 1990s so that we can feel more confirdent in our model before we begin future projections

library(optimParallel)
library(mizer)
setwd("c:/users/derekt/work/isabellefishery/")

# First let's import the f_history CSV. This contains fishing mortality history for each species
f_history <- as(read.csv("c:/users/derekt/work/isabellefishery/f_history_no_blanks.csv", row.names = 1), "matrix")
colnames(f_history) <- c('AMERICAN PLAICE', 'COD(ATLANTIC)','HADDOCK', 'HERRING(ATLANTIC)', 'REDFISH UNSEPARATED', 'SPINY DOGFISH', 'WITCH FLOUNDER', 'TURBOT,GREENLAND HALIBUT', 'YELLOWTAIL FLOUNDER')

# Now read in the species life history parameters matrix
species_params = read.csv("species_params_species_with_ram_SSB.csv")
names(species_params)[1] = "species"

# Finally, read in the time-varying SSB against which to compare the model
ram_ssb = as(read.csv("c:/users/derekt/work/isabellefishery/SSB_total_grams.csv", row.names = 1), "matrix")
colnames(ram_ssb) <- c('AMERICAN PLAICE', 'COD(ATLANTIC)','HADDOCK', 'HERRING(ATLANTIC)', 'REDFISH UNSEPARATED', 'SPINY DOGFISH', 'WITCH FLOUNDER', 'TURBOT,GREENLAND HALIBUT', 'YELLOWTAIL FLOUNDER')
ram_ssb <- ram_ssb[,1:9]

# TEMPORARY
ram_ssb <- ram_ssb / 1000

# Note that there are two versions of many of the above, in grams and in other units. Make sure that the units are standardised.

# Just a test selectivity function where fishing is applied equally to all size classes
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


# Set up the fishing effort (= mortality) matrix, including a spin-up period
row1995 = which(rownames(f_history) == "1995")

# Create a temporal effort matrix
relative_effort <- f_history
initial_effort <- matrix(relative_effort[1,], byrow=TRUE, nrow = 100, ncol = ncol(relative_effort), dimnames = list(1870:1969))
relative_effort <- rbind(initial_effort, relative_effort)

# Run the simulation over time with no selectivity
sim<- project(params, effort = relative_effort, dt = 0.25)
params@catchability
params@gear_params$gear

# Plot the results
biomasses_through_time = getBiomass(sim)
plot(sim, include_critical = TRUE)

# Load the selectivity matrix for knife-edge selectivity
fishing_type = read.csv("Mat_knife_edge_gear_params2.csv")
gear_params <- fishing_type
names(gear_params)[1] = "species"

# This essentially runs the model without any species fished
#gear_params[,4] = rep(1000000,9)

#### TEMPORARY
species_params[,5] = rep(0.8,9)

# Set up parameters data frame. Very sensitive to the value of kappa
params <- newMultispeciesParams(species_params, 
                                kappa = 1e12,
                                gear_params = gear_params)


# Run the simulation
sim<- project(params, effort = relative_effort, dt = 0.25)
params@catchability
params@gear_params$gear

# Plot the simulation
biomasses_through_time = getBiomass(sim)
plot(sim, include_critical = TRUE)


# Optimize over the parameters
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
    not_na_years = !is.na(ram_ssb[, species])
    
    # Calculate sum of squares and add to the running total
    total_sse = total_sse + sum((ram_ssb[not_na_years, species] - model_ssb[not_na_years, species])^2)

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

ptm <- proc.time()
aa = optim(new_Rmax, runModel)
proc.time() - ptm

# Pseudo code for calculating SSE for time-series
calculate_sse_time_series2 <- function(ram_ssb, model_ssb)
{
  
  # Temporarily remove spin up years
  start_year = which(rownames(model_ssb) == "1970")
  model_ssb = model_ssb[start_year:dim(model_ssb)[1],]
  
  return(sum(((ram_ssb - model_ssb)^2), na.rm=T))
}

# Run model just inputting rMax
runModel2 <- function(rMax)
{
  # Put new vector back into species params
  params@species_params$R_max = rMax
  
  # Run the model
  sim <- project(params, effort = relative_effort)
  
  # Extract final biomasses
  biomasses_through_time = getBiomass(sim)
  
  # Calculate SSE
  sse_final <- calculate_sse_time_series2(ram_ssb, biomasses_through_time)
  return(sse_final)
}

ptm <- proc.time()
aa = optim(new_Rmax, runModel2)
proc.time() - ptm

cl <- makeCluster(detectCores()-1)
setDefaultCluster(cl = cl)
clusterExport(cl, c("runModel", "calculate_sse_time_series", "params", "relative_effort", "ram_ssb"))

ptm <- proc.time()
aa = optimParallel(par = new_Rmax, fn = runModel, method = "L-BFGS-B", lower = rep(0,9), upper = rep(1e+20, 9))
proc.time() - ptm
stopCluster(cl)

# Run model with optimized Rmax parameters, extract and plot biomasses
params@species_params$R_max = aa$par
sim <- project(params, effort = relative_effort) 


# Plot model results
dev.new()
plot(sim, include_critical = TRUE)
x =params@species_params
biomasses_through_time = getBiomass(sim)




# Run model inputting rMax and kappa
# runModelMultiOptim <- function(initialParameterValues)
# {
#   # Put new vector back into species params
#   params@species_params$R_max = initialParameterValues[1:9]
#   params <- setParams(params, kappa = initialParameterValues[10])
#   
#   # Run the model
#   sim <- project(params, effort = relative_effort)
#   
#   # Extract final biomasses
#   biomasses_through_time = getBiomass(sim)
#   
#   
#   # Calculate SSE
#   sse_final <- calculate_sse_time_series(ram_ssb, biomasses_through_time)
#   return(sse_final)
# }

# initial value for kappa
kappa_temp = 1.0e11

# Optimize rMax and kappa parameter estimates
# Optimize while setting a lower bound on Rmax
#bb = optim(c(new_Rmax, kappa_temp), runModelMultiOptim, method = "L-BFGS-B", lower = rep(0,10))
bb = optim(new_Rmax, runModel, method = "L-BFGS-B", lower = rep(0,9), control = list(trace = 4, maxit = 1000))


# Run model with optimized Rmax parameters, extract and plot biomasses
params <- setParams(params, kappa = initialParameterValues[9])
params@species_params$R_max = bb$par
sim <- project(params, effort = relative_effort) 

# Plot model results
dev.new()
plot(sim, include_critical = TRUE)
x =params@species_params
