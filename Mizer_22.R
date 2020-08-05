#This is the code to run the multispecies model.
#Ugh data frame includes the 14 fish species from the top 20 species


setwd("c:/users/derekt/work/IsabelleFishery/")
library(mizer)

# Global parameters
time_torun_simulation_for_optimization = 10
time_torun_simulation_for_checking_persistence = 100

# Read in the species life history parameters from the .csv
ugh<-read.csv("ugh.csv", header = TRUE)
names(ugh)[1] = "species"
species_params<-ugh

# Read the DFO survey catch data
CB_14 = read.csv("c:/users/derekt/desktop/CB_14.csv")

# Subset the catch data to get the mean catches between 2000 and 2010
CB_sub = subset(CB_14, year > 1999 & year < 2011)
Mean_CB_sub = aggregate(sjob.TOTWGT~sjob.COMM, CB_sub, mean)

# Set up the multispecies parameter matrix
params <- newMultispeciesParams(species_params,interaction = NULL)

# Run the simulation once with default parameters
#sim <- project(params, effort = 0, t_max = time_torun_simulation_for_optimization) #Change t_max to determine how many years the model runs for
#plot(sim, include_critical = TRUE)
# Extract complete species parameter matrix
#params@species_params
# Extract final biomasses
#biomasses_through_time = getBiomass(sim)
#final_biomasses = biomasses_through_time[time_torun_simulation_for_optimization,]

# Create vector with new Rmax values - this is the parameter over which we will be optimizing
new_Rmax = rep(8.26e+09,length(params@species_params$R_max))

# Put new vector back into species parameters
params@species_params$R_max = new_Rmax


# Calculate the sum of squared error between the observed trawl survey catches and modelled data
calculate_sse <- function(Mean_CB_sub, final_biomasses)
{
  total_sse = 0

  for (ii in 1:length(final_biomasses))
  {
    row_obs = which(Mean_CB_sub[,1] == names(final_biomasses)[ii])
    sum_sq_temp = (final_biomasses[ii] - Mean_CB_sub[row_obs,2])^2
    # print(sum_sq_temp)
    total_sse = total_sse + as.numeric(sum_sq_temp)
    # print(total_sse)
  }
  
  return(total_sse)
}

# sse <- calculate_sse(Mean_CB_sub, final_biomasses)

# Function to run the model with specific rMax input parameters for optimization
runModel <- function(rMax)
{
  # Put new vector back into species parameters
  params@species_params$R_max = rMax
  
  # Run the model
  sim <- project(params, effort = 0, t_max = time_torun_simulation_for_optimization) #Change t_max to determine how many years the model runs for

  # Extract final biomasses
  biomasses_through_time = getBiomass(sim)
  final_biomasses = biomasses_through_time[time_torun_simulation_for_optimization,]
  
  # Calculate SSE
  sse_final <- calculate_sse(Mean_CB_sub, final_biomasses)
  return(sse_final)
}


# Run the optimization process
aa = optim(new_Rmax, runModel)

# Extract the final optimized parameters and run the model using these parameters for 100 years to examine the fit over a long time period
params@species_params$R_max = aa$par
sim <- project(params, effort = 0, t_max = time_torun_simulation_for_checking_persistence) #Change t_max to determine how many years the model runs for

# Plot the final fit
dev.new()
plot(sim, include_critical = TRUE)

# Extract final biomasses
biomasses_through_time = getBiomass(sim)
final_biomasses = biomasses_through_time[time_torun_simulation_for_checking_persistence,]






