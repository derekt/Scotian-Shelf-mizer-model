#This is the code to run the multispecies model.
#Ugh data frame includes the 14 fish species from the top 20 species


setwd("c:/users/derekt/work/IsabelleFishery/")
library(mizer)

# CHANGE THIS DIRECTORY TO POINT TO GITHUB REPOSITORY
setwd("c:/users/derekt/work/isabellefishery")

# Load helper functions
source("calculate_sse.R")
source("run_model.R")

# Global parameters
time_torun_simulation_for_optimization = 10
time_torun_simulation_for_checking_persistence = 100

# Read in the species life history parameters from the .csv
sp<-read.csv("species_params_no_fish.csv", header = TRUE)
names(sp)[1] = "species"
species_params<-sp

# Read the DFO survey catch data
survey_weight = read.csv("survey_species_mean_annual_weight.csv")

# Subset the catch data to get the mean catches between 2000 and 2010
survey_sub = subset(survey_weight, year > 1999 & year < 2011)
Mean_survey_sub = aggregate(sjob.TOTWGT~sjob.COMM, survey_sub, mean)

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




# sse <- calculate_sse(Mean_survey_sub, final_biomasses)




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






