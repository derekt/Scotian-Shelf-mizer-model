
#The goal of this script is to optimize Rmax 

#For some species, the SSB as available from the Ram database. In that case we are using the SSB to optimize Rmax

#For other species, no biomass data was available from the Ram database, so instead we used the survey biomass data to optimize R max

#At the end of the script we will merge the two sets of Rmax values and add them to the species params dataset to run simulations.

library(mizer)
library(tidyverse)

# CHANGE THIS DIRECTORY TO POINT TO GITHUB REPOSITORY
setwd("c:/users/derekt/work/isabellefishery")

# Load helper functions
source("calculate_sse.R")
source("run_model.R")

# Fixed parameters
time_torun_simulation = 10

#let's start with the species with RAM data
# fishing mortality values are averaged from 2000-2010
species_params = read.csv("species_params_species_with_ram_SSB.csv")
names(species_params)[1] = "species"

# Set mizer parameters matrix
params <- newMultispeciesParams(species_params, kappa = 1e11)

# This will display fishing parameters
#params@gear_params

# replace catchabilty with you time-averaged fishing mortality estimates 
params@gear_params$catchability<-species_params$fishing_mortality

# re-set the params object
params<-setParams(params)

# project the params using an effort of 1, later we can use this as a multiplier
# run with fishing. This is not including parameter optimization.
#sim <- project(params, t_max = 100, effort = 1)
#plot(sim)

#ok now fishing is in appropriate params format
#now we need to optimize rmax for the species we have ram data for
# Create a before simulation to compare to post-optimisation
sim <- project(params, effort= 1, t_max = time_torun_simulation) #Change t_max to determine how many years the model runs for
plot(sim, include_critical = TRUE)


# Import the RAM legacy SSB 
temp_file = read.csv("species_biomass_ram.csv")
names(temp_file)[1] = "sjob.COMM"
mean_SSB_2000_2010 = aggregate(SSB.g~sjob.COMM, temp_file, mean)

# Extract complete species parameter matrix
params@species_params

# Create vector with new Rmax values to optimize
new_Rmax = rep(8.26e+09,length(params@species_params$R_max))

# Put new vector back into species parameters
params@species_params$R_max = new_Rmax


# re-set the params object
params<-setParams(params)

# This code extracts the final year of biomass
#biomasses_through_time = getBiomass(sim)
#final_biomasses = biomasses_through_time[time_torun_simulation,]

sse <- calculate_sse(mean_SSB_2000_2010, final_biomasses)

# Estimate Rmax parameters
aa = optim(new_Rmax, runModel)

# Run model with optimized Rmax parameters, extract and plot final biomass
params@species_params$R_max = aa$par
sim <- project(params, effort = 1, t_max = time_torun_simulation) #Change t_max to determine how many years the model runs for

# Plot model results
dev.new()
plot(sim, include_critical = TRUE)


x =params@species_params

#In noram is the species info for the 4 species with no ram data. Their rmax values are set to the default (INF)
noram = read.csv("species_no_ram_data.csv")

#now we create a dataframe that has the optimized Rmax values for 9 species and Inf for the 4 species with no ram data
z= rbind(x,noram)


###################################################################################################
#now we do it with all the species


#import species info including fishing
#This includes all species and not just the ones that had Ram SSB data
species_params = 	z
# set up the params object

params <- newMultispeciesParams(species_params, kappa = 1e11)

params@gear_params

# replace catchabilty with you time-averaged fishing mortality estimates 


params@gear_params$catchability<-species_params$fishing_mortality

# re-set the params object

params<-setParams(params)

# project the params using an effort of 1, later we can use this as a multiplier
# run with fishing
sim <- project(params, t_max = 100, effort = 1)

plot(sim)

time_torun_simulation = 5
sim <- project(params, effort= 1, t_max = time_torun_simulation) #Change t_max to determine how many years the model runs for
plot(sim, include_critical = TRUE)




#import survey weight data
survey_weight = read.csv("survey_species_mean_annual_weight.csv")
# Subset the catch data to get the mean catches between 2000 and 2010
survey_sub = subset(survey_weight, year > 1999 & year < 2011)
Mean_survey_sub = aggregate(sjob.TOTWGT~sjob.COMM, survey_sub, mean)



# Extract complete species parameter matrix
params@species_params

# Create vector with new Rmax values
new_Rmax = rep(8.26e+09,length(params@species_params$R_max))

# Put new vector back into species parameters
params@species_params$R_max = new_Rmax


biomasses_through_time = getBiomass(sim)
final_biomasses = biomasses_through_time[time_torun_simulation,]




sse <- calculate_sse(Mean_survey_sub, final_biomasses)


aa = optim(new_Rmax, runModel)

params@species_params$R_max = aa$par
sim <- project(params, effort = 1, t_max = time_torun_simulation) #Change t_max to determine how many years the model runs for



plot(sim, include_critical = TRUE)
y=params@species_params


############################################################################################
#ok now we need to merge these two datasets
#let's start by subsetting y to only include certain species

y2 = subset(y, species %in% c("SILVER HAKE","NORTHERN SAND LANCE","HALIBUT(ATLANTIC)","WHITE HAKE"))

y2 = subset(y2, select = -c(X) )


joint= rbind(x, y2)

#let's run a simulation

species_params = joint
params <- newMultispeciesParams(species_params, kappa = 1e11)
sim <- project(params, effort = 1, t_max = 100)
plot(sim, include_critical = TRUE)



species_params = joint
# set up the params object
# I have just used deafult values here - you need to check

params <- newMultispeciesParams(species_params, kappa = 1e11)

params@gear_params

# replace catchabilty with you time-averaged fishing mortality estimates 


params@gear_params$catchability<-species_params$fishing_mortality

# re-set the params object

params<-setParams(params)

# project the params using an effort of 1, later we can use this as a multiplier
# run with fishing
sim <- project(params, t_max = 100, effort = 1)

plot(sim)


time_torun_simulation = 10
sim <- project(params, effort= 1, t_max = time_torun_simulation) #Change t_max to determine how many years the model runs for
plot(sim, include_critical = TRUE)

#making plots Julia suggested
plotPredMort(sim)
plotlyPredMort(sim)
#weird mortality results

plotGrowthCurves(sim, percentage = TRUE)
# growth seems ok
