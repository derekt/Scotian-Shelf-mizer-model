library(mizer)
library(tidyverse)


# CHANGE THIS DIRECTORY TO POINT TO GITHUB REPOSITORY
setwd("c:/users/derekt/work/isabellefishery")

# Load helper functions
source("calculate_sse.R")


# read in data (made up F values, just to show how to set up)

species_params<-read.csv("species_params_with_fishing.csv")

# set up the params object
# I have just used deafult values here - you need to check

params <- newMultispeciesParams(species_params, kappa = 1e11)

# have a look at the default fishing params - you can either use effort or catchablity to set up these fihsing moratlities
# for now because they are species specific catchability is easier
# the deafult size selectivity is a "knife-edge"
# more info  here:https://sizespectrum.org/mizer/reference/setFishing.html

params@gear_params

# replace catchabilty with you time-averaged fishing mortality estimates 
# (mine are made up as couldn't cut and past yours :)


params@gear_params$catchability<-species_params$fishing_mortality

# re-set the params object

params<-setParams(params)

# project the params using an effort of 1, later we can use this as a multiplier
# run with fishing
sim <- project(params, t_max = 100, effort = 1)

plot(sim)

# you can see on the plat that the fishing mortalitys for all species above the minimu size fished is 0.5
# so you can replace with your values now

time_torun_simulation = 100
sim <- project(params, effort= 1, t_max = time_torun_simulation) #Change t_max to determine how many years the model runs for
plot(sim, include_critical = TRUE)

??species_params



# Read the DFO survey catch data
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


dev.new()
plot(sim, include_critical = TRUE)


