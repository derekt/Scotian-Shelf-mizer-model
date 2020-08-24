#The goal of this script is to optimize Rmax 

#For some species, the SSB as available from the Ram database. In that case we are using the SSB to optimize Rmax

#For other species, no biomass data was available from the Ram database, so instead we used the survey biomass data to optimize R max

#At the end of the script we will merge the two sets of Rmax values and add them to the species params dataset to run simulations.

library(mizer)
library(tidyverse)

#let's start with the species with ram data
#import incl_fish_2.csv
species_params = incl_fish_2

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
#ok now fishing is in appropriate params format
#now we need to optimize rmax for the species we have ram data for

time_torun_simulation = 10
sim <- project(params, effort= 1, t_max = time_torun_simulation) #Change t_max to determine how many years the model runs for
plot(sim, include_critical = TRUE)


#import ram_cb_14.csv
Mean_CB_sub = aggregate(SSB.g~sjob.COMM, ram_cb_14, mean)



# Extract complete species parameter matrix
params@species_params

# Create vector with new Rmax values
new_Rmax = rep(8.26e+09,length(params@species_params$R_max))

# Put new vector back into species parameters
params@species_params$R_max = new_Rmax


biomasses_through_time = getBiomass(sim)
final_biomasses = biomasses_through_time[time_torun_simulation,]



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

sse <- calculate_sse(Mean_CB_sub, final_biomasses)

runModel <- function(rMax)
{
  # Put new vector back into species parameters
  params@species_params$R_max = rMax
  
  # Run the model
  sim <- project(params, effort = 1, t_max = time_torun_simulation) #Change t_max to determine how many years the model runs for
  
  # Extract final biomasses
  biomasses_through_time = getBiomass(sim)
  final_biomasses = biomasses_through_time[time_torun_simulation,]
  
  # Calculate SSE
  sse_final <- calculate_sse(Mean_CB_sub, final_biomasses)
  return(sse_final)
}


aa = optim(new_Rmax, runModel)

params@species_params$R_max = aa$par
sim <- project(params, effort = 1, t_max = time_torun_simulation) #Change t_max to determine how many years the model runs for


dev.new()
plot(sim, include_critical = TRUE)
x =params@species_params

###################################################################################################
#now we do it with all the species



#import incl_fish.csv
species_params = incl_fish
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

time_torun_simulation = 10
sim <- project(params, effort= 1, t_max = time_torun_simulation) #Change t_max to determine how many years the model runs for
plot(sim, include_critical = TRUE)




#import CB_14.csv
CB_sub = subset(CB_14, year > 1999 & year < 2011)
Mean_CB_sub = aggregate(sjob.TOTWGT~sjob.COMM, CB_14, mean)



# Extract complete species parameter matrix
params@species_params

# Create vector with new Rmax values
new_Rmax = rep(8.26e+09,length(params@species_params$R_max))

# Put new vector back into species parameters
params@species_params$R_max = new_Rmax


biomasses_through_time = getBiomass(sim)
final_biomasses = biomasses_through_time[time_torun_simulation,]



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

sse <- calculate_sse(Mean_CB_sub, final_biomasses)

runModel <- function(rMax)
{
  # Put new vector back into species parameters
  params@species_params$R_max = rMax
  
  # Run the model
  sim <- project(params, effort = 1, t_max = time_torun_simulation) #Change t_max to determine how many years the model runs for
  
  # Extract final biomasses
  biomasses_through_time = getBiomass(sim)
  final_biomasses = biomasses_through_time[time_torun_simulation,]
  
  # Calculate SSE
  sse_final <- calculate_sse(Mean_CB_sub, final_biomasses)
  return(sse_final)
}


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

time_torun_simulation = 10
sim <- project(params, effort= 1, t_max = time_torun_simulation) #Change t_max to determine how many years the model runs for
plot(sim, include_critical = TRUE)

#making plots Julia suggested
plotPredMort(sim)
plotlyPredMort(sim)
#weird mortality results

plotGrowthCurves(sim, percentage = TRUE)
# growth seems ok

