
#This is the code to run the multispecies model.
#Ugh data frame includes the 14 fish species from the top 20 species
setwd("c:/users/derekt/work/IsabelleFishery/")
library(mizer)
ugh<-read.csv("ugh.csv", header = TRUE)
names(ugh)[1] = "species"
species_params<-ugh
params <- newMultispeciesParams(species_params,interaction = NULL)
include_critical = TRUE
time_torun_simulation = 50
sim <- project(params, effort = 0, t_max = time_torun_simulation) #Change t_max to determine how many years the model runs for
plot(sim, include_critical = TRUE)

total_biomass_size_spectrum = XX
total_Biomass_background = YY


CB_14 = read.csv("c:/users/derekt/desktop/CB_14.csv")
CB_sub = subset(CB_14, year > 1999 & year < 2011)
Mean_CB_sub = aggregate(sjob.TOTWGT~sjob.COMM, CB_sub, mean)



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
  sim <- project(params, effort = 0, t_max = time_torun_simulation) #Change t_max to determine how many years the model runs for

  # Extract final biomasses
  biomasses_through_time = getBiomass(sim)
  final_biomasses = biomasses_through_time[time_torun_simulation,]
  
  # Calculate SSE
  sse_final <- calculate_sse(Mean_CB_sub, final_biomasses)
  return(sse_final)
}

time_torun_simulation = 50

aa = optim(new_Rmax, runModel)

params@species_params$R_max = aa$par
sim <- project(params, effort = 0, t_max = time_torun_simulation) #Change t_max to determine how many years the model runs for
plot(sim, include_critical = TRUE)






