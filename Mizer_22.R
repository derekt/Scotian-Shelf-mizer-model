
#This is the code to run the multispecies model.
#Ugh data frame includes the 14 fish species from the top 20 species 
library(mizer)
species_params<-ugh
params <- newMultispeciesParams(species_params,interaction = NULL)
include_critical = TRUE
sim <- project(params, effort = 0, t_max = 5) #Change t_max to determine how many years the model runs for
plot(sim, include_critical = TRUE)


