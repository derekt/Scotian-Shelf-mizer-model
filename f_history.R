#Ok this script is to try and get time varying fishing mortality
#We want to try and reproduce the cod collapse of the 1990s so that we can feel more confirdent in our model before we begin future projections

library(mizer)
setwd("c:/users/derekt/work/isabellefishery/")
species_params = read.csv("species_params_species_with_ram_SSB.csv")
names(species_params)[1] = "species"

params <- newMultispeciesParams(species_params, kappa = 1e11)



#First let's import the f_history CSV

f_history <- as(read.csv("c:/users/derekt/work/isabellefishery/f_history.csv", row.names = 1), "matrix")
colnames(f_history) <- c('AMERICAN PLAICE', 'COD(ATLANTIC)','HADDOCK', 'HERRING(ATLANTIC)', 'REDFISH UNSEPARATED', 'SPINY DOGFISH', 'WITCH FLOUNDER', 'TURBOT,GREENLAND HALIBUT', 'YELLOWTAIL FLOUNDER')


# Set up the reference year for catchability
# for (ii in 1:length(params@gear_params$catchability))
# {
#   temp_species = as.character(params@gear_params$species[ii])
#   location = which(dimnames(f_history)[[2]] == temp_species)
#   params@gear_params$catchability[ii] = as.numeric(f_history["1995",location])
# }

params@gear_params$catchability = rep(1,9)


gear_params <- 
  data.frame(species_params, 
             kappa = 1e11,
             gear_params = params@gear_params,
             sel_func = "sigmoid_length",
             l25 =  c(7.6, 9.8, 8.7, 10.1, 11.5, 19.8, 16.4, 19.8, 11.5),
             l50 = c(8.1, 11.8, 12.2, 20.8, 17.0, 29.0, 25.8, 29.0, 17.0))


params <- newMultispeciesParams(species_params, 
                                kappa = 1e11,
                                gear_params = params@gear_params)

params@gear_params$gear = params@gear_params$species


relative_effort <- sweep(f_history, 2, f_history["1995",],"/")


initial_effort <- matrix(relative_effort[1,], byrow=TRUE, nrow = 100, ncol = ncol(relative_effort), dimnames = list(1870:1969))
relative_effort <- rbind(initial_effort, relative_effort)

sim<- project(params, effort = relative_effort, dt = 0.25)
