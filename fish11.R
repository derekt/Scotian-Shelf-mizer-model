#Ok this script is to try and get time varying fishing mortality
#We want to try and reproduce the cod collapse of the 1990s so that we can feel more confirdent in our model before we begin future projections

library(mizer)
setwd("c:/users/derekt/work/isabellefishery/")
#First let's import the f_history CSV

f_history <- as(read.csv("c:/users/derekt/work/isabellefishery/f_history.csv", row.names = 1), "matrix")
colnames(f_history) <- c('AMERICAN PLAICE', 'COD(ATLANTIC)','HADDOCK', 'HERRING(ATLANTIC)', 'REDFISH UNSEPARATED', 'SPINY DOGFISH', 'WITCH FLOUNDER', 'TURBOT,GREENLAND HALIBUT', 'YELLOWTAIL FLOUNDER')

species_params = read.csv("species_params_species_with_ram_SSB.csv")
names(species_params)[1] = "species"

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


# Create a temporal effort matrix
relative_effort <- sweep(f_history, 2, f_history["1995",],"/")
initial_effort <- matrix(relative_effort[1,], byrow=TRUE, nrow = 100, ncol = ncol(relative_effort), dimnames = list(1870:1969))
relative_effort <- rbind(initial_effort, relative_effort)

# Run the simulation
sim<- project(params, effort = relative_effort, dt = 0.25)
params@catchability
params@gear_params$gear

# Pseudo code for calculating SSE for time-series
function optimize_time_series <- (ram_ssb, model_ssb)
{
  total_sse = 0
  loop (species)
  {
    loop (year 1970:2010)
    {
      total_sse = total_sse + (ram_ssb(species, year) - model_ssb(species, year))^2
    }
  }
}

