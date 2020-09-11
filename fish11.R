#Ok this script is to try and get time varying fishing mortality
#We want to try and reproduce the cod collapse of the 1990s so that we can feel more confirdent in our model before we begin future projections

library(mizer)
setwd("c:/users/derekt/work/isabellefishery/")
#First let's import the f_history CSV

f_history <- as(read.csv("c:/users/derekt/work/isabellefishery/f_history.csv", row.names = 1), "matrix")
colnames(f_history) <- c('AMERICAN PLAICE', 'COD(ATLANTIC)','HADDOCK', 'HERRING(ATLANTIC)', 'REDFISH UNSEPARATED', 'SPINY DOGFISH', 'WITCH FLOUNDER', 'TURBOT,GREENLAND HALIBUT', 'YELLOWTAIL FLOUNDER')

species_params = read.csv("species_params_species_with_ram_SSB.csv")
names(species_params)[1] = "species"


#things I'm adding
species_params = incl_fish_2
params <- newMultispeciesParams(species_params, kappa = 1e11)


params@gear_params$gear= params@gear_params$species



gear_params <- 
  data.frame(species_params, 
             kappa = 1e11,
             gear_params = params@gear_params,
             #sel_func = "sigmoid_length",
             #l25 =  c(7.6, 9.8, 8.7, 10.1, 11.5, 19.8, 16.4, 19.8, 11.5),
             #l50 = c(8.1, 11.8, 12.2, 20.8, 17.0, 29.0, 25.8, 29.0, 17.0)
             selectivity = array(1,c(9,9,100)))





params <- newMultispeciesParams(species_params, 
                                kappa = 1e11,
                                gear_params = params@gear_params)

params@gear_params$gear = params@gear_params$species

#params<-setFishing(params, selectivity = array(1,c(9,9,100)), catchability = params@catchability)


relative_effort <- sweep(f_history, 2, f_history["1995",],"/")


initial_effort <- matrix(relative_effort[1,], byrow=TRUE, nrow = 100, ncol = ncol(relative_effort), dimnames = list(1870:1969))
relative_effort <- rbind(initial_effort, relative_effort)

sim<- project(params, effort = relative_effort, dt = 0.25)
params@catchability
params@gear_params$gear

class(params@catchability)

params@catchability<-diag(x=1,nrow=9,ncol=9)
colnames(params@catchability)<-c('AMERICAN PLAICE', 'COD(ATLANTIC)','HADDOCK', 'HERRING(ATLANTIC)', 'REDFISH UNSEPARATED', 'SPINY DOGFISH', 'WITCH FLOUNDER', 'TURBOT,GREENLAND HALIBUT', 'YELLOWTAIL FLOUNDER')
rownames(params@catchability)<-c('AMERICAN PLAICE', 'COD(ATLANTIC)','HADDOCK', 'HERRING(ATLANTIC)', 'REDFISH UNSEPARATED', 'SPINY DOGFISH', 'WITCH FLOUNDER', 'TURBOT,GREENLAND HALIBUT', 'YELLOWTAIL FLOUNDER')
params@catchability


params@selectivity<-diag(x=1,nrow=9,ncol=9)
colnames(params@selectivity)<-c('AMERICAN PLAICE', 'COD(ATLANTIC)','HADDOCK', 'HERRING(ATLANTIC)', 'REDFISH UNSEPARATED', 'SPINY DOGFISH', 'WITCH FLOUNDER', 'TURBOT,GREENLAND HALIBUT', 'YELLOWTAIL FLOUNDER')
rownames(params@selectivity)<-c('AMERICAN PLAICE', 'COD(ATLANTIC)','HADDOCK', 'HERRING(ATLANTIC)', 'REDFISH UNSEPARATED', 'SPINY DOGFISH', 'WITCH FLOUNDER', 'TURBOT,GREENLAND HALIBUT', 'YELLOWTAIL FLOUNDER')
params@selectivity


relative_effort <- sweep(f_history, 2, f_history["1995",],"/")


initial_effort <- matrix(relative_effort[1,], byrow=TRUE, nrow = 100, ncol = ncol(relative_effort), dimnames = list(1870:1969))
relative_effort <- rbind(initial_effort, relative_effort)

sim<- project(params, effort = relative_effort, dt = 0.25)
