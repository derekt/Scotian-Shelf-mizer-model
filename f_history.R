#Ok this script is to try and get time varying fishing mortality
#We want to try and reproduce the cod collapse of the 1990s so that we can feel more confirdent in our model before we begin future projections

library(mizer)
species_params = read.csv("species_params_species_with_ram_SSB.csv")
names(species_params)[1] = "species"



params <- newMultispeciesParams(species_params, kappa = 1e11)

gear_params(params)

#First let's import the f_history CSV

f_history <- as(read.csv("Desktop/f_history.csv", row.names = 1), "matrix")
head(f_history)
#ok somehow the column names got messed up
names(f_history)[names(f_history) == 'AMERICAN.PLAICE'] <- 'AMERICAN PLAICE'
names(f_history)[names(f_history) == 'COD.ATLANTIC.'] <- 'COD(ATLANTIC)'
names(f_history)[names(f_history) == 'HERRING.ATLANTIC.'] <- 'HERRING(ATLANTIC)'
names(f_history)[names(f_history) == 'REDFISH..UNSEPARATED.'] <- 'REDFISH UNSEPARATED'
names(f_history)[names(f_history) == 'SPINY.DOGFISH'] <- 'SPINY DOGFISH'
names(f_history)[names(f_history) == 'WITCH.FLOUNDER'] <- 'WITCH FLOUNDER'
names(f_history)[names(f_history) == 'TURBOT.GREENLAND.HALIBUT'] <- 'TURBOT,GREENLAND HALIBUT'
names(f_history)[names(f_history) == 'YELLOWTAIL.FLOUNDER'] <- 'YELLOWTAIL FLOUNDER'




gear_params$catchability <- as.numeric(f_history["1995",])


params <- newMultispeciesParams(species_params, 
                                kappa = 1e11,
                                gear_params = gear_params)
