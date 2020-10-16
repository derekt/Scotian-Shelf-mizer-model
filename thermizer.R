
# ---
# title: "run_therMizer_FishMIP"
# author: "julia.blanchard@utas.edu.au"
# date: "2/9/2020"
# output:
#   html_document:
#   toc: yes
# fig_width: 10
# fig_height: 5
# ---
  
  # [Modified from Phoebe Woodworth-Jefcoat's original Hawaii longline example, for the North Sea]
  #  
  #  ## Purpose
  #  These runs are for the regional model of North Sea fish community and multispecies fisheries for FishMIP simulations using therMizer. They include temperature and plankton forcing from the CMIP6 models.  This code has been adapted from Phoebe Woodworth-Jefcoats' script for the Hawaii longline fishery (https://github.com/pwoodworth-jefcoats/therMizer-FishMIP-2020-HI). The same approach can be adapted for any regional mizer model using this script, provided the regional forcing files have been obtained (available from FishMip coordinators).
  #  
   ## Load libraries
   # Mizer version 2.0.3 or later is needed to use the extension. 
   # 
   # ```{r message = FALSE, warning = FALSE}
   # if (!require("mizer", character.only = TRUE)) {
   #   install.packages("mizer")
   # } else if (packageVersion("mizer") < "2.0.3") {
   #   install.packages("mizer")
   # }
   


# FOR ISABELLE TO RUN THERMIZER MODEL PUT THIS INTO THE SPECIES PARAMS MATRIX
#params@species_params$R_max = sim@params@species_params$R_max


# # Create dummy temperature matrix
# temp_matrix = matrix(nrow = 100 + 48 + 83, ncol =  9)
# for (ii in 1:9)
#   temp_matrix[,ii] = c(rep(10,148), seq(10,12, length.out = 83))


library("mizer")

   
   
 ## Set up multispecies model 
#For the FishMIP simulations, we'll be using the species parameters in [Blanchard et al. 2014](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/1365-2664.12238).  These parameters are already included in the mizer package and we will update some of the parameters based on more recent work.
   
#In order to use the therMizer extension, two additional species parameters are needed: `temp_min` and `temp_max`.  These represent the thermal tolerance limits for each species.  You could find these in the literature in physiological or tagging studies.  Alternatively, they can be assumed based on species' vertical and geographic ranges, that of their prey species, or some other information.  Note that these are only needed for the therMizer extention functions that relate to temperature, not those that relate to plankton.
   
#For the North Sea fish species, I have used the values given in Fishbase for thermal ranges (sometimes by clicking on the map). A lot of info is available in Rfishbase but not thermal range. A list for many species is also available in the Supplement of Cheung et al 2013 Nature Climate Change. But in reality these are much wider than the Cheung et al. estimates. For example, North Sea cod tagging studies show a range of -1.5 - 19 degrees C (Righton et al 2010, MEPS).
   

# Load species parameters
setwd("c:/users/derekt/work/isabellefishery/")

species_params = read.csv("thermizer_params.csv")
names(species_params)[1] = "species"

# Load the IPSL temperature data
surface_temp = read.csv("surface_temp.csv", header = T)
bottom_temp = read.csv("bottom_temp.csv", header = T)
plankton = read.csv("plankton_conc.csv", header = T)

# Load the empirical temperature data
empirical_temp = read.csv("empirical_temperature_data.csv", header = T)

# Extract the anomoly from the IPSL data
surface_temp = surface_temp - mean(surface_temp[1:48,3])
bottom_temp = bottom_temp - mean(bottom_temp[1:48,3])

# Now that we have the anomoly from 2018 to 2020, replace the 1970-2017 with the empirical data
surface_temp[1:48,3] = empirical_temp[,5]
surface_temp[49:dim(surface_temp)[1],3] = surface_temp[49:dim(surface_temp)[1],3] + mean(empirical_temp[1:48,5])
bottom_temp[1:48,3] = empirical_temp[,2]
bottom_temp[49:dim(bottom_temp)[1],3] = bottom_temp[49:dim(bottom_temp)[1],3] + mean(empirical_temp[1:48,2])

# Do the five year smoothing
yy = as.vector(filter(surface_temp[,3], rep(1/5,5)))
surface_temp[3:(dim(surface_temp)[1] - 3),3] = yy[3:(length(yy) - 3)]
yy = as.vector(filter(bottom_temp[,3], rep(1/5,5)))
bottom_temp[3:(dim(bottom_temp)[1] - 3),3] = yy[3:(length(yy) - 3)]
#plot(surface_temp[,3], type = "l", ylim = c(0,21))
#lines(bottom_temp[,3], type = "l", col = "blue")


# Set up gear params and selectivity function

selectivity_function <- function(w ,...)
{
  return(1)
}

gear_params <- 
  data.frame(species = species_params$species,
             gear = species_params$species,
             sel_func = "selectivity_function")

gear_params$catchability <- rep(1,dim(gear_params)[2])
View(gear_params)
# Create the params object 
params <- newMultispeciesParams(species_params, 
                                kappa = 1e11,
                                gear_params = gear_params)
# Run to equilibrium
sim <- project(params, effort = , t_max = 100, dt=0.25)
plot(sim)

# Load the selectivity matrix for knife-edge selectivity
#fishing_type = read.csv("Desktop/Mat_knife_edge_gear_params2.csv")
fishing_type = read.csv("Mat_knife_edge_gear_params2.csv")
names(fishing_type)[1] = "species"
#fishing_type=Mat_knife_edge_gear_params2

gear_params <- fishing_type
names(gear_params)[1] = "species"

# This essentially runs the model without any species fished
#gear_params[,4] = rep(1000000,9)

#### TEMPORARY
species_params[,5] = rep(0.8,9)

# Set up parameters data frame. Very sensitive to the value of kappa
params <- newMultispeciesParams(species_params, 
                                kappa = 5e12,
                                gear_params = gear_params)

sim <- project(params, effort = 1, t_max = 100, dt=0.1)
plot(sim)  
## Load data needed for FishMIP scenarios
#This round of FishMIP simulations includes three forcing inputs: fishing mortality, temperature, plankton.  We'll load them in that order.
   
#### Fishing 
#There are two fishing scenarios:
   
#* histsoc: Historical effort until 2017, then fixed at 2017 levels   
#* nat: No fishing  
   
#The time series of fishing mortality, "FishingEffort.dat" was created with "CreateF.Rmd".  We're also going to create another time series with F = 0 so that we can use the same years for all our scenarios.  This makes incorporating the temperature functions easier later on because all the time values will agree.
   

   # Load fishing scenario
   # read saved data
  # install.packages("readr")
  #   library(readr)
  #  effort_Fhistsoc<-readR("Downloads/FishingEffort.RDS")
   
   #effort_Fhistsoc = read.csv("Desktop/f_therMizer.csv")
   effort_Fhistsoc = read.csv("f_therMizer.csv")
   
   
   # Build fishing effort arrays
   #  we'll begin the model with 100 years of constant input for spinup.  This means the first year will be 1870 (1970 - 100).
   times <- seq(1870, 2100, by = 1)
   gear_names <-  gear_params(params)$gear
   effort_array_Fhistsoc <- array(NA, dim = c(length(times), length(gear_names)), dimnames = list(time = times, gear = gear_names))
   effort_array_Fnat <- array(NA, dim = c(length(times), length(gear_names)), dimnames = list(time = times, gear = gear_names))
   
   # Remove the year as unneeded
   effort_Fhistsoc <- effort_Fhistsoc[,2:dim(effort_Fhistsoc)[2]]
   effort_Fhistsoc <- as.matrix(effort_Fhistsoc)
   
#    # Now fill the array
   # Remember, the first 600 years are for spin-up
   # During this time, the first input value is repeated at each time step

   for (t in (times - 1869)) {
     if (t <= 101) { 
       effort_array_Fhistsoc[t,] <- effort_Fhistsoc[1,]
       effort_array_Fnat[t,] <- 0
     } else {
       effort_array_Fhistsoc[t,] <- effort_Fhistsoc[t - 100,]
       effort_array_Fnat[t,] <- 0
     }
   }
   
   # Add in the additional natural mortality for cod
   effort_array_Fhistsoc[125:dim(effort_array_Fhistsoc)[1],2] = 1.0
   
   # check it runs:
   # simt <- project(params, effort = effort_array_Fhistsoc, dt = 0.25, t_save = 1)
   # 
   # plot(simt)

   
   #### Temperature
   
   #There is one climate scenarios, with different temperature forcing for each:  

   #* ssp585: Simulated historical climate, then SSP5-RCP8.5 climate
   
#Note that only the years 1970 - 2100 are needed.  Temperature forcing files were created with "PrepTemperature_GFDL.Rmd" and "PrepTemperature_IPSL.Rmd".  
   
 
   # Load data for each CMIP6 model: GFDL-ESM4 and IPSL-CM6A-LR
   # IPSL_temperature_CC585 <- read.table("IPSL_ocean_temp_array_CCscenario_585.dat")
   # IPSL_temperature_CC585 <- as(IPSL_temperature_CC585, "matrix")
   
   # Build temperature arrays following methods used above
   species <- params@species_params$species
   
    ocean_temp_array_IPSL_CC585 <- array(NA, dim = c(length(times), length(species)), dimnames = list(time = times, sp = species))
   
   # 
   # for (t in (times - 1349)) {
   #   if (t <= 601) { 
   #     ocean_temp_array_IPSL_CC585[t,] <- IPSL_temperature_CC585[1,]
   #   } else {
   #     ocean_temp_array_IPSL_CC585[t,] <- IPSL_temperature_CC585[t - 600,]
   #   }
   # }
   
   # Write in temperatures into the array
   ocean_temp_array_IPSL_CC585[,c(1,2,3,5,6,7,8,9)] = c(rep(bottom_temp[1,3],100), bottom_temp[,3])
   ocean_temp_array_IPSL_CC585[,4] = c(rep(surface_temp[1,3],100), surface_temp[,3])
   
   #### Plankton
   
   # There is also different plankton forcing for each of the three climate scenarios.  These plankton forcings are used to create the resource spectra (which include more than plankton).  Plankton forcing files were created with "PrepPlankton_scaled.Rmd".
   # 
   # *JB NOTE : Check area assumption is OK.
   # 
   # ```{r}
   # # Load data for each CMIP6 model: GFDL-ESM4 and IPSL-CM6A-LR
   #IPSL_n_pp_CC585 <- read.table("IPSL_n_pp_array_CCscenario_585_scaled_S1.0I0.85.dat")
   #IPSL_n_pp_CC585 <- as(IPSL_n_pp_CC585, "matrix")
   
   
   # # Build plankton arrays to be filled
   #sizes <- params@w_full
   
   
   #n_pp_array_IPSL_CC585 <- array(NA, dim = c(length(times), length(sizes)), dimnames = list(time = times, w = sizes))
   
   # # Fill arrays, remembering that values in n_pp are log10 abundances.
   # # They'll need to be transformed (inverse log10) and divided by bin width (dw_full)
   # ### JB: COULD DO THIS IN PLANKTON_PREP?
   # for (t in (times - 1349)) {
   #   if (t <= 601) { 
    #     n_pp_array_IPSL_CC585[t,] <- (10^(IPSL_n_pp_CC585[1,]))/params@dw_full
   #   } else {
   #     n_pp_array_IPSL_CC585[t,] <- (10^(IPSL_n_pp_CC585[t - 600,]))/params@dw_full
   #   }
   # }
   # ### JB: replace all values above params@resource_params$w_pp_cutoff with zeroes
   # wcut_index<-which(params@w_full>=params@resource_params$w_pp_cutoff)
    # n_pp_array_IPSL_CC585[,wcut_index] <- 0
   # ```
   # 
   # ## Write parameters and functions for therMizer extension
   # 
   # The therMizer extension that allows us to use the temperature and plankton forcings includes a few new parameters that we can determine based on the user input above.  There are also several functions that we'll write, too.
   
   #### Parameters
   
  # The parameter `t_idx` will help with the simulations by providing the correct time index for the plankton and temperature forcing during the simulations.
   
   #``` {r}
   # Time indexing parameter
   # This will be added to t to convert the year into an index for the n_pp and ocean_temp arrays
   if (min(times) == 0) {
   other_params(params)$t_idx = 1
   } else if (min(times) == 1) {
   other_params(params)$t_idx = 0
   } else {
   other_params(params)$t_idx = -(min(times) - 1)
   }
   #```
   
# To scale the effect of temperature on encounter rate to a value ranging from 0 - 1, 
   #it is necessary to divide by the maximum possible value for each species.  
   #To scale the effect of temperature on metabolism to a value ranging from 0 - 1, 
   #it is necessary to subtract the minimum vaule for each species and then divide by the range.  T
   # This requires a bit of straightforward arithmetic, and users could do this on their end if they're 
   #so inclined.  These parameters handle that math so the user doesn't have to.
   
   #```{r}
   species_params(params)$encounter_scale <- rep(NA, length(params@species_params$temp_min))
   for (indv in seq(1:length(params@species_params$temp_min))) {
   
   # Create a vector of all temperatures each species has in its thermal nicheS
   temperature <- seq(params@species_params$temp_min[indv], params@species_params$temp_max[indv], by = 0.1)
   
   # Find the maximum value of the unscaled effect of temperature on encounter rate for each species 
   species_params(params)$encounter_scale[indv] <- max((temperature) * (temperature - params@species_params$temp_min[indv]) * (params@species_params$temp_max[indv] - temperature))
   }
   # Determine the minimum, maximum, and range of value for the effect of temperature on metabolism
   min_metab_value <- (exp(25.22 - (0.63/((8.62e-5)*(273 + params@species_params$temp_min)))))
   max_metab_value <- (exp(25.22 - (0.63/((8.62e-5)*(273 + params@species_params$temp_max)))))
   
   species_params(params)$metab_min <- min_metab_value
   species_params(params)$metab_range <- max_metab_value - min_metab_value
   #```
   
   #### Functions
   
   # Temperature will be added to the fuctions that determine encounter rate and energy available for growth and reproduction.
   # 
   # To scale encounter rate with temperature, we're essentially taking a temperature-dependent proportion of the value calculated by the mizerEncounter function.  If species are at their thermal optimum, we take the full value.  Elsewhere in their thermal range, we take a proportion that goes to zero at the limits of species' thermal tolerence.  
   #```{r message = FALSE, warning = FALSE}
   therMizerEncounter <- function(params, n, n_pp, n_other, t, ...) {
   
   # Access the correct element
   temp_at_t <- params@other_params$other$ocean_temp[t + params@other_params$other$t_idx,]
      #print(ceiling(t + 0.1))
      temp_at_t <- params@other_params$other$ocean_temp[ceiling(t + 0.1),]
      print(temp_at_t[1])
   
   # Calculate unscaled temperature effect using a generic polynomial rate equation
   unscaled_temp_effect <- temp_at_t * (temp_at_t - params@species_params$temp_min) * (params@species_params$temp_max - temp_at_t)

   # Scale using encounter_scale parameter
   scaled_temp_effect <- unscaled_temp_effect / params@species_params$encounter_scale

   # Set the encounter rate to zero if temperature is outside species' thermal tolerance
   above_max <- which(temp_at_t > params@species_params$temp_max)
   below_min <- which(temp_at_t < params@species_params$temp_min)
   
   if (length(above_max) > 0)
     scaled_temp_effect[above_max] = 0
   
   if (length(below_min) > 0)
     scaled_temp_effect[below_min] = 0
   
   # Calculate maximum possible encounter rate
   max_encounter <- mizerEncounter(params, n = n, n_pp = n_pp, n_other = n_other, ...)

   # Apply temperature effect
   return(max_encounter * scaled_temp_effect)
   
   }
#```

#To calculate the effect of temperature on metabolim, we use an Arrhenius function to scale the cost of metabolism.  When species are at their thermal maximum, the cost of metabolism is at its maximum.  When species are at their thermal minimum, the cost of metabolism is at its minimum

#```{r message = FALSE, warning = FALSE}
therMizerEReproAndGrowth <- function(params, n, n_pp, n_other, t, encounter,
                                     feeding_level, ...) {
  
  # Access the correct element
  #temp_at_t <- params@other_params$other$ocean_temp[t + params@other_params$other$t_idx,]
   #print(ceiling(t + 0.1))
   temp_at_t <- params@other_params$other$ocean_temp[ceiling(t + 0.1),]
   print(temp_at_t[1])
  # Arrhenius equation
  unscaled_temp_effect <- (exp(25.22 - (0.63/((8.62e-5)*(273 + temp_at_t)))))
  
  # Arrhenius equation scaled to a value between 0 and 1
  temp_effect_metabolism <- (unscaled_temp_effect - params@species_params$metab_min) / params@species_params$metab_range
  
  # Set the EReproAndGrowth to zero if temperature is outside species' thermal tolerance
  Emultiplier <- rep(1, length(params@species_params$species))
  
  above_max <- which(temp_at_t > params@species_params$temp_max)
  below_min <- which(temp_at_t < params@species_params$temp_min)
  
  if (length(above_max) > 0)
    Emultiplier[above_max] = 0
  
  if (length(below_min) > 0)
    Emultiplier[below_min] = 0
  
  # Apply scaled Arrhenius value to metabolism
  (sweep((1 - feeding_level) * encounter, 1, params@species_params$alpha, "*", check.margin = FALSE) - params@metab*temp_effect_metabolism)*Emultiplier  
  
}
#```

#Now we'll write a function to use the CMIP6 plankton densities rather than the mizer resource dynamics.  Code for this was informed by the approach used in: <https://rpubs.com/gustav/plankton-anchovy>.  
# 
# ``` {r}
# # Set up new resource forcing "function" - just changes to different time slot in the n_pp_array array
# plankton_forcing <- function(params, t, ...) {
# return(other_params(params)$n_pp_array[t + params@other_params$other$t_idx,])  
# }
# ```
# 
# Set the new rate functions and new resource
# 
# ```{r}
params <- setRateFunction(params, "Encounter", "therMizerEncounter")
params <- setRateFunction(params, "EReproAndGrowth", "therMizerEReproAndGrowth")
# params <- setResource(params, resource_dynamics = "plankton_forcing")
# ```

## Run the simulations

use_empirical_kappa <- function (params, n, n_pp, n_other, rates, t, dt, ...) 
{
   #print(t)
   #print(params@other_params$other$t_idx)
   #print(t + params@other_params$other$t_idxs)
   # Access the correct element
   #kappa_at_t <- params@other_params$other$kappa_forcing[t + params@other_params$other$t_idx,3]
   kappa_at_t <- params@other_params$other$kappa_forcing[ceiling(t)]
   params@resource_params$kappa <- kappa_at_t
   #params@resource_params$kappa <- params@other_params$other$kappa_forcing[t + params@other_params$other$t_idx]
   
   #print(params@resource_params$kappa)

   #c_p(w) = ?? w^{-??}
   #print(params@cc_pp[9])
   params@cc_pp = params@resource_params$kappa * params@w_full^(params@resource_params$lambda)
   print(params@cc_pp[9])
   tmp <- params@rr_pp * params@cc_pp/(params@rr_pp + rates$resource_mort)
   return(tmp - (tmp - n_pp) * exp(-(params@rr_pp + rates$resource_mort) * 
                                      dt))
}

params <- setResource(params, resource_dynamics = "use_empirical_kappa")
#params<- setResource(params, resource_dynamics = "resource_semichemostat")

#Because temperature and n_pp forcing are parameters, we'll need to make unique `params` objects for each CMIP6 model and climate scenario.  This means we'll have six new `params` objects (2 models x 3 climate scenarios).

#``` {r}
# Create parameter objects
# params_GFDL_picontrol <- params
# params_GFDL_ssp1rcp26 <- params
# params_GFDL_ssp5rcp85 <- params
# params_IPSL_picontrol <- params
# params_IPSL_ssp1rcp26 <- params
params_IPSL_ssp5rcp85 <- params
# Attach temperature
other_params(params_IPSL_ssp5rcp85)$kappa_forcings <- c(rep(plankton[1,3],100), plankton[,3])
# Attach plankton
other_params(params_IPSL_ssp5rcp85)$ocean_temp <- ocean_temp_array_IPSL_CC585

# ```

#Now we can run the simulations.  There are 12 runs here: 3 climate scenarios x 2 fishing scenarios x 2 #CMIP6 models.

# When running calling `project`, we'll also provide the vector for `initial_n_pp`, which is simply the first value from the n_pp arrays.

#``` {r message = FALSE}
sim_IPSL_ssp5rcp85_histsoc <- project(params_IPSL_ssp5rcp85, t_max = length(times), effort = effort_array_Fhistsoc)
#sim_IPSL_ssp5rcp85_nat <- project(params_IPSL_ssp5rcp85, initial_n_pp = n_pp_array_IPSL_CC585[1,], t_max = length(times), effort = effort_array_Fnat)



#```
#And plot the results to get a sense of what things look like.

#``` {r}
dev.new()
plot(sim_IPSL_ssp5rcp85_histsoc)

## FEEDING LEVELS VERY HIGH : CHECK PLANKTON COULD BE RELATIVE TO CALIBRATED? CHECK FISHING ALSO RELATIVE? CHECK CATCHABILTY NOT BEING USED
### CHECK GROWTH, CATCHES ETC
#```
#After checking through the code and results to make sure everything worked, we'll save the `sim` objects so that we can prepare the output as FishMIP requests.

#```{r}
save(sim_IPSL_ssp5rcp85_histsoc, file = "sim_IPSL_ssp5rcp85_histsoc.Rdata", ascii = TRUE)
#save(sim_IPSL_ssp5rcp85_nat, file = "sim_IPSL_ssp5rcp85_nat.Rdata", ascii = TRUE)
#```