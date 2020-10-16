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
   
# FOR ISABELLE TO RUN THERMIZER MODEL USING THE OPTIMIZATION FROM THE OTHER CODE PUT THIS INTO THE SPECIES PARAMS MATRIX
#params@species_params$R_max = sim@params@species_params$R_max


# # Create dummy temperature matrix
# temp_matrix = matrix(nrow = 100 + 48 + 83, ncol =  9)
# for (ii in 1:9)
#   temp_matrix[,ii] = c(rep(10,148), seq(10,12, length.out = 83))


library("mizer")
library("assertthat")

   
   
 ## Set up multispecies model 
#For the FishMIP simulations, we'll be using the species parameters in [Blanchard et al. 2014](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/1365-2664.12238).  These parameters are already included in the mizer package and we will update some of the parameters based on more recent work.
   
#In order to use the therMizer extension, two additional species parameters are needed: `temp_min` and `temp_max`.  These represent the thermal tolerance limits for each species.  You could find these in the literature in physiological or tagging studies.  Alternatively, they can be assumed based on species' vertical and geographic ranges, that of their prey species, or some other information.  Note that these are only needed for the therMizer extention functions that relate to temperature, not those that relate to plankton.
   
#For the North Sea fish species, I have used the values given in Fishbase for thermal ranges (sometimes by clicking on the map). A lot of info is available in Rfishbase but not thermal range. A list for many species is also available in the Supplement of Cheung et al 2013 Nature Climate Change. But in reality these are much wider than the Cheung et al. estimates. For example, North Sea cod tagging studies show a range of -1.5 - 19 degrees C (Righton et al 2010, MEPS).


# Load species parameters
setwd("c:/users/derekt/work/isabellefishery/")
source("optimization_functions.r")

#species_params = read.csv("thermizer_params.csv")
species_params = read.csv("thermizer_species_aquamaps.csv")
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

# Read in the observed SSB
obs_SSB = as(read.csv("c:/users/derekt/work/isabellefishery/SSB_total_grams.csv", row.names = 1), "matrix")
colnames(obs_SSB) <- c('AMERICAN PLAICE', 'COD(ATLANTIC)','HADDOCK', 'HERRING(ATLANTIC)', 'REDFISH UNSEPARATED', 'SPINY DOGFISH', 'WITCH FLOUNDER', 'TURBOT,GREENLAND HALIBUT', 'YELLOWTAIL FLOUNDER')
obs_SSB <- obs_SSB[,1:9]

# TEMPORARILY RESCALE
obs_SSB <- obs_SSB / 1000

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
#sim <- project(params, effort = , t_max = 100, dt=0.25)
#plot(sim)

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

#sim <- project(params, effort = 1, t_max = 100, dt=0.1)
#plot(sim)  

#### Fishing 
#There are two fishing scenarios:
   
#* histsoc: Historical effort until 2017, then fixed at 2017 levels   
#* nat: No fishing  

   
   #effort_Fhistsoc = read.csv("Desktop/f_therMizer.csv")
   effort_Fhistsoc = read.csv("therm_f_best_option_no_blanks.csv")
   
   # Replace the mortality > 1 with a value of 0.9
   effort_Fhistsoc[effort_Fhistsoc >=1] = 0.9

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
   # Remember, the first 100 years are for spin-up
   # During this time, the first input value is repeated at each time step

   for (t in (times - 1869)) {
     if (t <= 101) { 
       effort_array_Fhistsoc[t,] <- effort_Fhistsoc[1,]
       effort_array_Fnat[t,] <- 0
     } else if (t <= 148) {
       effort_array_Fhistsoc[t,] <- effort_Fhistsoc[t - 100,]
       effort_array_Fnat[t,] <- 0
     } else
      effort_array_Fhistsoc[t,] <- effort_Fhistsoc[48,]
      effort_array_Fnat[t,] <- 0
   }
   
   # Set the 1993 and beyond mortality in cod (representing elevated mortality in all size classes,
   # so simultaneously the knife-edge selection goes down to 1e03g
   
   effort_array_Fhistsoc[123:dim(effort_array_Fhistsoc)[1],2] = 0.5
   

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
   
   # Write in tbe empirical / projected temperatures into the array, based on whether the 
   # species is pelagic or demersal
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
   
   # Kappa scaling parameter
   other_params(params)$kappa_scaling = 1000
   
   
#   To scale the effect of temperature on encounter rate to a value ranging from 0 - 1, 
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
   #temp_at_t <- params@other_params$other$ocean_temp[t + params@other_params$other$t_idx,]
   
   #print(ceiling(t + 0.1))
   temp_at_t <- params@other_params$other$ocean_temp[ceiling(t + 0.1),]

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
   temp_at_t <- params@other_params$other$ocean_temp[ceiling(t + 0.1),]
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
   params@resource_params$kappa <- kappa_at_t * params@other_params$other$kappa_scaling
   #params@resource_params$kappa <- params@other_params$other$kappa_forcing[t + params@other_params$other$t_idx]
   
   #print(params@resource_params$kappa)

   #c_p(w) = ?? w^{-??}
   #print(params@cc_pp[9])
   params@cc_pp = params@resource_params$kappa * params@w_full^(params@resource_params$lambda)
   #print(params@cc_pp[9])
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





project <- function (object, effort, t_max = 100, dt = 0.1, t_save = 1, 
          t_start = 0, initial_n, initial_n_pp, append = TRUE, progress_bar = TRUE, 
          ...) 
{
   validObject(object)
   if (is(object, "MizerSim")) {
      params <- object@params
      no_t <- dim(object@n)[[1]]
      initial_n <- params@initial_n
      initial_n[] <- object@n[no_t, , ]
      initial_n_pp <- params@initial_n_pp
      initial_n_pp[] <- object@n_pp[no_t, ]
      initial_n_other <- object@n_other[no_t, ]
      t_start <- as.numeric(dimnames(object@n)[[1]][[no_t]])
   }
   else {
      params <- object
      if (missing(initial_n)) 
         initial_n <- params@initial_n
      if (missing(initial_n_pp)) 
         initial_n_pp <- params@initial_n_pp
      initial_n_other <- params@initial_n_other
   }
   params@initial_n[] <- initial_n
   params@initial_n_pp[] <- initial_n_pp
   params@initial_n_other <- initial_n_other
   no_sp <- length(params@w_min_idx)
   assert_that(is.array(initial_n), is.numeric(initial_n), are_equal(dim(initial_n), 
                                                                     c(no_sp, length(params@w))))
   assert_that(is.numeric(initial_n_pp), length(initial_n_pp) == 
                  length(params@w_full))
   assert_that(is.null(initial_n_other) || is.list(initial_n_other))
   other_names <- names(params@other_dynamics)
   if (length(other_names) > 0) {
      if (is.null(names(initial_n_other))) {
         stop("The initial_n_other needs to be a named list")
      }
      if (!setequal(names(initial_n_other), other_names)) {
         stop("The names of the entries in initial_n_other do not match ", 
              "the names of the other components of the model.")
      }
   }
   if (missing(effort)) 
      effort <- params@initial_effort
   if (is.null(dim(effort))) {
      no_gears <- dim(params@catchability)[1]
      if ((length(effort) > 1) & (length(effort) != no_gears)) {
         stop("Effort vector must be the same length as the number of fishing gears\n")
      }
      gear_names <- dimnames(params@catchability)[[1]]
      effort_gear_names <- names(effort)
      if (length(effort) == 1 & is.null(effort_gear_names)) {
         effort_gear_names <- gear_names
      }
      if (!all(gear_names %in% effort_gear_names)) {
         stop("Gear names in the MizerParams object (", 
              paste(gear_names, collapse = ", "), ") do not match those in the effort vector.")
      }
      time_dimnames <- seq(from = t_start, to = t_start + t_max, 
                           by = dt)
      effort <- t(array(effort, dim = c(no_gears, length(time_dimnames)), 
                        dimnames = list(gear = effort_gear_names, time = time_dimnames)))
   }
   no_gears <- dim(params@catchability)[1]
   if (dim(effort)[2] != no_gears) {
      stop("The number of gears in the effort array (length of the second dimension = ", 
           dim(effort)[2], ") does not equal the number of gears in the MizerParams object (", 
           no_gears, ").")
   }
   gear_names <- dimnames(params@catchability)[[1]]
   if (!all(gear_names %in% dimnames(effort)[[2]])) {
      stop("Gear names in the MizerParams object (", 
           paste(gear_names, collapse = ", "), ") do not match those in the effort array.")
   }
   effort <- effort[, gear_names, drop = FALSE]
   if (is.null(dimnames(effort)[[1]])) {
      stop("The time dimname of the effort argument must be numeric.")
   }
   time_effort <- as.numeric(dimnames(effort)[[1]])
   if (any(is.na(time_effort))) {
      stop("The time dimname of the effort argument must be numeric.")
   }
   if (is.unsorted(time_effort)) {
      stop("The time dimname of the effort argument should be increasing.")
   }
   t_end <- time_effort[length(time_effort)]
   time_effort_dt <- seq(from = time_effort[1], to = t_end, 
                         by = dt)
   effort_dt <- t(array(NA, dim = c(length(time_effort_dt), 
                                    dim(effort)[2]), dimnames = list(time = time_effort_dt, 
                                                                     dimnames(effort)[[2]])))
   for (i in 1:(length(time_effort) - 1)) {
      effort_dt[, time_effort_dt >= time_effort[i]] <- effort[i, 
                                                              ]
   }
   effort_dt <- t(effort_dt)
   if ((t_save < dt) || !isTRUE(all.equal((t_save - round(t_save/dt) * 
                                           dt), 0))) 
      stop("t_save must be a positive multiple of dt")
   t_skip <- round(t_save/dt)
   t_dimnames_index <- seq(1, to = length(time_effort_dt), by = t_skip)
   t_dimnames <- time_effort_dt[t_dimnames_index]
   sim <- MizerSim(params, t_dimnames = t_dimnames)
   sim@effort[] <- effort_dt[t_dimnames_index, ]
   sim@n[1, , ] <- initial_n
   sim@n_pp[1, ] <- initial_n_pp
   sim@n_other[1, ] <- initial_n_other
   no_sp <- nrow(sim@params@species_params)
   no_w <- length(sim@params@w)
   idx <- 2:no_w
   resource_dynamics_fn <- get(sim@params@resource_dynamics)
   other_dynamics_fns <- lapply(sim@params@other_dynamics, get)
   rates_fns <- lapply(sim@params@rates_funcs, get)
   w_min_idx_array_ref <- (sim@params@w_min_idx - 1) * no_sp + 
      (1:no_sp)
   a <- matrix(0, nrow = no_sp, ncol = no_w)
   b <- matrix(0, nrow = no_sp, ncol = no_w)
   S <- matrix(0, nrow = no_sp, ncol = no_w)
   n <- initial_n
   n_pp <- initial_n_pp
   n_other <- initial_n_other
   if (progress_bar == TRUE) {
      pb <- progress::progress_bar$new(format = "[:bar] :percent ETA: :eta", 
                                       total = length(t_dimnames_index), width = 60)
   }
   if (is(progress_bar, "Progress")) {
      progress_bar$set(message = "Running simulation", 
                       value = 0)
      proginc <- 1/length(t_dimnames_index)
   }
   t <- 0
   t_steps <- dim(effort_dt)[1] - 1
   for (i_time in 1:t_steps) {
      r <- rates_fns$Rates(params, n = n, n_pp = n_pp, n_other = n_other, 
                           t = t, effort = effort_dt[i_time, ], rates_fns = rates_fns)
      t <- t + dt
      # Not really suitable to go here, but set the knife-edge gear parameter 
      # to essentially zero to represent 0.5 natural mortality on all size
      # classes of cod at 1993 and beyond
      if (ceiling(t + 0.1) == 123)
      {
         params@gear_params[2,4] = 0.0001
         params@selectivity[2,2,] = 1
      }
      if (t)
      n_other_current <- n_other
      for (res in other_names) {
         n_other[[res]] <- other_dynamics_fns[[res]](params, 
                                                     n = n, n_pp = n_pp, n_other = n_other_current, 
                                                     rates = r, t = t, dt = dt)
      }
      n_pp <- resource_dynamics_fn(params, n = n, n_pp = n_pp, 
                                   n_other = n_other_current, rates = r, t = t, dt = dt)
      a[, idx] <- sweep(-r$e_growth[, idx - 1, drop = FALSE] * 
                           dt, 2, sim@params@dw[idx], "/")
      b[, idx] <- 1 + sweep(r$e_growth[, idx, drop = FALSE] * 
                               dt, 2, sim@params@dw[idx], "/") + r$mort[, 
                                                                        idx, drop = FALSE] * dt
      S[, idx] <- n[, idx, drop = FALSE]
      b[w_min_idx_array_ref] <- 1 + r$e_growth[w_min_idx_array_ref] * 
         dt/sim@params@dw[sim@params@w_min_idx] + r$mort[w_min_idx_array_ref] * 
         dt
      n[w_min_idx_array_ref] <- (n[w_min_idx_array_ref] + r$rdd * 
                                    dt/sim@params@dw[sim@params@w_min_idx])/b[w_min_idx_array_ref]
      n <- inner_project_loop(no_sp = no_sp, no_w = no_w, n = n, 
                              A = a, B = b, S = S, w_min_idx = sim@params@w_min_idx)
      store <- t_dimnames_index %in% (i_time + 1)
      if (any(store)) {
         if (is(progress_bar, "Progress")) {
            progress_bar$inc(amount = proginc)
         }
         if (progress_bar == TRUE) {
            pb$tick()
         }
         t_idx <- which(store)
         sim@n[t_idx, , ] <- n
         sim@n_pp[t_idx, ] <- n_pp
         sim@n_other[t_idx, ] <- n_other
      }
   }
   if (is(object, "MizerSim") && append) {
      no_t_old <- dim(object@n)[1]
      no_t <- length(t_dimnames)
      new_t_dimnames <- c(as.numeric(dimnames(object@n)[[1]]), 
                          t_dimnames[2:length(t_dimnames)])
      new_sim <- MizerSim(params, t_dimnames = new_t_dimnames)
      old_indices <- 1:no_t_old
      new_indices <- seq(from = no_t_old + 1, length.out = no_t - 
                            1)
      new_sim@n[old_indices, , ] <- object@n
      new_sim@n[new_indices, , ] <- sim@n[2:no_t, , ]
      new_sim@n_pp[old_indices, ] <- object@n_pp
      new_sim@n_pp[new_indices, ] <- sim@n_pp[2:no_t, ]
      new_sim@n_other[old_indices, ] <- object@n_other
      new_sim@n_other[new_indices, ] <- sim@n_other[2:no_t, 
                                                    ]
      new_sim@effort[old_indices, ] <- object@effort
      new_sim@effort[new_indices, ] <- sim@effort[2:no_t, ]
      return(new_sim)
   }
   return(sim)
}

inner_project_loop <- function(no_sp, no_w, n, A, B, S, w_min_idx) {
   .Call('_mizer_inner_project_loop', PACKAGE = 'mizer', no_sp, no_w, n, A, B, S, w_min_idx)
}


#``` {r message = FALSE}
#sim_IPSL_ssp5rcp85_histsoc <- project(params_IPSL_ssp5rcp85, t_max = length(times), effort = effort_array_Fhistsoc)
#sim_IPSL_ssp5rcp85_nat <- project(params_IPSL_ssp5rcp85, initial_n_pp = n_pp_array_IPSL_CC585[1,], t_max = length(times), effort = effort_array_Fnat)


new_Rmax = rep(8.26e+09, length(params@species_params$R_max))
params_IPSL_ssp5rcp85@species_params$R_max = new_Rmax


# Optimize over the parameters
ptm <- proc.time()
aa = optim(new_Rmax, runModel, params = params_IPSL_ssp5rcp85, t_max = length(times), effort = effort_array_Fhistsoc)
proc.time() - ptm

# Run the model with the final parameters
params_IPSL_ssp5rcp85@species_params$R_max = aa$par
sim_IPSL_ssp5rcp85_histsoc <- project(params_IPSL_ssp5rcp85, t_max = length(times), effort = effort_array_Fhistsoc)

#```
#And plot the results to get a sense of what things look like.
#``` {r}
dev.new()
# TEMPORARY FIX FOR PRINTING


#getMethod("plot",signature = c(x = "MizerSim", y = "missing"))

# plotFeedingLevel <- function (object, species = NULL, time_range, highlight = NULL, 
#                               all.sizes = FALSE, include_critical = FALSE, ...) 
# {
#    print("Here")
#    if (is(object, "MizerSim")) {
#       if (missing(time_range)) {
#          time_range <- max(as.numeric(dimnames(object@n)$time))
#       }
#       params <- object@params
#       feed <- getFeedingLevel(object, time_range = time_range, 
#                               drop = FALSE, ...)
#    }
#    else {
#       assert_that(is(object, "MizerParams"))
#       params <- object
#       feed <- getFeedingLevel(params, drop = FALSE, ...)
#    }
#    if (length(dim(feed)) == 3) {
#       feed <- apply(feed, c(2, 3), mean)
#    }
#    if (is.null(species)) {
#       species <- dimnames(params@initial_n)$sp[!is.na(params@A)]
#    }
#    sel_sp <- as.character(dimnames(feed)$sp) %in% species
#    feed <- feed[sel_sp, , drop = FALSE]
#    plot_dat <- data.frame(value = c(feed), Species = factor(dimnames(feed)$sp, 
#                                                             levels = dimnames(feed)$sp), w = rep(params@w, each = length(species)))
#    if (!all.sizes) {
#       for (sp in species) {
#          plot_dat$value[plot_dat$Species == sp & (plot_dat$w < 
#                                                      params@species_params[sp, "w_min"] | plot_dat$w > 
#                                                      params@species_params[sp, "w_inf"])] <- NA
#       }
#       plot_dat <- plot_dat[complete.cases(plot_dat), ]
#    }
#    p <- ggplot() + geom_line(aes(x = w, y = value, colour = Species, 
#                                  linetype = Species, size = Species), data = plot_dat)
#    linesize <- rep(0.8, length(params@linetype))
#    names(linesize) <- names(params@linetype)
#    linesize[highlight] <- 1.6
#    p <- p + scale_x_continuous(name = "Size [g]", trans = "log10") + 
#       scale_y_continuous(name = "Feeding Level", limits = c(0, 
#                                                             1)) + scale_colour_manual(values = params@linecolour) + 
#       scale_linetype_manual(values = params@linetype) + scale_size_manual(values = linesize)
#    if (include_critical) {
#       feed_crit <- getCriticalFeedingLevel(params)[sel_sp, 
#                                                    , drop = FALSE]
#       plot_dat_crit <- data.frame(value = c(feed_crit), Species = factor(dimnames(feed)$sp, 
#                                                                          levels = dimnames(feed)$sp), w = rep(params@w, each = length(species)))
#       if (!all.sizes) {
#          for (sp in species) {
#             plot_dat_crit$value[plot_dat_crit$Species == 
#                                    sp & (plot_dat_crit$w < params@species_params[sp, 
#                                                                                  "w_min"] | plot_dat_crit$w > params@species_params[sp, 
#                                                                                                                                     "w_inf"])] <- NA
#          }
#          plot_dat_crit <- plot_dat_crit[complete.cases(plot_dat_crit), 
#                                         ]
#       }
#       p <- p + geom_line(aes(x = w, y = value, colour = Species, 
#                              linetype = Species), data = plot_dat_crit)
#    }
#    return(p)
# }
# 
# getFeedingLevel <- function (object, n, n_pp, n_other, encounter, time_range, drop = FALSE,...) 
# {
#    if (is(object, "MizerParams")) {
#       params <- object
#       if (missing(encounter)) {
#          if (missing(n)) 
#             n <- params@initial_n
#          if (missing(n_pp)) 
#             n_pp <- params@initial_n_pp
#          if (missing(n_other)) 
#             n_other <- params@initial_n_other
#          encounter <- getEncounter(params, n, n_pp, n_other)
#       }
#       if (!all(dim(encounter) == c(nrow(params@species_params), 
#                                    length(params@w)))) {
#          stop("encounter argument must have dimensions: no. species (", 
#               nrow(params@species_params), ") x no. size bins (", 
#               length(params@w), ")")
#       }
#       f <- get(params@rates_funcs$FeedingLevel)
#       return(f(params, encounter = encounter))
#    }
#    else {
#       sim <- object
#       if (missing(time_range)) {
#          time_range <- dimnames(sim@n)$time
#       }
#       time_elements <- get_time_elements(sim, time_range)
#       feed_time <- plyr::aaply(which(time_elements), 1, function(x) {
#          n <- array(sim@n[x, , ], dim = dim(sim@n)[2:3])
#          dimnames(n) <- dimnames(sim@n)[2:3]
#          print("Here")
#          feed <- getFeedingLevel(sim@params, n = n, n_pp = sim@n_pp[x, 
#                                                                     ], n_other = n_other[x, ], ...)
#          return(feed)
#       }, .drop = drop)
#       return(feed_time)
#    }
# }
# 
# getEncounter <- function (params, n = initialN(params), n_pp = initialNResource(params), 
#                           n_other = initialNOther(params), ...) 
# {
#    f <- get(params@rates_funcs$Encounter)
#    f(params, n = n, n_pp = n_pp, n_other = n_other, t = 230, ...)
# }

#plot(sim_IPSL_ssp5rcp85_histsoc, time_range = 1870:2100, t = 231)

plotBiomass(sim_IPSL_ssp5rcp85_histsoc)
dev.new()



# Plot model results
dev.new()
plot(sim_IPSL_ssp5rcp85_histsoc, include_critical = TRUE)
x =params@species_params
biomasses_through_time = getBiomass(sim)
## FEEDING LEVELS VERY HIGH : CHECK PLANKTON COULD BE RELATIVE TO CALIBRATED? CHECK FISHING ALSO RELATIVE? CHECK CATCHABILTY NOT BEING USED
### CHECK GROWTH, CATCHES ETC
#```
#After checking through the code and results to make sure everything worked, we'll save the `sim` objects so that we can prepare the output as FishMIP requests.

#```{r}
save(sim_IPSL_ssp5rcp85_histsoc, file = "sim_IPSL_ssp5rcp85_histsoc.Rdata", ascii = TRUE)
#save(sim_IPSL_ssp5rcp85_nat, file = "sim_IPSL_ssp5rcp85_nat.Rdata", ascii = TRUE)
#```