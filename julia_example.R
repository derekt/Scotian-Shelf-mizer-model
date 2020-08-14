
library(mizer)
library(tidyverse)

# read in data (made up F values, just to show how to set up)

species_params<-read.csv("dummyfile.csv")

# set up the params object
# I have just used deafult values here - you need to check, I also am making up a kappa value

params <- newMultispeciesParams(species_params, kappa = 1e11)

# have a look at the default fishing params - you can either use effort or catchablity to set up these fihsing moratlities
# for now because they are species specific catchability might be easier
# the default size selectivity is a "knife-edge"
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
