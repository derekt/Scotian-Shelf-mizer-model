
#Sept 29th
#Goal of this script is to compare modelled SSB to observed SSB from Ram database

#loading results from model run that Derek put on github
#load("/Users/isabellehurley/Downloads/model_output.Rdata")
#load("model_output.Rdata")
setwd("c:/users/derekt/work/isabellefishery/")
load("sim_IPSL_ssp5rcp85_histsoc.Rdata")


#sim=model_output
plotBiomass(sim_IPSL_ssp5rcp85_histsoc)
params=sim_IPSL_ssp5rcp85_histsoc@params
params@species_params


#Oct 4th
#now I want to try and add another plot with fishing mortality


#f_history  = as(read.csv("Desktop/f_history_no_blanks.csv", row.names = 1), "matrix")
f_history  = as(read.csv("therm_f_best_option_no_blanks.csv", row.names = 1), "matrix")

head(f_history)

f_history <- f_history[,1:9]
head(f_history)
class(f_history)
colnames(f_history) <- c('AMERICAN PLAICE', 'COD(ATLANTIC)','HADDOCK', 'HERRING(ATLANTIC)', 'REDFISH UNSEPARATED', 'SPINY DOGFISH', 'WITCH FLOUNDER', 'TURBOT,GREENLAND HALIBUT', 'YELLOWTAIL FLOUNDER')

obs_SSB = as(read.csv("c:/users/derekt/work/isabellefishery/SSB_total_grams.csv", row.names = 1), "matrix")
colnames(obs_SSB) <- c('AMERICAN PLAICE', 'COD(ATLANTIC)','HADDOCK', 'HERRING(ATLANTIC)', 'REDFISH UNSEPARATED', 'SPINY DOGFISH', 'WITCH FLOUNDER', 'TURBOT,GREENLAND HALIBUT', 'YELLOWTAIL FLOUNDER')
obs_SSB <- obs_SSB[,1:9]

## check obs vs. predicted SSB
pred_SSB <-getSSB(sim_IPSL_ssp5rcp85_histsoc)
pred_SSB = pred_SSB[101:148,]


# Calculate means in the right format
pred_SSB_mean = apply(pred_SSB, 2, mean)
pred_SSB_mean = melt(pred_SSB_mean)

obs_SSB_mean = apply(obs_SSB, 2, mean, na.rm=T)

ssb_pred_obs = cbind(pred_SSB_mean, obs_SSB_mean)
ssb_pred_obs$species <- row.names(ssb_pred_obs)

ssb_pred_obs <- as.data.frame(ssb_pred_obs)
names(ssb_pred_obs) = c( "pred", "obs", "species")

# Calculate means in the right format
fish_obs_mean = apply(f_history, 2, mean)
fish_obs_mean = melt(fish_obs_mean)
 
ssb_pred_obs_fish = cbind(pred_SSB_mean, obs_SSB_mean,fish_obs_mean)        
ssb_pred_obs_fish$species <- row.names(ssb_pred_obs_fish)

ssb_pred_obs_fish <- as.data.frame(ssb_pred_obs_fish)
names(ssb_pred_obs_fish) = c( "pred", "obs", "fishing mortality", "species")



#^ok great, so now observed SSB, predicted SSB and observed fishing mortality are all in the same dataframe
#now let's make a plot with 3 panels
years = 1970:2017
for (ii in 1:9)
{
  dev.new()
  min_biomass = min(min(obs_SSB[,ii], na.rm=T), min(min(pred_SSB[,ii])))
  max_biomass = max(max(obs_SSB[,ii], na.rm=T), max(max(pred_SSB[,ii])))
  
  par(mfrow = c(2,1))
  plot(years, obs_SSB[,ii], xlab = "Year", ylab = "SSB", type = "l", ylim = c(min_biomass, max_biomass + 100000000000), main = ssb_pred_obs$species[ii])
  lines(years, pred_SSB[,ii], col= "red")
  legend("topright", legend = c("Predicted SSB", "Observed SSB"), col = c("red", "black"), lty=1, cex=0.8)
  plot(years, f_history[,ii], xlab = "Year", ylab = "fishing mortality", type = "l", ylim = c(0, 1))
  
}

###########################################################################################################
#now we're going to do the same thing with catch
# 
# 
# 
# #obs_catch = as(read.csv("Desktop/obs_catch.csv", row.names = 1), "matrix")
# #colnames(obs_catch) <- c('AMERICAN PLAICE', 'COD(ATLANTIC)','HADDOCK', 'HERRING(ATLANTIC)', 'REDFISH UNSEPARATED', 'SPINY DOGFISH', 'WITCH FLOUNDER', 'TURBOT,GREENLAND HALIBUT', 'YELLOWTAIL FLOUNDER')
# #obs_catch <- obs_catch[,1:9]
# 
# ## check obs vs. predicted catch
# 
# #pred_yield <-getYield(sim_IPSL_ssp5rcp85_histsoc)
# #pred_yield = pred_yield[101:148,]
# 
# # Calculate means in the right format
# #pred_yield_mean = apply(pred_yield, 2, mean)
# #pred_yield_mean = melt(pred_yield_mean)
# 
# #obs_catch_mean = apply(obs_catch, 2, mean, na.rm=T)
# 
# #catch_pred_obs = cbind(pred_yield_mean, obs_catch_mean)
# #catch_pred_obs$species <- row.names(catch_pred_obs)
# 
# #catch_pred_obs <- as.data.frame(catch_pred_obs)
# #names(catch_pred_obs) = c( "pred", "obs", "species")
# 
# #library(ggplot2)
# #p <- ggplot() + # plot predicted and observed yields
#   geom_point(data = catch_pred_obs, 
#              aes(x = log10(pred),  y = log10(obs), color = species)) +
#   # plot optimal fit line
#   geom_abline(color = "black", slope = 1, intercept = 0) + 
#   xlab("log10 Predicted catch") + 
#   ylab("log10 Observed catch") #+
# #scale_fill_manual(values = wes_palette(12, "Zissou")) 
# plot(p)
# 
# #timeseries plots
# years = 1970:2017
# for (ii in 1:9)
# {
#   dev.new()
#   min_biomass = min(min(obs_catch[,ii], na.rm=T), min(min(pred_yield[,ii])))
#   max_biomass = max(max(obs_catch[,ii], na.rm=T), max(max(pred_yield[,ii])))
#   
#   par(mfrow = c(2,1))
#   plot(years, obs_catch[,ii], xlab = "Year", ylab = "catch", type = "l", ylim = c(min_biomass, max_biomass), main = catch_pred_obs$species[ii])
#   lines(years, pred_yield[,ii], col= "red")
#   legend("topright", legend = c("Predicted catch", "Observed catch"), col = c("red", "black"), lty=1, cex=0.8)
#   plot(years, f_history[,ii], xlab = "Year", ylab = "fishing moratlity", type = "l", ylim = c(0, 1))
#   
# }


####################################
#something's off. Trying just cod with landings data from dfo to see if the catch data from NAFO was the issue

#just trying cod

#library(ggplot2)


#cod = as(read.csv("Desktop/cod.csv", row.names = 1), "data.frame")

#colnames(cod) <- c('Years', 'PREDCOD', 'OBSCOD')



#library(ggplot2)

#p = ggplot() + 
  #geom_line(data = cod, aes(x = Years, y = PREDCOD), color = "blue") +
  #geom_line(data = cod, aes(x = Years, y = OBSCOD), color = "red")+ ylab("catch (g)")
#p

##################################
#so the l;andings cod did look better, but we don't have landings data for all nine species.
#so I went back to my NAFO catch code and I think I found the error
#trying again

obs_catch = as(read.csv("obs_catch2.csv", row.names = 1), "matrix")
colnames(obs_catch) <- c('AMERICAN PLAICE', 'COD(ATLANTIC)','HADDOCK', 'HERRING(ATLANTIC)', 'REDFISH UNSEPARATED', 'SPINY DOGFISH', 'WITCH FLOUNDER', 'TURBOT,GREENLAND HALIBUT', 'YELLOWTAIL FLOUNDER')
obs_catch <- obs_catch[,1:9]

## check obs vs. predicted catch

pred_yield <-getYield(sim_IPSL_ssp5rcp85_histsoc)
pred_yield = pred_yield[101:148,]

# Calculate means in the right format
pred_yield_mean = apply(pred_yield, 2, mean)
pred_yield_mean = melt(pred_yield_mean)

obs_catch_mean = apply(obs_catch, 2, mean, na.rm=T)

catch_pred_obs = cbind(pred_yield_mean, obs_catch_mean)
catch_pred_obs$species <- row.names(catch_pred_obs)

catch_pred_obs <- as.data.frame(catch_pred_obs)
names(catch_pred_obs) = c( "pred", "obs", "species")

library(ggplot2)
p <- ggplot() + # plot predicted and observed yields
  geom_point(data = catch_pred_obs, 
             aes(x = log10(pred),  y = log10(obs), color = species)) +
  # plot optimal fit line
  geom_abline(color = "black", slope = 1, intercept = 0) + 
  xlab("log10 Predicted catch") + 
  ylab("log10 Observed catch") #+
#scale_fill_manual(values = wes_palette(12, "Zissou")) 
plot(p)

#timeseries plots
years = 1970:2017
for (ii in 1:9)
{
  dev.new()
  min_biomass = min(min(obs_catch[,ii], na.rm=T), min(min(pred_yield[,ii])))
  max_biomass = max(max(obs_catch[,ii], na.rm=T), max(max(pred_yield[,ii])))
  
  par(mfrow = c(2,1))
  plot(years, obs_catch[,ii], xlab = "Year", ylab = "catch", type = "l", ylim = c(min_biomass, max_biomass), main = catch_pred_obs$species[ii])
  lines(years, pred_yield[,ii], col= "red")
  legend("topright", legend = c("Predicted catch", "Observed catch"), col = c("red", "black"), lty=1, cex=0.8)
  plot(years, f_history[,ii], xlab = "Year", ylab = "fishing moratlity", type = "l", ylim = c(0, 1))
  
}

