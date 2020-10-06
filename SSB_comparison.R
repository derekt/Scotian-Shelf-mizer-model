#Sept 29th
#Goal of this script is to compare modelled SSB to observed SSB from Ram database

#loading results from model run that Derek put on github
#load("/Users/isabellehurley/Downloads/model_output.Rdata")
setwd("c:/users/derekt/work/isabellefishery/")
load("model_output.Rdata")
library(wesanderson)

model_output = load("model_output.Rdata")

#sim=model_output
plot(sim)
params=sim@params
params@species_params

#Trying to follow Julia's code to make the plot to compare modelled SSB to observed SSB from Ram database
# Import time-averaged the RAM legacy SSB 

obs_SSB = as(read.csv("c:/users/derekt/work/isabellefishery/SSB_total_grams.csv", row.names = 1), "matrix")
colnames(obs_SSB) <- c('AMERICAN PLAICE', 'COD(ATLANTIC)','HADDOCK', 'HERRING(ATLANTIC)', 'REDFISH UNSEPARATED', 'SPINY DOGFISH', 'WITCH FLOUNDER', 'TURBOT,GREENLAND HALIBUT', 'YELLOWTAIL FLOUNDER')
obs_SSB <- obs_SSB[,1:9]

## check obs vs. predicted SSB
pred_SSB <-getSSB(sim)
pred_SSB = pred_SSB[101:148,]


# Calculate means in the right format
pred_SSB_mean = apply(pred_SSB, 2, mean)
pred_SSB_mean = melt(pred_SSB_mean)

obs_SSB_mean = apply(obs_SSB, 2, mean, na.rm=T)

ssb_pred_obs = cbind(pred_SSB_mean, obs_SSB_mean)
ssb_pred_obs$species <- row.names(ssb_pred_obs)

ssb_pred_obs <- as.data.frame(ssb_pred_obs)
names(ssb_pred_obs) = c( "pred", "obs", "species")

p <- ggplot() + # plot predicted and observed yields
  geom_point(data = ssb_pred_obs, 
             aes(x = log10(pred),  y = log10(obs), color = species)) +
  # plot optimal fit line
  geom_abline(color = "black", slope = 1, intercept = 0) + 
  xlab("log10 Predicted SSB") + 
  ylab("log10 Observed SSB") #+
  #scale_fill_manual(values = wes_palette(12, "Zissou")) 
plot(p)

years = 1970:2017
for (ii in 1:9)
{
  dev.new()
  min_biomass = min(min(obs_SSB[,ii], na.rm=T), min(min(pred_SSB[,ii])))
  max_biomass = max(max(obs_SSB[,ii], na.rm=T), max(max(pred_SSB[,ii])))
                    
  par(mfrow = c(2,1))
  plot(years, obs_SSB[,ii], xlab = "Year", ylab = "Observed SSB", type = "l", ylim = c(min_biomass, max_biomass), main = ssb_pred_obs$species[ii])
  plot(years, pred_SSB[,ii], xlab = "Year", ylab = "Predicted SSB", type = "l", ylim = c(min_biomass, max_biomass))
}
