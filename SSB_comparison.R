#Sept 29th
#Goal of this script is to compare modelled SSB to observed SSB from Ram database

#loading results from model run that Derek put on github
load("/Users/isabellehurley/Downloads/model_output.Rdata")

model_output = load("Downloads/model_output.Rdata")

sim =model_output
plot(sim)
params=sim@params
params@species_params
#Trying to follow Julia's code to make the plot to compare modelled SSB to observed SSB from Ram database
# Import time-averaged the RAM legacy SSB 
#Callesd "SSB.g_comparison.csv" on github

temp_file = read.csv("Desktop/SSB.g_comparison.csv")

mean_SSB = aggregate(SSB.g~Species, temp_file, mean)
names(mean_SSB) <- c("species","SSB.g")

# merge these with species_params to ensure matched rows
species_params<-merge(species_params,mean_SSB,by="species")


## check obs vs. predicted SSB
pred_SSB <-melt(getSSB(sim)[100,])
pred_SSB$obs <- params@species_params$SSB.g
pred_SSB$species <-row.names(pred_SSB)
p <- ggplot() + # plot predicted and observed yields
  geom_point(data = pred_SSB, 
             aes(x = log10(value), y = log10(obs), color = species)) +
  # plot optimal fit line
  geom_abline(color = "black", slope = 1, intercept = 0) + 
  xlab("log10 Predicted SSB") + 
  ylab("log10 Observed SSB") #+ 
# scale_fill_manual(values = wes_palette(12, "Zissou")) 
p
