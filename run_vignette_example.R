#vignette_dir = "C:\Users\derekt\Work\mizer\vignettes\"

species_params <-read.csv("C:/Users/derekt/Work/mizer/vignettes/NS_species_params.csv")
inter <-read.csv("C:/Users/derekt/Work/mizer/vignettes/inter.csv", row.names = 1)

species_params$a <- c(0.007, 0.001, 0.009, 0.002, 0.010, 0.006, 0.008, 0.004,
                      0.007, 0.005, 0.005, 0.007)
species_params$b <- c(3.014, 3.320, 2.941, 3.429, 2.986, 3.080, 3.019, 3.198,
                      3.101, 3.160, 3.173, 3.075)

gear_params <- 
  data.frame(species = species_params$species,
             gear = species_params$species,
             sel_func = "sigmoid_length",
             l25 =  c(7.6, 9.8, 8.7, 10.1, 11.5, 19.8, 16.4, 19.8, 11.5,
                      19.1, 13.2, 35.3),
             l50 = c(8.1, 11.8, 12.2, 20.8, 17.0, 29.0, 25.8, 29.0, 17.0,
                     24.3, 22.9, 43.6))


f_history <-as(read.csv("C:/Users/derekt/Work/mizer/vignettes/NS_f_history.csv", row.names = 1), "matrix")
gear_params$catchability <- as.numeric(f_history["1990",])

params <- newMultispeciesParams(species_params, 
                                interaction = inter, 
                                kappa = 9.27e10,
                                gear_params = gear_params)

relative_effort <- sweep(f_history,2,f_history["1990",],"/")
relative_effort[as.character(1988:1992),]

initial_effort <- matrix(relative_effort[1, ], byrow = TRUE, nrow = 100,
                         ncol = ncol(relative_effort), dimnames = list(1867:1966))
relative_effort <- rbind(initial_effort, relative_effort)


