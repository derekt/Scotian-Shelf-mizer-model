# Run the mizer model to optimize parameters

# Function to run the model with specific rMax input parameters for optimization
runModel <- function(rMax,Mean_survey_sub)
{
  # Put new vector back into species parameters
  params@species_params$R_max = rMax
  
  # Run the model
  sim <- project(params, effort = 1, t_max = time_torun_simulation) #Change t_max to determine how many years the model runs for
  
  # Extract final biomasses
  biomasses_through_time = getBiomass(sim)
  final_biomasses = biomasses_through_time[time_torun_simulation,]
  
  # Calculate SSE
  sse_final <- calculate_sse(Mean_survey_sub, final_biomasses)
  return(sse_final)
}
