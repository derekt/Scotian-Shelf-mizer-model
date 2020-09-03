# Calculate the sum of squared errors


calculate_sse <- function(data_to_compare_to, model_projection)
{
  total_sse = 0
  
  for (ii in 1:length(model_projection))
  {
    row_obs = which(data_to_compare_to[,1] == names(model_projection)[ii])
    sum_sq_temp = (model_projection[ii] - data_to_compare_to[row_obs,2])^2
    # print(sum_sq_temp)
    total_sse = total_sse + as.numeric(sum_sq_temp)
    # print(total_sse)
  }
  
  return(total_sse)
}