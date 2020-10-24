# Extract NPP and SST for the region of interest
# Load the Scotian Shelf shapefile
library(rgdal)
library(raster)
library(grid)
library(zoo)
library(viridis)

setwd("c:/users/derekt/work/research/data/CMIP6/")
source("c:/users/derekt/work/research/scripts/fishmip-analyses/helperscripts.r")
ess <- readOGR(dsn = "." , layer = "esspoly")

# Read ESS coordinates
ess_coords = read.csv("c:/users/derekt/work/isabellefishery/ess_coords.csv", header = T)
names(ess_coords) = c("Lat", "Lon")

years = 1970:2100


extract_hist_future <- function(variable_name, average_vals = F, convert_mols_secs = F, convert_mols = F)
{
  directory = "c:/users/derekt/work/research/data/CMIP6/"
  filename1 = paste("ipsl-cm6a-lr_r1i1p1f1_historical_", variable_name, "_onedeg_global_monthly_1850_2014.nc", sep="")
  filename2 = paste("ipsl-cm6a-lr_r1i1p1f1_ssp585_", variable_name, "_onedeg_global_monthly_2015_2100.nc", sep="")
  variable_to_extract = variable_name

  # Extract historical data
  nc <- nc_open(paste(directory, filename1, sep = ""))
  print(ncatt_get( nc, variable_to_extract, attname=NA, verbose=FALSE )$units)
  missing_val <- ncatt_get(nc, variable_to_extract, attname=NA, verbose=FALSE)$missing_value
  lat <- ncvar_get(nc, "lat")
  lon <- ncvar_get(nc, "lon")

  time_vector <-  ncvar_get(nc, "time")   
  yearmonth = as.yearmon("1601-01") + time_vector[1] / 12
  time_months <- as.yearmon("1601-01") + as.yearmon(time_vector / 12)
  time_vector <- time_months
  time1 = as.yearmon("1970-01")
  time2 = as.yearmon("2014-12")

  # Work out which months to extract
  tpte = time_period_to_extract(time1, time2, time_vector)
  temp_extraction = matrix(data = NA, nrow = dim(ess_coords)[1], ncol = (tpte$end_point_to_extract - tpte$start_point_to_extract) + 1)

  # Open the variable for extraction
  ta1 <- ncvar_get(nc, variable_to_extract)[, ,tpte$start_point_to_extract:tpte$end_point_to_extract]

  # Extract the needed data
  for (kk in 1:dim(ta1)[3])
  {
    for (ii in 1:dim(ess_coords)[1])
    {
      lat_loc = which(lat == ess_coords[ii,1])
      lon_loc = which(lon == ess_coords[ii,2] - 0.5)
      temp_extraction[ii,kk] = ta1[lon_loc,lat_loc,kk] 
    }  
  }
  
  num_years = (floor(as.numeric(time2)) - floor(as.numeric(time1)) + 1)
  
  if (average_vals)
  {
    # Return the average per year
    average_per_year = vector(length = num_years)

    for (ii in 1:num_years)
    {
      average_per_year[ii] = mean(temp_extraction[ , ((ii-1) * 12 + 1) : (ii * 12)], na.rm = T)
    }
  }
  else
  {
    # Otherwise sum the values, take the average, and assume NAs are the average of the observed values in each year
    sum_per_year = vector(length = num_years)
    for (ii in 1:num_years)
    {
      temp_vals = temp_extraction[ , ((ii-1) * 12 + 1) : (ii * 12)]
      
      if (sum(is.na(temp_vals)) > 0)
        temp_vals[is.na(temp_vals)] = mean(temp_vals, na.rm = T)
      
      sum_per_year[ii] = temp_vals
    }
  }


  # Extract future data
  nc <- nc_open(paste(directory, filename2, sep = ""))
  lat <- ncvar_get(nc, "lat")
  lon <- ncvar_get(nc, "lon")

  time_vector <-  ncvar_get(nc, "time")   
  yearmonth = as.yearmon("1601-01") + time_vector[1] / 12
  time_months <- as.yearmon("1601-01") + as.yearmon(time_vector / 12)
  time_vector <- time_months
  time1 = as.yearmon("2015-01")
  time2 = as.yearmon("2100-12")

  # Work out which months to extract
  tpte = time_period_to_extract(time1, time2, time_vector)
  temp_extraction = matrix(data = NA, nrow = dim(ess_coords)[1], ncol = (tpte$end_point_to_extract - tpte$start_point_to_extract) + 1)

  # Open the variable for extraction
  ta1 <- ncvar_get(nc, variable_to_extract)[, ,tpte$start_point_to_extract:tpte$end_point_to_extract]

  # Extract the needed data
  for (kk in 1:dim(ta1)[3])
  {
    for (ii in 1:dim(ess_coords)[1])
    {
      lat_loc = which(lat == ess_coords[ii,1])
      lon_loc = which(lon == ess_coords[ii,2] - 0.5)
      temp_extraction[ii,kk] = ta1[lon_loc,lat_loc,kk] 
    }  
  }
  
  num_years_future = (floor(as.numeric(time2)) - floor(as.numeric(time1)) + 1)

  # if (average_vals)
  # {
    # Return the average per year
    average_per_year_future = vector(length = num_years_future)
    
    for (ii in 1:num_years_future)
    {
      average_per_year_future[ii] = mean(temp_extraction[ , ((ii-1) * 12 + 1) : (ii * 12)], na.rm = T)
    }
    
    average_per_year = c(average_per_year, average_per_year_future)
  # }
  # else
  # {
  #   # Otherwise sum the values, take the average, and assume NAs are the average of the observed values in each year
  #   sum_per_year_future = vector(length = num_years_future)
  #   
  #   for (ii in 1:num_years_future)
  #   {
  #     temp_vals = temp_extraction[ , ((ii-1) * 12 + 1) : (ii * 12)]
  #     
  #     if (sum(is.na(temp_vals)) > 0)
  #       temp_vals[is.na(temp_vals)] = mean(temp_vals, na.rm = T)
  #     
  #     sum_per_year_future[ii] = sum(temp_vals)
  #   }
  #   sum_per_year = c(sum_per_year, sum_per_year_future)
  # }
  
  if (convert_mols_secs)
  {
    # Units are mol m-2 s-1

    # Turn into mean grams C per m^2 per second
    average_per_year = average_per_year * 12.011

    # Turn from C into wet weight
    average_per_year = average_per_year * 10

    # Turn from wet weight per s^-1 into per year
    average_per_year = average_per_year * 60 * 60 * 24 * 365

    # Turn into grams wet weight for the area per year
    average_per_year = average_per_year * 1000000 * 108000
  }
  
  if (convert_mols)
  {
    # Units are mol m-2
    
    # Turn into mean grams C per m^2
    average_per_year = average_per_year * 12.011
    
    # Turn from C into wet weight
    average_per_year = average_per_year * 10
    
    # Turn into grams wet weight for the area per year
    average_per_year = average_per_year * 1000000 * 108000
  }
  
    dev.new()
   # if (average_vals)
      plot(years, average_per_year, type = "l", main = variable_name)
  #  else
  #    plot(years, sum_per_year, type = "l", main = variable_name)
      
  return(average_per_year)
}


  # Extract the data that we need/
  intpp <- extract_hist_future("intpp", TRUE, TRUE, FALSE)
  intppdiat <- extract_hist_future("intppdiat", TRUE, TRUE, FALSE)
  phyc <- extract_hist_future("phyc-vint", TRUE, FALSE, TRUE)
  phydiat <- extract_hist_future("phydiat-vint", TRUE, FALSE, TRUE)
  tob <- extract_hist_future("tob", TRUE, FALSE, FALSE)
  tos <- extract_hist_future("tos", TRUE, FALSE, FALSE)
  zmeso <- extract_hist_future("zmeso-vint", TRUE, FALSE, TRUE)
  zmicro <- extract_hist_future("zmicro-vint", TRUE, FALSE, TRUE)
  zooc <- extract_hist_future("zooc-vint", TRUE, FALSE, TRUE)
  

  # Save the data
  setwd("c:/users/derekt/work/isabellefishery/")
  phytoplankton_conc = data.frame(year = years, phyconc = phyc)
  zooplankton_conc = data.frame(year = years, zooconc = zooc)
  plankton_conc = data.frame(year = years, plankconc = phyc + zooc)
  bottom_temp = data.frame(year = years, bottomtemp = tob)
  surface_temp = data.frame(year = years, surfacetemp = tos)
  
  write.csv(phytoplankton_conc, "phytoplankton_conc.csv")
  write.csv(zooplankton_conc, "zooplankton_conc.csv")
  write.csv(plankton_conc, "plankton_conc.csv")
  write.csv(bottom_temp, "bottom_temp.csv")
  write.csv(surface_temp, "surface_temp.csv")
  
  plot(years, plankton_conc[,2], type = "l", ylim = c(1e11, 3e12))
  lines(years, zooplankton_conc[,2], type = "l", col = "blue")
  lines(years, phytoplankton_conc[,2], type = "l", col = "green")
  legend("topright", legend = c("All plankton", "Zooplankton", "Phytoplankton"), lty = 1, col = c("black", "blue", "green"), pch = NA)
