# Extract NPP and SST for the region of interest
# Load the Scotian Shelf shapefile
library(rgdal)
library(raster)
setwd("c:/users/derekt/work/research/data/CMIP6/")
ess <- readOGR(dsn = "." , layer = "esspoly")

library(grid)
library(zoo)
library(viridis)

fliplr_matrix<- function(data_to_flip)
{
  temp= data_to_flip
  for (ii in 1:dim(data_to_flip)[2])
    data_to_flip[,ii] = temp[,dim(data_to_flip)[2] - ii + 1]
  
  data_to_flip
}


flipud_vector <- function(data_to_flip)
{
  temp= data_to_flip
  for (ii in 1:length(data_to_flip))
    data_to_flip[ii] = temp[length(data_to_flip) - ii + 1]
  
  data_to_flip
}
move_hemispheres<- function(data_to_flip)
{
  temp= data_to_flip
  data_to_flip[1:180,] = temp[181:360,]
  data_to_flip[181:360,] = temp[1:180,]
  data_to_flip
}

move_hemispheres_vector<- function(data_to_flip)
{
  temp= data_to_flip
  data_to_flip[1:180] = temp[181:360]
  data_to_flip[181:360] = temp[1:180]
  data_to_flip
}



source("c:/users/derekt/work/isabellefishery/extract_environmental_netcdf.R")
filename = "ipsl-cm6a-lr_1i1p1f1_historical_intppdiat_onedeg_global_monthly_1850_2014.nc"


directory = "c:/users/derekt/work/research/data/CMIP6/"
variable_to_extract ="tcb"


nc <- nc_open(paste(directory, filename, sep = ""))

