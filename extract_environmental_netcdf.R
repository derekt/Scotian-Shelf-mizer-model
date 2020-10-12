library(ncdf4)
library(zoo)

averageEnvironmentalCDF <-
  function(directory,
           filename,
           variable,
           time1,
           time2,
           average_whole_period = FALSE,
           convert_to_kg_km = TRUE, hist = TRUE, ipsl = FALSE)
  {
    
    model_type <- get_model_type(filename)
    
    # Open the netcdf file
    nc <- nc_open(paste(directory, filename, sep = "\\"))
    
    # Look at the attributes and dimensions
    # print(nc)
    
    # Extract important information
    data_attributes <- ncatt_get(nc, variable)
    
    main_title <- data_attributes$long_field_name
    data_units <- data_attributes$units
    
    if (!ipsl)
    {
      lon <- ncvar_get(nc, "LONGITUDE")
      lat <- ncvar_get(nc, "LATITUDE")
      
      time_vector <-  ncvar_get(nc, "TIME")
    } else
    {
      lon <- ncvar_get(nc, "longitude")
      lat <- ncvar_get(nc, "latitude")
      
      time_vector <-  ncvar_get(nc, "time")      
    }
    
    
    if (hist)
    {
      yearmonth = as.yearmon("1950-01") + time_vector[1] 
      print(yearmonth)    
      time_months = floor(as.numeric(as.yearmon(yearmonth + seq(0, (length(time_vector) - 1)))))
      time_vector <- time_months
    } else
    {
      yearmonth = as.yearmon("2006-01") + time_vector[1] 
      print(yearmonth)    
      time_months = floor(as.numeric(as.yearmon(yearmonth + seq(0, (length(time_vector) - 1)))))
      time_vector <- time_months      
    }
    
    # Extract averages
    if (!average_whole_period)
    {
      # Extract the correct time period
      if ((class(time1) == "yearmon") && (class(time2) == "yearmon"))
      {
        # Check to make sure starts in January and ends in Dec. Doesn't do stupidity checks (e.g. end date before start date)
        if ((as.numeric(time1) == as.numeric(floor(time1))) && ((as.numeric(time2) - floor(as.numeric(time2))) > 0.9))
        { ; } else
        {
          print("When taking yearly averages of monthly data, dates given should start in Jan and end in Dec")
        }
      }
      
      # Get the correct time period to extract
      tpte = time_period_to_extract(time1, time2, time_vector)
      
      # Calculate number of years
      num_years = (floor(as.numeric(time2)) - floor(as.numeric(time1)) + 1)
      
      # Create a new matrix to hold the results
      new_var_mat = array(rep(0, num_years * length(lon) * length(lat)),
                          c(length(lon), length(lat), num_years))
      
      # Now get the variable of interest
      if (names(nc$dim)[length(nc$dim)] != "TIME")
        print("Warning: might be assuming wrong dimension for time from NetCDF file")
      ta1 <- 
        ncvar_get(nc, variable)[, , ]
      temp_array1 <-
        ncvar_get(nc, variable)[, , tpte$start_point_to_extract:tpte$end_point_to_extract]
      
      # Average data
      for (ii in 1:length(lon))
      {
        for (jj in 1:length(lat))
        {
          # Extract values per grid cell
          temp_vec = temp_array1[ii, jj, ]
          
          if ((class(time1) == "yearmon") && (class(time2) == "yearmon"))
          {
            # Extract yearly averages
            year_values = vector(length = num_years)
            for (hh in 1:num_years)
            {
              year_values[hh] = mean(temp_vec[((hh - 1) * 12 + 1):(hh * 12)])
            }
          } else
          {
            year_values = temp_vec
          }
          
          # Fill in 3D matrix of annual averages
          new_var_mat[ii, jj, ] = year_values
        }
      }
      
      years = floor(as.numeric(time1)):floor(as.numeric(time2))
      
      
    } else 
    {
      # Create a new matrix to hold the results
      new_var_mat = array(rep(0, length(lon) * length(lat)), c(length(lon), length(lat)))
      
      # Get the correct time period to extract
      tpte = time_period_to_extract(time1, time2, time_vector)
      
      # Now get the variable of interest
      if (names(nc$dim)[length(nc$dim)] != "time")
        print("Warning: might be assuming wrong dimension for time from NetCDF file")
      temp_array1 <-
        ncvar_get(nc, variable)[, , tpte$start_point_to_extract:tpte$end_point_to_extract]
      
      # Extract averages
      for (ii in 1:length(lon))
      {
        for (jj in 1:length(lat))
        {
          # Fill in 2D matrix of whole time-period averages
          new_var_mat[ii, jj] = mean(temp_array1[ii, jj, ])
        }
      }
      
      years = paste(as.character(time1), "to", as.character(time2), sep=" ")
    }
    
    if(can_model_units_be_standardized(data_units, convert_to_kg_km))
    {
      new_var_mat = new_var_mat * 1000
      data_units = "kg C km-2"
    }
    
    return_list <-
      list(
        main_title = main_title,
        data_units = data_units,
        lon = lon,
        lat = lat,
        years = years,
        fishvar = new_var_mat
      )
  }