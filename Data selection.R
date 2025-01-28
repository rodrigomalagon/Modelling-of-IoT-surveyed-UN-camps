######
###### Data selection functions
######

## Retrieve the mode of a vector
get_mode <- function(vector) {
  freq_table <- table(vector)         # Get the frequency table
  mode_value <- names(freq_table)[which.max(freq_table)]  # Find the most frequent value
  return(mode_value)
}

## Retrieve time leap of a sensor time series in minutes
get_time_leap_mins <- function(sensor_ts){
  leaps <- (sensor_ts[2:nrow(sensor_ts),'TAG_VALUE_TIMESTAMP']-sensor_ts[1:(nrow(sensor_ts)-1),'TAG_VALUE_TIMESTAMP'])/60
  leaps <- round(leaps)
  leap_mode <- get_mode(leaps)|> as.integer()
  return(leap_mode)
}

## Retrieve completeness of number of readings of a time series
get_time_completeness <- function(sensor_ts,initial_timestamp,final_timestamp){
  
  # Extract leap from time series
  leap <- get_time_leap_mins(sensor_ts)
  
  # Calculation of sensor leap status
  desired_num_readings <- 
    ((final_timestamp - initial_timestamp)/(60*leap)) |> round()
  status <- dim(sensor_ts)[1]/desired_num_readings
  #message <-   paste0('Bold percentage of expected time completeness of sensor: ',status)
  #print(message)
  return(status)
}


###
### Spatial functions
###

## Get centroid from sf object (collection of points)
get_centroid <- function(sf_points){
  centroid <- sf_points|>
    st_union()|>
    st_centroid()|>
    st_coordinates()
  return(centroid)
}

## Transform sf object (collection of points) to aeqd projection using PROJ method
transform_to_centered_aeqd <- function(sf_points){
  
  # Retrieve centroid
  centroid <- get_centroid(sf_points)
  
  # Build PROJ string
  proj_string <- sprintf("+proj=aeqd +lat_0=%f +lon_0=%f +datum=WGS84 +units=m",centroid[2],centroid[1])
  
  # Execute transformation
  sf_points <- st_transform(sf_points, crs = proj_string)
  
  return(sf_points)
}

## Create sf object from sensors dataset
create_sf_from_df <- function(sensors_df,long_lat_names = c('long','lat'),crs='EPSG:4326'){
  sensors_sf <- st_as_sf(sensors_df,coords = long_lat_names, crs = crs) |> transform_to_centered_aeqd()
  return(sensors_sf)
}
