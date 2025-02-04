######
###### Helping spatial functions
######

# Get centroid from sf object (collection of points)
get_centroid <- function(sf_points){
  centroid <- sf_points|>
    st_union()|>
    st_centroid()|>
    st_coordinates()
  return(centroid)
}

# transform sf object (collection of points) to aeqd projection using PROJ method
transform_to_centered_aeqd <- function(sf_points){
  
  # Retrieve centroid
  centroid <- get_centroid(sf_points)
  
  # Build PROJ string
  proj_string <- sprintf("+proj=aeqd +lat_0=%f +lon_0=%f +datum=WGS84 +units=m",centroid[2],centroid[1])
  
  # Execute transformation
  sf_points <- st_transform(sf_points, crs = proj_string)
  
  return(sf_points)
}

# Create sf object from sensors dataset
create_sf_from_df <- function(df,long_lat_names = c('long','lat'),crs='EPSG:4326'){
  sensors_sf <- st_as_sf(df,coords = long_lat_names, crs = crs) |> transform_to_centered_aeqd()
  return(sensors_sf)
}


# Retrieve ids of neighbouring sensors
get_neighbours_ids <- function(sensor_id,sensors,buffer_search = units::set_units(10,'m')){
  nieghbours_sf <- st_as_sf(sensors,
                            coords = c('long','lat'),
                            crs='EPSG:4326') |>
    transform_to_centered_aeqd()
  
  # Filter out the "center" sensor
  sensor <- nieghbours_sf[nieghbours_sf$MISSION_DEVICE_TAG == sensor_id,]
  nieghbours_sf <- nieghbours_sf[nieghbours_sf$MISSION_DEVICE_TAG != sensor_id,]
  
  # Retrieve mininum distance to other sensors
  distances <- st_distance(sensor,nieghbours_sf)
  min_dist <- min(distances)
  
  if(min_dist > buffer_search){
    # Return at least closest neighbour
    return(nieghbours_sf$MISSION_DEVICE_TAG[which(distances == min_dist)])
  }else{
    # Return all neighbours within buffer search distance
    return(nieghbours_sf$MISSION_DEVICE_TAG[which(distances <= buffer_search)])
  }
  
}

