######
###### Helping functions for the BYM model
######

# Extract voronoi neighborhoods from sensors points
extract_voronoi_from_sensors <- function(sensors_sf,outer_buffer_distance=units::set_units(20,'m')){
  points <- sensors_sf$geometry
  envelope <- st_buffer(points,dist = outer_buffer_distance) |> st_union()
  voronoi <- st_voronoi(st_union(points)) |> st_collection_extract()
  voronoi <- st_intersection(voronoi,envelope)
  
  # ensure polygons are in same order as original point geometries
  voronoi_ordered <- voronoi[unlist(st_intersects(points,voronoi))] 
  
  return(voronoi_ordered)
}