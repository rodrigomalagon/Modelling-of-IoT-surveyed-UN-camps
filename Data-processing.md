Data processing
================
Rodrigo Malagón
2025-01-27

``` r
# Load packages
c('sf') |> sapply(require,character.only = TRUE)

# Load helping functions
source('Data selection.R')
source('Data processing.R')
```

## Data selection

Retrieve the queried data, locally queried from SQL database.

``` r
power_consumption <- read.csv(file = #path to queried consumption data from local SQL database
                                )
sensors <- read.csv(file = #path to queried consumption data from local SQL database
                      )
```

Format raw values.

``` r
# Process power consumption reading with weight value
power_consumption$value <- gsub(',','.',power_consumption$TAG_VALUE_RAW)|> as.numeric()
power_consumption$value <- power_consumption$value / as.numeric(power_consumption$TAG_WEIGHT)

# Format latituted and longitued fields from sensors dataframe
sensors$lat <- gsub(',','.',sensors$OGI_LAT) |> as.numeric()
sensors$long <- gsub(',','.',sensors$OGI_LONG)|> as.numeric()
```

Extract time leap per sensors to account for sensors with different gap
between their readings.

``` r
# Extract time leap in each sensor's time series
sensors$time_leap_mins <- lapply(sensors$MISSION_DEVICE_TAG,
  function(sensor_id){
  
  # Filter single time series
  sensor_ts <- power_consumption[power_consumption$MISSION_DEVICE_TAG == sensor_id,]
  
  # Retrieve leap of time series
  get_time_leap_mins(sensor_ts)
  }) |> unlist()
```

Extract degree of time completeness and filter sensors with only \> 80 %
completeness

``` r
# Set time extent parameters of the project
initial_timestamp <- power_consumption[1,'TAG_VALUE_TIMESTAMP']
final_timestamp <- power_consumption[nrow(power_consumption),'TAG_VALUE_TIMESTAMP']

# Extract degree of time series completeness
sensors$time_completeness <-  lapply(sensors$MISSION_DEVICE_TAG,
  function(sensor_id){
  
  # Filter single time series
  sensor_ts <- power_consumption[power_consumption$MISSION_DEVICE_TAG == sensor_id,]

  # Retrieve time completeness of sensor selected
  get_time_completeness(sensor_ts,initial_timestamp,final_timestamp)
  })|>unlist()

sensors <- sensors[sensors$time_completeness > 0.8,]
```

Filter out repeated locations

``` r
df <- sensors
df$long_lat <- paste0(df$long,'_',df$lat)
locations_summary <- table(df$long_lat)
locations_summary <- locations_summary[locations_summary>1]
repeated_locations <- names(locations_summary)
df <- df[!(df$long_lat %in% repeated_locations),]
sensors <- dplyr::select(df,-long_lat)
```

Filter out cluster locations

``` r
# Create sf object
sensors_sf <- create_sf_from_df(sensors)

# Set filter distance
distance_filter_threshold <- units::set_units(4,'m')

# Obtain distance matrix 
dist_matrix <- st_distance(sensors_sf,sensors_sf)

# Obtain nearest neighbour distance for each sensor
nn_distances <- lapply(1:nrow(sensors_sf), function(row_ind){
  row <- dist_matrix[row_ind,]
  
  # Select only neighbouring distances and retrieve minimum
  row[row > units::set_units(0,'m')] |> min()
  
}) |> unlist() |> units::set_units('m')

# Filter sensors with nearest neighbour further than given threshold
sensors <- sensors[nn_distances > distance_filter_threshold,]
```

Filter time series data according to selected sensors

``` r
power_consumption <- power_consumption[power_consumption$MISSION_DEVICE_TAG %in% sensors$MISSION_DEVICE_TAG,]
```

Save selected sensors and time series data

``` r
write.csv(sensors,file = 'data/sensors_selected.csv',row.names = FALSE)
write.csv(power_consumption,file = 'data/power_consumption_selected.csv',row.names = FALSE)
```

## Time series processing

Read selected data

``` r
sensors <- read.csv('data/sensors_selected.csv')
power_consumption <- read.csv('data/power_consumption_selected.csv')
```

Directories setting

``` r
processing_outputs_dir <- './data/processed_data/'
timestamp_processing_dir <- paste0(processing_outputs_dir,'abyei_pow_con_processed_timestamp/')
filter_and_diff_dir <- paste0(processing_outputs_dir,'abyei_pow_con_time_series_diffs/')
interpolated_dir <- paste0(processing_outputs_dir,'abyei_pow_con_interpolated_time_series_diffs/')

dir.create(processing_outputs_dir)
dir.create(timestamp_processing_dir)
dir.create(filter_and_diff_dir)
dir.create(interpolated_dir)
```

Timestamp correction

``` r
for(sensor_id in sensors$MISSION_DEVICE_TAG){
  
  # Get time series per sensor
  ts_df <- power_consumption[power_consumption$MISSION_DEVICE_TAG == sensor_id,]
  
  # Skip constant time series
  vals <- ts_df$value |> unique() 
  if(length(vals)==1){
    error_message <- paste0('WARNING: Sensor ',sensor_id,' has contant values! Time series skipped.')
    print(error_message)
    next
  }
  
  # Process timestamp
  t0 <- ts_df$TAG_VALUE_TIMESTAMP[1]
  tf <- ts_df$TAG_VALUE_TIMESTAMP[nrow(ts_df)]
  ts_df <- timestamp_correction(sensor_id,sensors,ts_df,t0,tf)
  
  # Save processed individual time series
  file_path <- paste0(timestamp_processing_dir,sensor_id,'.csv')
  write.csv(ts_df,file = file_path,row.names = FALSE)
  }
```

Filter correct data, obtain differences and mask out outliers

``` r
# Set input and ouput files
input_path <- timestamp_processing_dir
output_path <- filter_and_diff_dir

for(file in list.files(input_path,full.names = TRUE)){
  
  # Retrieve pre-processed data
  ts_df <- read.csv(file)
  
  # Filter error and obtain clean value differences
  ts_df <- ts_filter_correct(ts_df)
  ts_df <- ts_differences(ts_df)
  
  # Mask outlier differences
  ts_df <- ts_mask_outliers(ts_df)
  
  # Save processed time series
  sensor_id <- ts_df$MISSION_DEVICE_TAG[[1]]
  file_path <- paste0(output_path,sensor_id,'.csv')
  write.csv(ts_df,file = file_path,row.names = FALSE)
}
```

Interpolate with Kalman filter

``` r
input_path <- filter_and_diff_dir
output_path <- interpolated_dir

for(file in list.files(input_path,full.names = TRUE)){
    
  # Retrieve pre-processed data
  ts_df <- read.csv(file)

  # Clean time series and interpolate
  ts_df <- kalman_interpolate_ts(ts_df = ts_df)
  
  # Save processed time series
  sensor_id <- ts_df$MISSION_DEVICE_TAG[[1]]
  file_path <- paste0(output_path,sensor_id,'.csv')
  write.csv(ts_df,file = file_path,row.names = FALSE)
}
```

Stack interpolated time series

``` r
# Define empty data frame with desired structure
cols <- c("MISSION_DEVICE_TAG","reg_timestamp","value","value_filtered",'value_diff_raw',"value_diff","value_diff_interpolated")
stack_processed_power_consumption <- data.frame(matrix(ncol = length(cols),nrow = 0))
colnames(stack_processed_power_consumption) <- cols

# Stack all time series
counter <- 0
input_path <- interpolated_dir

for(file in list.files(input_path,full.names = TRUE)){
  ts_df <- read.csv(file)
  stack_processed_power_consumption <- rbind(stack_processed_power_consumption,ts_df)
  
  counter <- counter + 1
  # Print count check
  if(counter %% 10 == 0){
    print(paste0(counter,' time series stacked.'))
  }
}
```

Filter only processed sensors

``` r
sensors <- sensors[sensors$MISSION_DEVICE_TAG %in% stack_processed_power_consumption$MISSION_DEVICE_TAG,]
```

Save processed dataset

``` r
file_path <- paste0(processing_outputs_dir,'sensors_processed.csv')
write.csv(sensors,file = file_path,row.names = FALSE)

file_path <- paste0(processing_outputs_dir,'power_consumption_processed.csv')
write.csv(stack_processed_power_consumption,file = file_path,row.names = FALSE)
```
