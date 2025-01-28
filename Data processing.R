######
###### Data processing functions 
######

# Wrap function to correct timestamp
timestamp_correction <- function(sensor_id,sensors,ts_df,t0,tf,
                                 leap_residuals_threshold = 60*5 # threshold in seconds
                                 ){
  
  # Retrieve time leap for this sensors time series
  time_leap <- sensors[sensors$MISSION_DEVICE_TAG==sensor_id,"time_leap_mins"] * 60
  
  # Retrieve time_leap residuals of timestamp values 
  leap_residuals <- (ts_df$TAG_VALUE_TIMESTAMP - t0) %% (time_leap)
  
  # Apply comparison with threshold and regulize timestamp values
  if(Reduce('|',leap_residuals >= leap_residuals_threshold)){
    print('Timestamp distribution issue! Some timestamps values surpass the threshold distance to the multiples of the provided time leap.')
  }else{
    reg_timestamp <- t0 + (0:(round((tf-t0)/time_leap)))*time_leap
    new_ts <- data.frame(MISSION_DEVICE_TAG = ts_df$MISSION_DEVICE_TAG[[1]],
                         reg_timestamp = reg_timestamp,
                         value = NA)
    ts_df <- fill_new_ts(new_ts,ts_df,leap_residuals_threshold = leap_residuals_threshold)
  }
  
  # Print and return
  n <- ts_df$value |> is.na() |> sum()
  print(paste0('Number of NA-filled empty spaces of new regular time series: ',n))  
  return(ts_df)
}

# Look-up value of new time series from old time series
fill_new_ts <- function(new_ts,old_ts,
                        leap_residuals_threshold = 60*5 # threshold in seconds
                        ){
  
  for(row in 1:length(new_ts$reg_timestamp)){
    timestamp <- new_ts$reg_timestamp[row]
    
    # Look up for closest timestamp value in old ts and update value if found
    look_up_cond <- which(abs(old_ts$TAG_VALUE_TIMESTAMP - timestamp) < leap_residuals_threshold)
    if(length(look_up_cond) > 0){
      new_ts$value[row] <- old_ts$value[look_up_cond][[1]]
    }
  }
  return(new_ts)
}

# Filter correct values (positive, increasing)
ts_filter_correct <- function(ts_df){
  
  # Create new  column
  ts_df$value_filtered <-  ts_df$value
  
  for(row in 1:nrow(ts_df)){
    
    value <- ts_df$value[row]
    
    # Mark non-positive values as an error
    if(!is.na(ts_df$value[row])){
      if(ts_df$value[row] <= 0){
        value <- NA
      }
    }
    
    # Mark decreasing values as an error
    if(row >= 2){
      if(!is.na(ts_df$value[row]) & !is.na(ts_df$value[row-1])){
        if(ts_df$value[row] < ts_df$value[row - 1]){
          value <- NA
        }
      }
    }
    
    # Assigna new value
    ts_df$value_filtered[row] <- value
  }
  return(ts_df)
}


# FUNCTION: Compute time series differences
ts_differences <- function(ts_df){
  len <- nrow(ts_df)
  
  # Calculate difference
  previous <- ts_df$value_filtered[1:(len-1)]
  posterior <- ts_df$value_filtered[2:len]
  ts_df$value_diff_raw <- c(NA,posterior-previous)
  return(ts_df)
}


#Filter out huge positive values (coming from high positive gaps from raw monotonic data)
ts_mask_outliers <- function(ts_df, filtering_factor = 3, threshold_upper_bound = 10){
  ts <- ts_df$value_diff_raw[!(is.na(ts_df$value_diff_raw))]
  m <- median(ts)
  p99 <- quantile(ts,0.99) - m
  
  # Define threshold criteria for outlier detection
  threshold <- min(m + filtering_factor * p99, threshold_upper_bound)
  if(threshold == 0){# border case
    threshold <- threshold_upper_bound
  }
  
  # Apply threshold check
  ts_df$value_diff <- ifelse(ts_df$value_diff_raw > threshold,NA,ts_df$value_diff_raw)
  
  return(ts_df)
}

# Interpolate missing data in time series using Kalman filter
kalman_interpolate_ts <- function(ts_df,max = 50){
  
  # Interpolate using Kalman filter
  ts_df$value_diff_interpolated <- imputeTS::na_kalman(ts_df$value_diff)
  
  # Insert mean values of non-NA original values if maximum threshold exceeded
  m <- mean(ts_df$value_diff[!(is.na(ts_df$value_diff))])
  ts_df$value_diff_interpolated <- ifelse(ts_df$value_diff_interpolated > max,m,ts_df$value_diff_interpolated)
  
  return(ts_df)
}
