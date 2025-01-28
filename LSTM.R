######
###### Helping functions for the LSTM model
######


###
### --- Data preparation functions
###

# Homologate dates between sensors: "cut" time series at a common lower and upper temporal bound
homologate_dates <- function(timeseries_dataset,
                             time_var = 'reg_timestamp',
                             sensor_id_col = 'MISSION_DEVICE_TAG'){
  
  # Create formula and aggregate dataset by sensor id to extract first and last date per sensor
  frml <- create_formula(y_var = time_var,x_vars = sensor_id_col) 
  first_times <- aggregate(frml,data = timeseries_dataset,FUN = function(x){
    x[[1]]
  })
  last_times <- aggregate(frml,data = timeseries_dataset,FUN = function(x){
    x[[length(x)]]
  })
  
  # Retrieve lower and upper bound for the dataset cut
  lower_hour_bound <- first_times[[time_var]] |> as.POSIXct() |> max() |> lubridate::ceiling_date(unit = 'hour')
  upper_hour_bound <-last_times[[time_var]] |> min() |> as.POSIXct() |> lubridate::floor_date(unit = 'hour')
  
  # Cut data frame
  timeseries_dataset <- timeseries_dataset[(timeseries_dataset[[time_var]] >= lower_hour_bound) & (timeseries_dataset[[time_var]] <= upper_hour_bound),]
  
  return(timeseries_dataset) 
}

# Create additive formula from list of independent variables
create_formula <- function(y_var = 'y',x_vars){
  
  frml <- as.character(x_vars[[1]])
  if(length(x_vars) > 1){
    for(var in 2:length(x_vars)){
      frml <- paste(frml,x_vars[[var]],sep = ' + ')
    }
  }
  frml <- paste(y_var,'~',frml)
  #cat(paste0('\nFormula:\n',frml,'\n')) # print formula to inform on level of aggregation
  frml <- formula(frml)  
  return(frml)
}


###
### --- Spatial data preparation functions
###

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

# Retrieve mean value of target variable in a neighbourhood
get_mean_neighbourhood <- function(neighbours_ids,time_series_dataset,
                                   time_vars_combination,
                                   sensor_id_col = 'MISSION_DEVICE_TAG'){
  # Select data of the neighborhood
  ts_df <- time_series_dataset[time_series_dataset[[sensor_id_col]] %in% neighbours_ids,]
  ts_df <- ts_df[c('reg_timestamp','value_diff_interpolated',sensor_id_col)]
  colnames(ts_df)[1:2] <- c('timestamp','y')
  
  # Extract main time features
  ts_df$date <- ts_df$timestamp |> as.POSIXct() |>lubridate::date()
  ts_df$hour <- ts_df$timestamp |> as.POSIXct() |>lubridate::hour()
  
  # Aggregate temporally each sensor (at given time combinations) by SUM
  frml <- create_formula(x_vars = c(time_vars_combination,sensor_id_col))
  ts_df <- aggregate(frml,data = ts_df, FUN = sum)
  
  # Aggregate spatially (all the neighbourhood) by MEAN
  frml <- create_formula(x_vars = time_vars_combination)
  ts_df <- aggregate(frml,data = ts_df, FUN = mean)
  
  # Rename value column
  colnames(ts_df)[which(colnames(ts_df)=='y')] <- 'neigh_value'
  
  return(ts_df)
}


###
### --- Temporal data preparation functions
###

# Extract cosine and sine components of cyclic numeric data
transform_to_circ_topology <- function(data_df,col_names){
  
  for(col_name in col_names){
    norm_col <- min_max_norm(data_df[[col_name]]) # ensure normalization
    col_name_x <- paste0(col_name,'_x')
    col_name_y <- paste0(col_name,'_y')
    data_df[[col_name_x]] <- cos(2* pi * norm_col)
    data_df[[col_name_y]] <- sin(2* pi * norm_col)      
    
    # Normalize x and y components
    data_df[[col_name_x]] <- min_max_norm(data_df[[col_name_x]])
    data_df[[col_name_y]] <- min_max_norm(data_df[[col_name_y]])
    
    # Delete original column
    data_df <- data_df[,!(colnames(data_df) == col_name)]
  }
  return(data_df)
}

# Transform series with min-max normalization
min_max_norm <- function(series){
  m <- min(series)
  d <- max(series)-m
  return((series-m)/d)
}

# Process all variables in preparation to tensor desired formats and ranges
process_cols <- function(data_df,target_var = 'y',numeric_vars,cat_vars){
  data_df_transformed <- data_df
  
  # Target variable
  data_df_transformed[[target_var]] <- z_norm(data_df_transformed[[target_var]])
  
  # Numeric variables
  for(col in numeric_vars){
    data_df_transformed[[col]] <- min_max_norm(data_df_transformed[[col]])
  }
  
  # Categorical variables
  for(col in cat_vars){
    data_df_transformed <- add_discrete_vars_from_category(data_df_transformed,col)
    
    # delete former column
    cols <- !(colnames(data_df_transformed) %in% col)
    data_df_transformed <- data_df_transformed[,cols]
  }
  
  return(data_df_transformed)
}

# Transform series with Z_score normalization
z_norm <- function(series){
  m <- mean(series)
  s <- sd(series)
  r <- (series-m)/s
  return(r)
}

# Create boolean columns per category in a categorical column of a dataframe
add_discrete_vars_from_category <- function(data_df,cat_col){
  for(category in unique(data_df[[cat_col]])){
    if(is.character(category)){
      
      # Define a boolean column per category
      data_df[category] <- as.numeric(data_df[[cat_col]] == category)
    }else{print('Category with non-character type. Process aborted.')}
  }
  return(data_df)
}

# Shift features (at time t) whose values are known beforehand to match "past" observations 
# (at time t-1) in preparation to set independent variables at time t to infere value of 
# dependent variable 'y' at time t+1.
shift_known_futere_features <- function(data_df,known_features){
  for(col in known_features){
    new_col <- c(data_df[(2:nrow(data_df)),col],NA)
    data_df[[col]] <- new_col
  }
  return(data_df)
}

# Set data matrix and reorder columns
create_data_matrix <- function(data,del_time_cols){
  
  # delete non modelled columns
  data <- data[,!(colnames(data) %in% del_time_cols)]
  
  # ensure 'y' variable at first column and  transform to numeric matrix
  data <- data[c('y',colnames(data)['y' != colnames(data)])] 
  data <- data.matrix(data) |> unname() 
  return(data)  
}

# Create LSTM-ready tensor given a data matrix with target time series in first column
create_lstm_tensor <- function(data, len_series, num_vars, lag, pred_window){
  
  # Apply lag to dataset to obtain array
  
  X <- array(NA, dim = c(len_series - (lag + pred_window) + 1, lag, num_vars))
  len <- dim(X)[1]
  
  y <- array(NA, dim = c(len, pred_window))
  
  
  #Fill tensor
  t_series <- data[,1]
  
  for(i in 1:len){
    lag_selection_1 <- i:(i + (lag-1))
    X[i,,1] <- t_series[lag_selection_1]
    if(num_vars > 1){
      lag_selection_2 <- lag_selection_1
      X[i,,2:num_vars] <- data[lag_selection_2,2:num_vars]
    }
    y_selection <- (i+lag):(i + lag + pred_window -1)
    y[i,]<- t_series[y_selection]
  }
  
  r <- list(x_array = X,y_array = y)
  
  return(r)
}

# Split tensor data into training and validation sets
split_data <- function(X,y,train_val_ratio = NULL,train_test_ratio){
  
  # Get data size parameters
  len <- dim(X)[1]
  
  if(is.null(train_val_ratio)){
    train_size <- round(len * train_test_ratio)
    
    X_train <- X[1:train_size,,]
    y_train <- y[1:train_size,]
    
    X_test <- X[(train_size + 1):len,,]
    y_test <- y[(train_size + 1):len,]
    
    r <- list(
      x_train = X_train,
      y_train = y_train,
      x_test = X_test,
      y_test = y_test)
    
  }else{
    train_size <- round(len * train_test_ratio * train_val_ratio)
    val_size <- round(len * train_test_ratio) - train_size
    
    X_train <- X[1:train_size,,]
    y_train <- y[1:train_size,]
    
    X_validation <- X[(train_size + 1):(train_size + val_size),,]
    y_validation <- y[(train_size + 1):(train_size + val_size),]    
    
    X_test <- X[(train_size + val_size + 1):len,,]
    y_test <- y[(train_size + val_size + 1):len,]
    
    r <- list(
      x_train = X_train,
      y_train = y_train,
      x_validation = X_validation,
      y_validation = y_validation,
      x_test = X_test,
      y_test = y_test)
    
  }
  
  return(r)
}


###
### --- LSTM model
###

# Setup LSTM architecture 
create_lstm_model <- function(lag,num_vars,pred_window){
  model <- NULL
  model <- keras_model_sequential() |>
    layer_lstm(units = 64,input_shape = c(lag,num_vars),
               activation = 'relu',
               recurrent_activation='sigmoid',
               #bias_regularizer = regularizer_l2(0.0001), # add L2 regularization to bias terms
               return_sequences = TRUE) |>
    layer_lstm(units = 32, 
               activation = 'relu',
               recurrent_activation='sigmoid',
               return_sequences = TRUE) |>
    layer_lstm(units = 16, 
               activation = 'relu',
               recurrent_activation='sigmoid',
               return_sequences = FALSE) |>
    #layer_lstm(units = 8, 
    #           activation = 'relu',
    #           recurrent_activation='sigmoid',
    #           return_sequences = FALSE) |>
    layer_dense(units = pred_window)
  
  summary(model)
  
  return(model)
}

# Bump-up definition to retrieve original data ranges (z-score normalization)
bump_up_z_score <- function(series,ref_series){
  m <- mean(ref_series)
  s <- sd(ref_series)
  return(series*s + m)
}

# Retrieve time structure for whole-tensor prediction
get_time_index_for_lstm_trend <- 
  function(len_series,lag,pred_window,original_data_df,time_vars){
    first_time_index <- lag + 1
    last_time_index <- len_series - (pred_window - 1)
    
    lstm_trend_time_df <- original_data_df[(first_time_index:last_time_index),time_vars,drop = FALSE]#  ensure df structure
    
    return(lstm_trend_time_df)
  }

# Retrieve first dimension of either a vector or an array
get_first_dimension <- function(x){
  return(ifelse(is.null(dim(x)[1]),length(x),dim(x)[1]))
}

# Prepare data file
create_lstm_trend_df <- function(sensor_id,actual_trend,lstm_trend_time_df,lstm_predicted_trend,
                                 train_length,validation_length = 0){
  # Set sensor id
  lstm_trend_df <- data.frame(MISSION_DEVICE_TAG = rep(sensor_id,nrow(lstm_trend_time_df))) 
  # Creat dataset type column
  lstm_trend_df$data_set <- c(rep('train',train_length),
                              rep('validation',validation_length),
                              rep('test',(nrow(lstm_trend_time_df) - validation_length - train_length)))
  # Paste time rows and actual trend values
  lstm_trend_df <- cbind(lstm_trend_df,lstm_trend_time_df) 
  lstm_trend_df$actual_trend <- actual_trend
  # Add futur prediction inside prediction window as columns
  for(time_prediction in 1:dim(lstm_predicted_trend)[2]){
    col_title <- paste0('prediction_',time_prediction)
    lstm_trend_df[[col_title]] <- lstm_predicted_trend[,time_prediction]
  }
  
  return(lstm_trend_df)
}

# Plot selected results (predictions on test dataset)
plot_test_prediction_sample <- function(timestamp, rows_sel, 
                                        actual, predicted, 
                                        plot_title, time_label,
                                        file_path,save = TRUE,inline = FALSE){
  
  x <- timestamp[rows_sel]
  df <- data.frame(
    x = x,
    actual = actual[rows_sel],
    predicted = predicted[rows_sel]
  )
  
  # Create plot
  p <- ggplot(df, aes(x = x)) +
    geom_line(aes(y = actual), color = "blue", linewidth = 1) +   # actual values in blue
    geom_line(aes(y = predicted), color = "red", linewidth = 1) +  # predicted values in red
    labs(
      title = plot_title,
      x = time_label,
      y = "kWh"
    ) +
    theme_minimal()  # Optional: To use a minimal theme
  
  # Print plot either in file or inline
  if(save){
    png(file_path)
    print(p)
    dev.off()
  }
  
  if(inline){
    print(p)  
  }
}


# Plot residual distribution
plot_test_prediction_residuals <- function(residuals,file_path,save = TRUE,inline = FALSE){
  
  df <- data.frame(residuasl = residuals)
  
  # Create plot
  p <- ggplot(df, aes(x = residuals)) +
    geom_histogram(aes(y = after_stat(density)), 
                   bins = 50, 
                   fill = "azure2", 
                   color = "black", 
                   alpha = 0.7) +  # Histogram with density on y-axis
    geom_density(color = "cyan4", linewidth = 1.5) +  # Density plot in cyan4
    labs(
      title = 'Residuals distribution',
      x = "Residuals",
      y = "Density"
    ) +
    theme_minimal()  # Optional: clean theme  
  
  
  # Print plot either in file or inline
  if(save){
    png(file_path)
    print(p)
    dev.off()
  }
  
  if(inline){
    print(p)  
  }
}