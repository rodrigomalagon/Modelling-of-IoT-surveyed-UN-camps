Modelling
================
Rodrigo Malagón
2025-01-28

``` r
# Load packages
c('sf','keras3','ggplot2','forecast','spdep','INLA') |> sapply(require,character.only = TRUE)

# Load helping functions
source('LSTM.R')
source('Spatial functions.R')
source('AutoARIMA.R')
source('BYM.R')
```

Set general environment

``` r
# General modelling seting
dir.create('./lstm_models/')
dir.create('./autoarima_models/')
dir.create('./bym_models/')
```

## LSTM models

Retrieved processed data

``` r
power_consumption <- "./data/processed_data/power_consumption_processed.csv" |> read.csv()

sensors <- "./data/processed_data/sensors_processed.csv" |> read.csv()
```

### Data preparation for LSTM models

Set current model environment

``` r
# Set environment for current model
date <- Sys.Date()
model_dir <- paste0('./lstm_models/model_',date,'/') 
lstm_data_dir <- paste0(model_dir,'lstm_data/')

dir.create(model_dir)
dir.create(lstm_data_dir)
dir.create(paste0(lstm_data_dir,'features/'))
dir.create(paste0(lstm_data_dir,'tensors/'))
```

Homologate beginning and end dates across sensors

``` r
# Homologate data at beginning and end of time series
power_consumption <- homologate_dates(power_consumption)
```

Create data frame with data and extract temporal features

``` r
## Extract and prepare features

# Set global parameters
time_vars_combination <- c('date') # level of temporal aggregation
extract_spatial_feature <- TRUE # Define if temporal feature will be incuded in modelling
topology_transformed_vars <- c('day','month') # Define cyclic variables to be separeted in cos and sine components

# Loop over sensors
for(sensor_id in sensors$MISSION_DEVICE_TAG[1:100]){
  
  # Filter the sensor's time series data (timestamp and target variable)
  data_df <- power_consumption[power_consumption$MISSION_DEVICE_TAG == sensor_id,]
  data_df <- data_df[c('reg_timestamp','value_diff_interpolated')]
  colnames(data_df) <- c('timestamp','y')

  # Extract main time features
  data_df$date <- data_df$timestamp |> as.POSIXct() |>lubridate::date()
  data_df$hour <- data_df$timestamp |> as.POSIXct() |>lubridate::hour()
 
  # Define time combination (aggregation level) through an aggregation formula
  frml <- create_formula(y_var = 'y', x_vars = time_vars_combination)
  data_df <- aggregate(frml,data = data_df, FUN = sum) # sum all power consumption values per time slot
 
  # Extract spatial features if needed
  if(extract_spatial_feature){
    
    # Identify neighbours and extract their compunt mean value of the target variable
    neighbours_ids <- get_neighbours_ids(sensor_id = sensor_id, sensors = sensors)
    neigh_mean_df <- get_mean_neighbourhood(neighbours_ids = neighbours_ids,
                                            time_series_dataset = power_consumption,
                                            time_vars_combination = time_vars_combination)
    data_df$neigh_value <- neigh_mean_df$neigh_value    
  }
  
  # Extract other time features
  data_df$month <- data_df$date |> as.POSIXct()|>lubridate::month()
  data_df$day <- data_df$date |> as.POSIXct()|> lubridate::day()
  data_df$dow <- data_df$date |> as.POSIXct()|> weekdays()
  
 # Transform topology of cyclic data
  data_df_processed <- transform_to_circ_topology(data_df,topology_transformed_vars) 
  
  # Distinguish variable types (numeric, categoricañ)
  numeric_vars <- character()
  if(extract_spatial_feature){
    numeric_vars <- c(numeric_vars,c('neigh_value'))
  }
  for(var in topology_transformed_vars){
    numeric_vars <- c(numeric_vars,paste0(var,'_x'),paste0(var,'_y'))
  }
  cat_vars <- c('dow')
  
  # Process variables according to type (normalization for numeric and boolean extraction for categorical)
  data_df_processed <- process_cols(data_df_processed,target_var = 'y',numeric_vars = numeric_vars,cat_vars = cat_vars)
  
  # Save processed data per sensor
  r <- list(data_df = data_df,data_df_processed = data_df_processed)
  saveRDS(r,file = paste0(lstm_data_dir,'features/','sensor_',sensor_id,'.rds'))
}
```

Create tensors from extracted features

``` r
## Create tensors from extracted features

# Set global parameters
input_dir <- paste0(lstm_data_dir,'features/')
output_dir <- paste0(lstm_data_dir,'tensors/')

# Tensor settings
lag <- 14
pred_window <- 1
train_val_ratio <- NULL#default train-validation split omitted
train_test_ratio <- 0.8
```

``` r
# Loop over sensor files
for(file in list.files(input_dir,full.names = TRUE)){
  
  # Retrieved dataset with features
  data <- readRDS(file)
  data_df_processed <- data$data_df_processed
  
  
  # Get known future features (other feature apart from power consumption)
  known_features <- colnames(data_df_processed)
  known_features <- known_features[!(known_features %in% c('y','neigh_value'))]
  
  # Perform temporal shift of known features
  data_df_processed <- shift_known_futere_features(data_df = data_df_processed,
                                                   known_features = known_features)

  # Create data matrix
  data_matrix <- create_data_matrix(data_df_processed,del_time_cols = c('date'))

  # Set tensor parameters
  len_series <- nrow(data_matrix)
  num_vars <- ncol(data_matrix)


  # Obtain lstm main tensors
  lstm_tensors <- create_lstm_tensor(data = data_matrix, len_series = len_series, num_vars = num_vars,
                                     lag = lag, pred_window = pred_window)
  X <- lstm_tensors$x_array
  y <- lstm_tensors$y_array

  
  # Split tensor
  tensors <- split_data(X,y,train_val_ratio = train_val_ratio,
                               train_test_ratio = train_test_ratio)#no train_val_ratio used  
  lstm_tensors <- list(X = X,y = y, split_tensors = tensors) 
  
  # Save dataset
  save_obj <- list(data = data, lstm_tensors = lstm_tensors)
  saveRDS(save_obj, file = paste0(output_dir,basename(file)))
}
```

Create setup file

``` r
# Generate setup file
writeLines(c("Date of tensors creation:", as.character(Sys.time()),
             'Lag:',lag,
             'Prediction window size:',pred_window,
             'Time variables combination (aggregation level):',time_vars_combination,
             'Extraction of mean neighbourhood feature',extract_spatial_feature
             ),
           paste0(model_dir,"data_setup_parameters.md"))
```

### Run LSTM models

Set up environment for model outputs

``` r
# Set environment for current model's output
output_trend_path <- paste0(model_dir,'/outputs/')
dir.create(output_trend_path)
```

Set up model setting

``` r
## Run LSTM models

# Set input
input_dir <- paste0(lstm_data_dir,'tensors/')

# LSTM parameters
seed <- 100
optimizer_lr <- 1e-3
batch_size_lag_factor <- round((2 * 30 / lag)) # aprox 2 month batch size
batch_size <- batch_size_lag_factor * lag
num_epochs <- 10
loss_function <- 'mean_squared_error'
metrics <- c('mae')
```

``` r
# Loop over sensor tensor files
for(file in list.files(input_dir,full.names = TRUE)){
  # Retrieve analysis-ready data
  sensor_data <- readRDS(file)
  name <- sub('.rds','',basename(file)) |> strsplit(split = '_',fixed = TRUE) |> unlist()
  sensor_id <- name[2]
  data <- sensor_data$data
  lstm_tensors <- sensor_data$lstm_tensors
  num_vars <- dim(lstm_tensors$split_tensors$x_train)[3]
  len_series <- nrow(data$data_df)
  
  ## Set and run model
  
  # Seed
  set.seed(seed)
  tensorflow::set_random_seed(seed) 
  
  # Create LSTM architecture
  lstm_model <- create_lstm_model(lag,num_vars,pred_window)
  
  # Compile model
  compile(lstm_model,
          loss = loss_function,
          optimizer =  optimizer_adam(learning_rate = optimizer_lr),
          metrics = metrics)
  
  # Fit model
  if(!is.null(train_val_ratio)){ # use train-validation split if available
    val_data <- list(lstm_tensors$split_tensors$x_validation, lstm_tensors$split_tensors$y_validation)
  }else{
    val_data <- list(lstm_tensors$split_tensors$x_test, lstm_tensors$split_tensors$y_test)
  }
  history <- lstm_model |> fit(
    x = lstm_tensors$split_tensors$x_train, 
    y = lstm_tensors$split_tensors$y_train,
    epochs = num_epochs,
    batch_size = batch_size,
    validation_data = val_data,
    verbose = 2
  ) 
  lstm_model_fit <- list(model = lstm_model, history=history)
  
  # Compute trend as the predicted value by LSTM
  lstm_trend <- lstm_model_fit$model |> predict(lstm_tensors$X)
  
  # Bump up predictions and actual trend
  for(time_prediction in 1:dim(lstm_trend)[2]){
    lstm_trend[,time_prediction] <- bump_up_z_score(lstm_trend[,time_prediction],data$data_df$y)
  }
  actual_trend <- bump_up_z_score(lstm_tensors$y[,1],data$data_df$y)
  
    # Retrieve correct trend time indices and prepare trend file
  lstm_trend_time_df <- get_time_index_for_lstm_trend(len_series = len_series,
                                lag = lag,
                                pred_window = pred_window,
                                original_data_df = data$data_df,
                                time_vars = time_vars_combination)
  lstm_trend_time_df$timestamp <- lstm_trend_time_df$date |> as.POSIXct(format = "%Y-%m-%d")
  
  # Create trend data frame
  train_length <- get_first_dimension(lstm_tensors$split_tensors$y_train)
  
  #validation_length <- get_first_dimension(lstm_tensors$split_tensors$y_validation)
  lstm_trend_df <- create_lstm_trend_df(sensor_id = sensor_id,
                                           actual_trend = actual_trend,
                                           lstm_trend_time_df = lstm_trend_time_df,
                                           lstm_predicted_trend = lstm_trend,
                                        train_length = train_length,
                                        validation_length = 0# default: validation data omitted
                                           )
  
  # Save trend data frame
  file_path <- paste0(output_trend_path,'sensor_',sensor_id,'.csv')
  write.csv(lstm_trend_df,file = file_path,row.names = FALSE) 
}
```

Generate plots

``` r
# Set global parameters
output_plot_path <- paste0(model_dir,'/plots/prediction_daily_test_plot_samples/')
output_residual_hist_path <- paste0(model_dir,'/plots/residuals_daily_test_histograms/')

dir.create(paste0(model_dir,'plots/'))
dir.create(output_plot_path)
dir.create(output_residual_hist_path)

# Plot parameters
test_plot_data_selection <- 1:90 # window size selection 1:30 first month of test set
save_plots <- TRUE
```

``` r
input_dir <- paste0(model_dir,'outputs/')

# Loop over sensors lstm outputs
for(file in list.files(input_dir,full.names = TRUE)){
  
  # Read data
  lstm_trend_df <- read.csv(file)
  sensor_id <- lstm_trend_df$MISSION_DEVICE_TAG[[1]]
  lstm_trend_df$timestamp <- lstm_trend_df$timestamp |> as.POSIXct() |> as.Date()
  
  # Retrieve test dataset predictions
  predictions <- lstm_trend_df[lstm_trend_df$data_set == 'test',c('timestamp','actual_trend','prediction_1')]
  colnames(predictions)[which(colnames(predictions) == 'actual_trend')] <- 'actual_value'
  colnames(predictions)[which(colnames(predictions) == 'prediction_1')] <- 'predicted_value'
  
  # Plot predictions
  plot_title <- paste0("One-step ahead predictions (lag = ",lag,', pred_window = ',pred_window,')')
  time_label <- 'time (day)'
  file_path <- paste0(output_plot_path,'sensor_',sensor_id,'.png')
  plot_test_prediction_sample(timestamp = predictions$timestamp,
                            rows_sel = test_plot_data_selection,
                            actual = predictions$actual_value,
                            predicted = predictions$predicted_value,
                            plot_title = plot_title,
                            time_label = time_label,
                            file_path,save = save_plots,inline = FALSE)
  
  # Plot residuals histograms
  residuals_test <- predictions$predicted_value - predictions$actual_value
  file_path <- paste0(output_residual_hist_path,'sensor_',sensor_id,'.png')
  plot_test_prediction_residuals(residuals_test,file_path,save = save_plots,inline = FALSE)
}
```

Generate setup file

``` r
# Generate setup file with LSTM settings
setup_info <- c(
            ' -- DATA (TENSOR) SETUP --',
             'Lag:',lag,
             'Prediction window size:',pred_window,
             'Time variables combination (aggregation level):',time_vars_combination,
             'Extraction of mean neighbourhood feature',extract_spatial_feature,
             ' -- MODEL SETUP --',
            "Date of last models run:", as.character(Sys.time()),
             'seed:',seed,
             'Optimizer learning rate:',optimizer_lr,
             'Batch size-lag factor:',batch_size_lag_factor,
             'Number of epohcs:',num_epochs
             )
writeLines(setup_info,
           paste0(model_dir,"model_setup_parameters.md"))
```

## AutoARIMA models

Setup environment structure

``` r
# Setup environment structure
date <- Sys.Date()
models_dir <- 
  paste0('./autoarima_models/','model_',date,'/')
lstm_arima_prediction_path <- 
  paste0(models_dir,'outputs/') 
plots_dir <- paste0(models_dir,'/plots/prediction_daily_test_plots/')

residuals_histograms_dir <- 
  paste0(models_dir,'/plots/residuals_daily_test_distribution_histograms/')

dir.create(models_dir)
dir.create(paste0(models_dir,'plots/'))
dir.create(lstm_arima_prediction_path)
dir.create(residuals_histograms_dir)
```

Iterate over sensors to correcg LSTM outputs with AutoARIMA method

``` r
# Set global parameters
input_dir <-  './lstm_models/model_2025-01-28/outputs/'
files <- list.files(input_dir,full.names = TRUE)

# Setup register of automatic ARIMA parameters found by the model
arima_orders <- data.frame(source_file = files,
                           MISSION_DEVICE_TAG = rep(NA,length(files)),
                           p = rep(NA,length(files)),
                           d = rep(NA,length(files)),
                           q = rep(NA,length(files)))
  
# Loop over sensor predictions by LSTM
for(file in files){
  
  ts <- read.csv(file)
  sensor_id <- ts$MISSION_DEVICE_TAG[1]
  
  # Assign sensor id to orders data frame
  arima_orders$MISSION_DEVICE_TAG[arima_orders$source_file == file] <- sensor_id
  
  # Pre process lstm output
  ts <- preprocess_lstm_output_df(ts)
  
  # Apply Arima model to residuals
  r <- apply_auto_arima(ts)
  
  #Clean negatives in LSTM+ARIMA predictions files
  ts$ts$lstm_arima <- ifelse(r$ts$lstm_arima >= 0,r$ts$lstm_arima,0)
  
  # Save time predictions
  file_path <- paste0(lstm_arima_prediction_path,'sensor_',sensor_id,'.csv')
  write.csv(r$ts,
            file = file_path,
            row.names = FALSE)
  
    # Save arima order
  arima_orders$p[which(arima_orders$source_file == file)] <- r$arima_order[1]
  arima_orders$d[which(arima_orders$source_file == file)] <- r$arima_order[2]
  arima_orders$q[which(arima_orders$source_file == file)] <- r$arima_order[3]
  
}
```

``` r
# Save arima orders dataset
arima_orders_file_path <- paste0(models_dir,'arima_orders.csv')
write.csv(arima_orders,
            file = arima_orders_file_path,
            row.names = FALSE)
```

Create diagnostics

``` r
##  Create diagnostics register
file_path <- paste0(models_dir,'arima_orders.csv')
arima_diag <- read.csv(file_path)

# Extract test dataset rmse for LSTM + ARIMA model
arima_diag$rmse <- rep(NA,nrow(arima_diag))
for(sensor_id in arima_diag$MISSION_DEVICE_TAG){
  rmse <- extract_rmse_arima(sensor_id = sensor_id,
                             arima_output_files_path = lstm_arima_prediction_path)
  arima_diag$rmse[which(arima_diag$MISSION_DEVICE_TAG == sensor_id)] <- rmse
}

# Order by RMSE
arima_diag <- arima_diag |> dplyr::arrange(rmse)

# Save diagnostics
arima_diag_file_path <- paste0(models_dir,'lstm_arima_diagnostics.csv')
write.csv(arima_diag,file = arima_diag_file_path,
          row.names = FALSE)
```

## BYM model

### Data preparation for BYM

``` r
sensors_sf <- "./data/processed_data/sensors_processed.csv" |> read.csv() |> create_sf_from_df()
```

Filter best sensors (rmse \> 7)

``` r
# Filter best sensors (rmse > 7)
rmse_threshold <- 7
arima_diag <- './autoarima_models/model_2025-01-28/lstm_arima_diagnostics.csv' |> read.csv()
sensors_filter <- arima_diag$MISSION_DEVICE_TAG[arima_diag$rmse < rmse_threshold]
sensors_sf <- sensors_sf[sensors_sf$MISSION_DEVICE_TAG %in% sensors_filter,]
```

Build dataset for BYM model

``` r
# Build data frame from LSTM + ARIMA output files
lstm_arima_prediction_path <- './autoarima_models/model_2025-01-28/outputs/' 
arima_files <- list.files(lstm_arima_prediction_path,full.names = TRUE)
selected_cols <- c('MISSION_DEVICE_TAG','data_set','timestamp','actual_trend','prediction_lstm','lstm_arima')
data <- arima_files[1] |> read.csv()
data <- data[selected_cols]
for(i in 2:length(arima_files)){
  ts <- read.csv(arima_files[i])
  ts <- ts[selected_cols]
  data <- rbind(data,ts)
}

# Filter best-rmse-ranking sensors
data <- data[data$MISSION_DEVICE_TAG %in% sensors_sf$MISSION_DEVICE_TAG,]

# Filter out NA values from non-predicted timestamps from ARIMA model
data <- data[!(is.na(data$lstm_arima)),]

# Rename columns
colnames(data)[which(colnames(data) == 'MISSION_DEVICE_TAG')] <- 'location'
colnames(data)[which(colnames(data) == 'timestamp')] <- 'time'
colnames(data)[which(colnames(data) == 'actual_trend')] <- 'y'
colnames(data)[which(colnames(data) == 'prediction_lstm')] <- 'lstm'
```

Order datasets to match location id’s order

``` r
# Order data and locations data frames
data <- data |> dplyr::arrange(location)
sensors_sf <- sensors_sf |> dplyr::arrange(MISSION_DEVICE_TAG)
```

Re-index locations with sequential numeric ids

``` r
## Re-index locations ids with sequential ids

data$MISSION_DEVICE_TAG <- data$location # save unprocessed location for results extraction
l <- unique(data$location)

# Definition of diccionary to perform re-indexing
locations_dictionary <- list(id = 1:length(l), location = l)
data$location <- lapply(data$location, function(name){
  locations_dictionary$id[which(locations_dictionary$location == name)]
}) |> unlist()
```

Process time and target/expected values

``` r
## Process time and target/expected values for BYM

# Time processing
data$timestamp <- data$time # save unprocessed time for results extraction

data$time <- data$time |> as.POSIXct()
data$time <- as.Date(data$time)- as.Date(data$time[[1]])
data$time <- data$time|>as.factor()

# Extract second location column for structured component of BYM
data$location.struct <- data$location

# Extract residual
data$residual <- data$y - data$lstm_arima
```

Residual log transformation

``` r
m <- min(data$residual) -  1e-6
data$log_residual <- log(data$residual - m)
```

Select variables for BYM model and final results

``` r
# Variables selection
data <- data[c('data_set','MISSION_DEVICE_TAG','location','location.struct','timestamp','time','y','lstm','lstm_arima','residual','log_residual')]
```

### Voronoi setting

Create voronoi from sensors dataset

``` r
## Create voronoi from sensors dataset

# Parameters
voronoi_buffer_dist <- 30

# Create voronoi
voronoi <- extract_voronoi_from_sensors(sensors_sf,
                                    outer_buffer_distance=units::set_units(voronoi_buffer_dist,'m'))


voronoi_sf <- st_sf(id = 1:length(voronoi),
                    geometry = voronoi)
```

Retrieve adjacency structure

``` r
# Retrieve adjacency structure from voronoi polygons
nb <- poly2nb(voronoi)  # Neighbours from spatial polygons
adj_matrix <- nb2mat(nb)
adj_graph_obj <- inla.read.graph(adj_matrix)
```

### Run model

Priors and formula setting

``` r
# Priors setting
besag.precision.prior <- list(theta = list(prior = "loggamma", param = c(1, 0.01))) 
space_unstr.precision.prior<- list(theta = list(prior = "loggamma", param = c(1,0.01)))
time.precision.prior <- list(theta = list(prior = "loggamma", param = c(1,0.01)))

# Set formula with or without priors
formula_bym <- log_residual ~ 1 + f(location.struct, model = "besag", graph = adj_graph_obj,
    #hyper = besag.precision.prior,
    constr = FALSE) + 
  f(location, model = "iid"#, 
    #hyper = space_unstr.precision.prior
    ) +
  f(time, model = "iid"#, 
    #hyper = time.precision.prior
    )
```

``` r
# Data selection for modelling
data_bym <- data
data_bym$log_residual[data_bym$data_set == 'test'] <- NA 

# Run model
result_bym <- INLA::inla(
  formula = formula_bym,
  family = "gaussian",
  data = data_bym,
  control.predictor = list(compute = TRUE),  # Computes fitted values
  control.compute = list(dic = TRUE, waic = TRUE)  # Computes model
)

summary(result_bym)
```

Set results directory

``` r
# Setup environment structure for outputs
date <- Sys.Date()
bym_outputs_dir <- paste0('./bym_models/model_',date,'/')
dir.create(bym_outputs_dir)
```

Save BYM results

``` r
# Save BYM results object
file_path <- paste0(bym_outputs_dir,'bym_results_',voronoi_buffer_dist,'m.rds')
saveRDS(object = result_bym,file = file_path)


# Extract BYM prediction results
data_pred <- cbind(data_bym,
                   bym_predictions = exp(result_bym$summary.fitted.values$mean) + 
                     data_bym$lstm_arima + m)

# Save BYM outputs
file_name <- paste0('bym_outputs_',voronoi_buffer_dist,'m.csv')
write.csv(data_pred,file = paste0(bym_outputs_dir,file_name),row.names = FALSE)
```
