######
###### Helping functions for the AutoARIMA model
######

# Pre-process lstm output data frame
preprocess_lstm_output_df <- function(ts){
  
  # get timestamp structure
  ts$timestamp <- ts$timestamp |> as.POSIXct() 
  
  # filter positive values of lstm outputs
  ts$prediction_lstm <- (abs(ts$prediction_1) + ts$prediction_1)/2
  
  # retrieve lstm residuals
  ts$lstm_residuals <- ts$actual_trend - ts$prediction_lstm
  
  return(ts)
}

# Apply authomatic ARIMA(p,d,q) analysis to time series data frame
apply_auto_arima <- function(ts, residuals_col = 'lstm_residuals', max_lag = 14, forecast_lag = 14,n_forecast = 1){
  
  # Select training data
  train <- ts[ts$data_set == 'train',residuals_col]
  
  # Run model on training data
  arima_model <- auto.arima(train,
                            max.p = max_lag,
                            max.q = max_lag)
  
  # Forecast on test dataset
  arima_coefs <- coefficients(arima_model)
  arima_order <- arimaorder(arima_model)
  
  include_drift <- FALSE
  include_mean <- TRUE
  if('drift' %in% names(arima_coefs)){
    include_drift <- TRUE
    include_mean <- FALSE
  }
  else if(arima_order[2] == 0 & !('intercept' %in% names(arima_coefs))){
    arima_coefs['intercept'] <- mean(train) # ensure intercept in model output parameters (coefficients) 
    #when there is no differencing (d=0)
  }
  
  # Apply the fixed coefficients from the original model to the test series
  arima_application_indices <- (forecast_lag + 1):nrow(ts)
  ts$arima <- rep(NA,nrow(ts))
  for(i in arima_application_indices){
    forecast_ts <- ts$lstm_residuals[(i-forecast_lag):(i-n_forecast)]
    pred_model <- Arima(forecast_ts, 
                        order = arimaorder(arima_model),  # Same p, d, q order from the training model
                        fixed = arima_coefs,
                        include.drift = include_drift,
                        include.mean = include_mean)       # Fix the coefficients
    prediction <- forecast(pred_model,h = n_forecast)
    
    # Add forecasted values to dataframe
    ts$arima[[i]] <- prediction$mean[[1]]
  }
  
  # Post process column
  ts$lstm_arima <- ts$prediction_lstm + ts$arima
  
  r <- list(ts = ts,arima_order = arimaorder(arima_model))
  
  return(r)
}

# Extract RMSE from LSTM + ARIMA results
extract_rmse_arima <- function(sensor_id,arima_output_files_path){
  ts <- read.csv(paste0(arima_output_files_path,'sensor_',sensor_id,'.csv'))
  actual <- ts[ts$data_set == 'test',"actual_trend"]
  predicted <- ts[ts$data_set == 'test',"lstm_arima"]
  rmse <- rmse_score(actual,predicted)
  return(rmse)
}

# Compmute RMSE score
rmse_score <- function(y_true, y_pred) {
  sqrt(mean((y_true - y_pred)^2))
}
