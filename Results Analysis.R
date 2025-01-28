######
###### Helping functions for the LSTM model
######

# Plot results comparison
plot_bym_test_plots <- function(results_df,output_plots_path){
  
  sensor_ids <- unique(results_df$MISSION_DEVICE_TAG)
  
  for(sensor_id in sensor_ids[1:length(sensor_ids)]){
    
    # Create df for plot
    df <- results_df[results_df$MISSION_DEVICE_TAG == sensor_id,
                     c('data_set','timestamp','y','lstm','lstm_arima','bym_predictions')]
    
    # Procsess timestamp
    df$timestamp <- df$timestamp |> as.POSIXct()
    
    # Select subset
    df <- df[df$data_set == 'test',]
    
    
    
    # Plot
    p <- ggplot(df, aes(x = timestamp)) +
      geom_line(aes(y = y, color = "Actual"), linewidth = 0.5) +   
      geom_line(aes(y = lstm, color = "LSTM"), linewidth = 0.75) + 
      geom_line(aes(y = lstm_arima, color = "LSTM + ARIMA"), linewidth = 0.75) + 
      geom_line(aes(y = bym_predictions, color = "LSTM + ARIMA + BYM"), 
                linewidth = 0.75) +  
      labs(
        title = 'Model comparisons (one-day-ahead predictions)',
        x = 'Time',
        y = "kWh",
        color = "Models"
      ) +
      scale_color_manual(
        values = c("Actual" = "grey30",'LSTM' = 'steelblue1', "LSTM + ARIMA" = "turquoise4", "LSTM + ARIMA + BYM" = "tomato3")
      ) +
      theme_minimal()
    
    
    # Save
    image_name <- paste0('sensor_',sensor_id,'.png')
    save_plot(p,save_file_path = paste0(output_plots_path,image_name))
  }  
  
  return('Done!')
}

# Save generated plot
save_plot <- function(plot,save_file_path = NULL,save_mode = TRUE,inline_mode = FALSE){
  
  # Print plot either in file or inline
  if(save_mode){
    png(save_file_path)
    print(plot)
    dev.off()
  }
  
  if(inline_mode){
    print(plot)  
  }
}

# Extract score from results df
extract_score <- function(sensor_id = NULL,results_df,
                          data_set_col = 'data_set',data_set_val = 'test',
                          actual_col = 'y',result_col,score_fun){
  
  df <- results_df
  if(!is.null(sensor_id)){ # filter data at sensor level if provided
    df <- df[df$MISSION_DEVICE_TAG == sensor_id,]  
  }
  df <- df[df[data_set_col] == data_set_val,]
  actual <- df[[actual_col]]
  predicted <- df[[result_col]]
  score_fun(actual,predicted)
}

# Compmute RMSE score
rmse_score <- function(y_true, y_pred) {
  sqrt(mean((y_true - y_pred)^2))
}

# Compute R^2
r2_score <- function(y_true, y_pred) {
  ss_res <- sum((y_true - y_pred)^2)  # Residual Sum of Squares
  ss_tot <- sum((y_true - mean(y_true))^2)  # Total Sum of Squares
  r2 <- 1 - (ss_res / ss_tot)
  return(r2)
}
