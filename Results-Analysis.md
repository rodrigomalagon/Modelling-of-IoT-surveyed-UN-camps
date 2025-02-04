Results Analysis
================
Rodrigo Malagón
2025-01-28

``` r
# Load packages
c('ggplot2','sf','corrplot') |> sapply(require,character.only = TRUE)

# Load helping functions
source('Results Analysis.R')
source('Spatial functions.R')
source('BYM.R')
```

## Model metrics

``` r
# Retrieve results
bym_outputs_dir <- paste0('./bym_models/model_','2025-01-28','/')
results_10m <- paste0(bym_outputs_dir,'bym_outputs_10m.csv') |> read.csv()
results_30m <- paste0(bym_outputs_dir,'bym_outputs_30m.csv') |> read.csv()
```

Generate plots to compare model fits per sensor

``` r
# Generate plots to compare model fits per sensor
it <- list(
  list(
    voronoi_buffer_dist = 10,
    results_df = results_10m
  ),
  list(
    voronoi_buffer_dist = 30,
    results_df = results_30m
  )
)

for(i in 1:length(it)){
  output_plots_path <- paste0(bym_outputs_dir,'prediction_test_plots_',it[[i]]$voronoi_buffer_dist,'m/')
  dir.create(output_plots_path)
  
  # Create comparison plots
  plot_bym_test_plots(results_df = it[[i]]$results_df,
                      output_plots_path = output_plots_path)
}
```

Generate diagnostics data frame

``` r
diagnostics <- data.frame(MISSION_DEVICE_TAG = unique(results_10m$MISSION_DEVICE_TAG))

## Get scores per model combination

# Rmse
diagnostics$lstm_rmse <- lapply(diagnostics$MISSION_DEVICE_TAG, function(sensor_id){
  extract_score(sensor_id = sensor_id,
                          results_df = results_10m,
                          result_col = 'lstm',score_fun = rmse_score)
}) |> unlist()

diagnostics$lstm_arima_rmse <- lapply(diagnostics$MISSION_DEVICE_TAG, function(sensor_id){
  extract_score(sensor_id = sensor_id,
                          results_df = results_10m,
                          result_col = 'lstm_arima',score_fun = rmse_score)
}) |> unlist()

diagnostics$lstm_arima_bym_10m_rmse <- lapply(diagnostics$MISSION_DEVICE_TAG, function(sensor_id){
  extract_score(sensor_id = sensor_id,
                          results_df = results_10m,
                          result_col = 'bym_predictions',score_fun = rmse_score)
}) |> unlist()

diagnostics$lstm_arima_bym_30m_rmse <- lapply(diagnostics$MISSION_DEVICE_TAG, function(sensor_id){
  extract_score(sensor_id = sensor_id,
                          results_df = results_30m,
                          result_col = 'bym_predictions',score_fun = rmse_score)
}) |> unlist()


# R2
diagnostics$lstm_r2 <- lapply(diagnostics$MISSION_DEVICE_TAG, function(sensor_id){
  extract_score(sensor_id = sensor_id,
                          results_df = results_10m,
                          result_col = 'lstm',score_fun = r2_score)
}) |> unlist()

diagnostics$lstm_arima_r2 <- lapply(diagnostics$MISSION_DEVICE_TAG, function(sensor_id){
  extract_score(sensor_id = sensor_id,
                          results_df = results_10m,
                          result_col = 'lstm_arima',score_fun = r2_score)
}) |> unlist()

diagnostics$lstm_arima_bym_10m_r2 <- lapply(diagnostics$MISSION_DEVICE_TAG, function(sensor_id){
  extract_score(sensor_id = sensor_id,
                          results_df = results_10m,
                          result_col = 'bym_predictions',score_fun = r2_score)
}) |> unlist()

diagnostics$lstm_arima_bym_30m_r2 <- lapply(diagnostics$MISSION_DEVICE_TAG, function(sensor_id){
  extract_score(sensor_id = sensor_id,
                          results_df = results_30m,
                          result_col = 'bym_predictions',score_fun = r2_score)
}) |> unlist()


write.csv(diagnostics,file = paste0(bym_outputs_dir,'diagnostics.csv'),row.names = FALSE)
```

Study metrics distribution

``` r
diag_select <- diagnostics
diag_select <- diag_select %>% dplyr::select(contains("rmse"))

diagnostics_long <- tidyr::pivot_longer(diag_select,
                              cols = everything(), names_to = "model", values_to = "rmse")

# Rename
diagnostics_long$model[diagnostics_long$model == 'lstm_arima_bym_30m_rmse'] <- 'BYM(30M)'
diagnostics_long$model[diagnostics_long$model == 'lstm_arima_bym_10m_rmse'] <- 'BYM(10M)'
diagnostics_long$model[diagnostics_long$model == 'lstm_rmse'] <- 'LSTM'
diagnostics_long$model[diagnostics_long$model == 'lstm_arima_rmse'] <- 'LSTM + ARIMA'

p <- ggplot(diagnostics_long, aes(x = model, y = rmse, fill = model)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distibution of RMSE across sensors", x = "Models", y = "RMSE") +
  scale_fill_brewer(palette = "Set3")

#Save 

model_comparison_plot_dir <- paste0(bym_outputs_dir,'model_comparison/')
dir.create(model_comparison_plot_dir)

image_name <- 'model_rmse_comparison.png'
save_plot(p,save_file_path = paste0(model_comparison_plot_dir,image_name),inline_mode = TRUE)
```

Study overall metrics

``` r
# Extract overall metrics
overall_metrics <- data.frame(
  model = c('LSTM','LSTM + AutoARIMA','LSTM + AutoARIMA + BYM(10m)','LSTM + AutoARIMA + BYM(30m)'),
  rmse_train = c(
    extract_score(results_df = results_10m,result_col = 'lstm',score_fun = rmse_score,data_set_val = 'train'),
    extract_score(results_df = results_10m,result_col = 'lstm_arima',score_fun = rmse_score,data_set_val = 'train'),
    extract_score(results_df = results_10m,result_col = 'bym_predictions',score_fun = rmse_score,data_set_val = 'train'),
    extract_score(results_df = results_30m,result_col = 'bym_predictions',score_fun = rmse_score,data_set_val = 'train')
    ),
  r2_train = c(
    extract_score(results_df = results_10m,result_col = 'lstm',score_fun = r2_score,data_set_val = 'train'),
    extract_score(results_df = results_10m,result_col = 'lstm_arima',score_fun = r2_score,data_set_val = 'train'),
    extract_score(results_df = results_10m,result_col = 'bym_predictions',score_fun = r2_score,data_set_val = 'train'),
    extract_score(results_df = results_30m,result_col = 'bym_predictions',score_fun = r2_score,data_set_val = 'train')
    ),
    rmse_test = c(
    extract_score(results_df = results_10m,result_col = 'lstm',score_fun = rmse_score,data_set_val = 'test'),
    extract_score(results_df = results_10m,result_col = 'lstm_arima',score_fun = rmse_score,data_set_val = 'test'),
    extract_score(results_df = results_10m,result_col = 'bym_predictions',score_fun = rmse_score,data_set_val = 'test'),
    extract_score(results_df = results_30m,result_col = 'bym_predictions',score_fun = rmse_score,data_set_val = 'test')
    ),
  r2_test = c(
    extract_score(results_df = results_10m,result_col = 'lstm',score_fun = r2_score,data_set_val = 'test'),
    extract_score(results_df = results_10m,result_col = 'lstm_arima',score_fun = r2_score,data_set_val = 'test'),
    extract_score(results_df = results_10m,result_col = 'bym_predictions',score_fun = r2_score,data_set_val = 'test'),
    extract_score(results_df = results_30m,result_col = 'bym_predictions',score_fun = r2_score,data_set_val = 'test')
    )
)
overall_metrics$r2_train <- overall_metrics$r2_train * 100
overall_metrics$r2_test <- overall_metrics$r2_test * 100
overall_metrics
```

``` r
# Save overall metrics
# Round numeric cols to 2 digits
for(col in colnames(overall_metrics)){
  if(is.numeric(overall_metrics[[col]])){
    overall_metrics[[col]] <- overall_metrics[[col]] |> round(digits = 2)
  }
}

write.csv(overall_metrics,
          file = paste0(bym_outputs_dir,'model_comparison/overall_metrics.csv'),
          row.names = FALSE)
```

``` r
# Create plot of overall metrics
overall_metrics_long <- tidyr::pivot_longer(overall_metrics, cols = c(r2_train,r2_test), 
                          names_to = "Feature", values_to = "Value")

# Create bar plots
p <- ggplot(overall_metrics_long, aes(x = Value, y = Feature, fill = model)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "R^2 model comparison", x = "Value", y = "Metric") +
  scale_fill_brewer(palette = "Set2")

# Save plot
image_name <- 'overall_r2_comparison.png'
save_plot(p,save_file_path = paste0(model_comparison_plot_dir,image_name),inline_mode = TRUE)
```

## BYM effects

Set model to analyse

``` r
bym_outputs_dir <- paste0('./bym_models/model_','2025-01-28','/')
```

Retrieve BYM results

``` r
bym_results <- list()
for(buff_dist in c(10,30)){
  lab <- paste0(buff_dist,'m')
  bym_results[[lab]] <-  paste0(bym_outputs_dir,'bym_results_',lab,'.rds') |> readRDS()
}
```

Sensor data alignment

``` r
# Read sensor data and order by name to align with BYM setting
sensors_df <- "./data/sensors_selected.csv" |>
  read.csv()  |>
  dplyr::arrange(MISSION_DEVICE_TAG)

# Extract sensores considered in BYM models
bym_10_outputs <- paste0(bym_outputs_dir,'bym_outputs_10m.csv') |> 
  read.csv()
bym_modelled_sensors <- bym_10_outputs$MISSION_DEVICE_TAG |> unique()

# Filter sensors data frame accordinf to the retrieved ids
sensors_df <- sensors_df[sensors_df$MISSION_DEVICE_TAG %in% bym_modelled_sensors,]
```

Prepare output directory

``` r
dir.create(paste0(bym_outputs_dir,'spatial_effects/'))
```

Save structured and structured effect data frames

``` r
for(voronoi_buffer_dist in c(10,30)){
  
  # Extract model results
  lab <- paste0(voronoi_buffer_dist,'m')
  model_results <- bym_results[[lab]]
  
  # Create voronoi polygons
  sensors_sf <- sensors_df |> create_sf_from_df()
  voronoi_geoms <- extract_voronoi_from_sensors(sensors_sf,                                        outer_buffer_distance=units::set_units(voronoi_buffer_dist,'m'))
  voronoi_sf <- st_sf(sensors_sf$MISSION_DEVICE_TAG,
                    geometry = voronoi_geoms)
  
  # Set up output sf objects
  spatial_effects_df <- data.frame(
      MISSION_DEVICE_TAG = sensors_df$MISSION_DEVICE_TAG,
      structured_effect = model_results$summary.random$location$mean,
      unstructured_effect = model_results$summary.random$location.struct$mean,
      geometry = voronoi_sf$geometry
      ) |> st_sf(crs = st_crs(voronoi_sf))
  
  # Write data frame
  file_path <- paste0(bym_outputs_dir,'spatial_effects/spatial_effects_',
                      voronoi_buffer_dist,'m.shp')
  st_write(spatial_effects_df,dsn = file_path,row.names = FALSE)
}
```

## Results analysis

Set model to analyse and retrieve error dataset with lat-long

``` r
bym_outputs_dir <- paste0('./bym_models/model_','2025-01-28','/')
diagnostics_loc <- paste0(bym_outputs_dir,'diagnostics_loc.csv') |> read.csv()
```

### Error extraction

Retrieve diagnostic metrics

``` r
diagnostics <- paste0(bym_outputs_dir,'diagnostics.csv') |> read.csv()
```

Sensors location retrieval and merge

``` r
file_path <- "./data/sensors_selected.csv"
sensors <- read.csv(file_path)
sensors_sf <- create_sf_from_df(sensors_df = sensors)

diagnostics_loc <- merge(diagnostics,
      sensors[c('MISSION_DEVICE_TAG','long','lat')],
      by = 'MISSION_DEVICE_TAG',
      all.x = TRUE)
```

Save/load error df with locations

``` r
file_path <-paste0(bym_outputs_dir,'diagnostics_loc.csv')
write.csv(diagnostics_loc,file = file_path,row.names = FALSE)
```

### Error vs temporal variability and spatial structure

Build results analysis folder

``` r
results_analysis_dir <- paste0(bym_outputs_dir,'results_analysis/')
dir.create(results_analysis_dir)
```

Retrieve time series data

``` r
ts_df <- paste0(bym_outputs_dir,'bym_outputs_10m.csv') |> read.csv()
```

Extract variance features per sensor

``` r
# VAR per sensor
diagnostics_loc$ts_var <- diagnostics_loc$MISSION_DEVICE_TAG |> lapply(
  function(sensor_id){
    ts <- ts_df[ts_df$MISSION_DEVICE_TAG == sensor_id,'y']
    var(ts)
  }
) |> 
  unlist()

# SD(Moving VAR) per sensor
diagnostics_loc$ts_mov_var_sd <- diagnostics_loc$MISSION_DEVICE_TAG |> lapply(
  function(sensor_id){
    ts <- ts_df[ts_df$MISSION_DEVICE_TAG == sensor_id,'y']
    
    # Moving window var extraction
    vars <- numeric()
    window_rad <- 7  # radius of window
    for(i in 1:length(ts)){
      window <- (max(0,i-window_rad):min(length(ts),i+window_rad))
      vars <- c(vars,var(ts[window]))
    }
    sd(vars)
  }
) |> 
  unlist()

# MINMAX(Moving VAR) per sensor
diagnostics_loc$ts_mov_var_minmax <- diagnostics_loc$MISSION_DEVICE_TAG |> lapply(
  function(sensor_id){
    ts <- ts_df[ts_df$MISSION_DEVICE_TAG == sensor_id,'y']
    
    # Moving window var extraction
    vars <- numeric()
    window_rad <- 7  # radius of window
    for(i in 1:length(ts)){
      window <- (max(0,i-window_rad):min(length(ts),i+window_rad))
      vars <- c(vars,var(ts[window]))
    }
    max(vars) - min(vars)
  }
) |> 
  unlist()
```

Extract spatial features

``` r
# Set sf object for spatial analysis
diagnostics_loc_sf <- diagnostics_loc |> create_sf_from_df() 

# Extract number of neighbours per sensor closer to a given distance
diagnostics_loc$num_neigh_10m <- diagnostics_loc$MISSION_DEVICE_TAG |> lapply(
  function(sensor_id){
    dist <- st_distance(diagnostics_loc_sf[diagnostics_loc_sf$MISSION_DEVICE_TAG == sensor_id,],diagnostics_loc_sf) < units::as_units(10,'m') # Buffer distance
    sum(dist)
  }
) |> unlist()

diagnostics_loc$num_neigh_30m <- diagnostics_loc$MISSION_DEVICE_TAG |> lapply(
  function(sensor_id){
    dist <- st_distance(diagnostics_loc_sf[diagnostics_loc_sf$MISSION_DEVICE_TAG == sensor_id,],diagnostics_loc_sf) < units::as_units(30,'m') # Buffer distance
    sum(dist)
  }
) |> unlist()
```

Centroid distance analysis

``` r
# Get centroid of all sensors
centroid <- diagnostics_loc_sf |> st_union() |> st_centroid()


diagnostics_loc$dist_cent <- diagnostics_loc$MISSION_DEVICE_TAG |> lapply(
  function(sensor_id){

      # Obtain distance to centroid
    d <- st_distance(diagnostics_loc_sf[diagnostics_loc_sf$MISSION_DEVICE_TAG == sensor_id,"geometry"],centroid)
    return(d)
  }
) |> unlist()
```

Distance to 10m-adjacency-graph component centroid

``` r
# Classify all points by connected component of within-20m relationship (10m buffer polygons touching each other)
adj_matrix <- st_distance(diagnostics_loc_sf) < units::set_units(20,'m')
graph <- igraph::graph_from_adjacency_matrix(adj_matrix,mode = 'undirected')
components <- igraph::components(graph)
diagnostics_loc_sf$connected_component <- components$membership

# Obtain centroid per component
component_centroids <- diagnostics_loc_sf |>
  dplyr::group_by(connected_component) |>         # Group by the "connected_component" column
  dplyr::summarize(geometry = st_centroid(st_union(geometry)))
```

Plot extracted connected components and centroids

``` r
plot(diagnostics_loc_sf$geometry,col='blue')
plot(component_centroids$geometry,col = 'red',add = TRUE,pch = 16)
```

``` r
diagnostics_loc$dist_cent_con_comp <- diagnostics_loc$MISSION_DEVICE_TAG |> lapply(function(sensor_id){
  sensor_selection <- diagnostics_loc_sf[diagnostics_loc_sf$MISSION_DEVICE_TAG == sensor_id,]
  centroid <- component_centroids[component_centroids$connected_component == sensor_selection$connected_component,"geometry"]
  st_distance(sensor_selection$geometry,centroid)
})|> unlist()
```

Save extracted data

``` r
file_path <- paste0(results_analysis_dir,'rmse_analysis.csv')
write.csv(diagnostics_loc,file = file_path,row.names = FALSE)
```

### Correlation analysis

``` r
# Filter and rename cols of interest for correlation analysis
cols <- c('lstm_rmse','lstm_arima_rmse','lstm_arima_bym_10m_rmse',
          'lstm_arima_bym_30m_rmse','ts_var','ts_mov_var_sd','num_neigh_10m','num_neigh_10m',
          'dist_cent','dist_cent_con_comp')
df <- diagnostics_loc[cols]

colnames(df) <- c('LSTM_RMSE','ARIMA_RMSE','BYM30_RMSE',
                  'BYM10_RMSE','VAR','SD(MOV_VAR)','NEIGH_10m','NEIGH_30m','DIST_CENT','DIST_CENT_CON_COMP')

# Create and save corr matrix
cor_mat <- cor(df)
cor_mat_df <- cor_mat |> as.data.frame()
cor_mat_df <- cor_mat_df |> round(digits = 2) 
cor_mat_df <- cor_mat_df[1:4,5:ncol(cor_mat_df)]
cor_mat_df|>write.csv(paste0(results_analysis_dir,'correlation_matrix.csv'))
cor_mat_df
```

Plot correlation

``` r
image_path <- paste0(results_analysis_dir,'correlation-plot.png')
png(image_path)
corrplot(cor(df),method = "circle",type = "upper", tl.col = "black", tl.srt = 45)
dev.off()
```

Correlation test

``` r
corr_p_vals <- data.frame(errors = c('LSTM_RMSE','ARIMA_RMSE','BYM30_RMSE','BYM10_RMSE'))

# Retrieve correlation test p-values between errors and features
for(feature in c('VAR','SD(MOV_VAR)','NEIGH_10m','NEIGH_30m','DIST_CENT','DIST_CENT_CON_COMP')){
  corr_p_vals[feature] <- corr_p_vals$errors |> lapply(
  function(error){
         res <- cor.test(df[[error]], df[[feature]], method = "pearson")
         res$p.value
       }
      ) |> unlist()
}

corr_p_vals

write.csv(corr_p_vals,
          file = paste0(results_analysis_dir,'corr_p_vals.csv'),
          row.names = FALSE)
```
