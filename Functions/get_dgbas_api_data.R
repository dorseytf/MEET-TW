##################################
## Script name: get_dgbas_api_data.R
##
## Description: Function to create an API call to the DGBAS database and format the retrieved data series.
##

get_dgbas_api_data <- function(dgbas_api_call) {
  # API Call
  dgbas_res <- fromJSON(rawToChar(GET(dgbas_api_call)$content))
  
  # Get structure of results
  has_value_types <- ifelse(length(dgbas_res$data$structure$dimensions$series$values) > 1, TRUE, FALSE)
  n_value_types <- length(dgbas_res$data$structure$dimensions$series$values)
  
  if(n_value_types == 2){
    value_type <- dgbas_res$data$structure$dimensions$series$values[[1]] %>%
      rename(value_type_id = id, value_type = name) %>%
      mutate(value_type_id = as.integer(value_type_id) - 1)
    metrics <- dgbas_res$data$structure$dimensions$series$values[[2]] %>%
      rename(metric_id = id, metric = name) %>%
      mutate(metric_id = as.integer(metric_id) - 1)
    period <- dgbas_res$data$structure$dimensions$observation$values[[1]] %>%
      mutate(period_id = row_number() - 1) %>%
      rename(period = id) %>%
      select(-name)
    
    dgbas_metric_units <- expand_grid(value_type_id = value_type$value_type_id, 
                                      metric_id = metrics$metric_id, 
                                      period_id = period$period_id) %>%
      left_join(value_type, by = "value_type_id") %>%
      left_join(metrics, by = "metric_id") %>%
      left_join(period, by = "period_id") %>%
      mutate(varnum = sprintf("x%s_%s.observations.%s", value_type_id, metric_id, period_id),
             sector = "Total")
    
  } else if(n_value_types == 3){
    value_type <- dgbas_res$data$structure$dimensions$series$values[[1]] %>%
      rename(value_type_id = id, value_type = name) %>%
      mutate(value_type_id = as.integer(value_type_id) - 1)
    metrics <- dgbas_res$data$structure$dimensions$series$values[[2]] %>%
      rename(metric_id = id, metric = name) %>%
      mutate(metric_id = as.integer(metric_id) - 1)
    period <- dgbas_res$data$structure$dimensions$observation$values[[1]] %>%
      mutate(period_id = row_number() - 1) %>%
      rename(period = id) %>%
      select(-name)
    sector <- dgbas_res$data$structure$dimensions$series$values[[3]] %>%
      rename(sector_id = id, sector = name) %>%
      mutate(sector_id = as.integer(sector_id) - 1)
    
    dgbas_metric_units <- expand_grid(value_type_id = value_type$value_type_id, 
                                      metric_id = metrics$metric_id, 
                                      period_id = period$period_id,
                                      sector_id = sector$sector_id) %>%
      left_join(value_type, by = "value_type_id") %>%
      left_join(metrics, by = "metric_id") %>%
      left_join(period, by = "period_id") %>%
      left_join(sector, by = "sector_id") %>%
      mutate(varnum = sprintf("x%s_%s_%s.observations.%s", value_type_id, metric_id, sector_id, period_id))
    
  } else if(n_value_types == 1){
    metrics <- dgbas_res$data$structure$dimensions$series$values[[1]] %>%
      rename(metric_id = id, metric = name) %>%
      mutate(metric_id = as.integer(metric_id) - 1)
    period <- dgbas_res$data$structure$dimensions$observation$values[[1]] %>%
      mutate(period_id = row_number() - 1) %>%
      rename(period = id) %>%
      select(-name)
    
    dgbas_metric_units <- expand_grid(metric_id = metrics$metric_id, 
                                      period_id = period$period_id) %>%
      left_join(metrics, by = "metric_id") %>%
      left_join(period, by = "period_id") %>%
      mutate(varnum = sprintf("x%s.observations.%s", metric_id, period_id),
             value_type = "Value",
             sector = "Total")
  }
  
  # Extract data values and append headers
  dgbas_res_data <- dgbas_res$data$dataSets$series %>%
    as_tibble() %>%
    clean_names() %>%
    unnest(cols = names(.), names_sep = ".") %>%
    unnest(cols = names(.), names_sep = ".") %>%
    unnest(cols = names(.), names_sep = ".") %>%
    pivot_longer(cols = names(.), names_to = "varnum", values_to = "value") %>%
    left_join(dgbas_metric_units, by = "varnum") %>%
    select(metric, value_type, period, value)
  
  return(dgbas_res_data)
}
