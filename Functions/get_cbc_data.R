##################################
## Script name: get_cbc_data.R
##
## Description: Function to create an API call to the CBC database and format the retrieved data series.
##


get_cbc_data <- function(cbc_api_base, cbc_data_series_id) {
  cbc_res <- fromJSON(rawToChar(GET(sprintf("%s/%s", cbc_api_base, cbc_data_series_id))$content))
  
  nmetrics <- length(cbc_res$data$structure$Table2$data)
  n_structures <- length(cbc_res$data$structure)
  
  if(nmetrics == 2){ # Only 2 values for metric
    if(n_structures == 4){ # if the structure includes sectors as well as metric
      cbc_metric_units <- expand_grid(metric = cbc_res$data$structure$Table2$data,
                                      units  = as.character(str_split(cbc_res$meta$units, ", ", simplify = TRUE)),
                                      sector = cbc_res$data$structure$Table3$data) %>%
        mutate(metric = str_trim(metric),
               units  = str_trim(units),
               sector = str_trim(sector))
    } else{ # structure does not include sector breakdown
      cbc_metric_units <- tibble(metric = cbc_res$data$structure$Table2$data,
                                 units = as.character(str_split(cbc_res$meta$units, ", ", simplify = TRUE))) %>%
        mutate(metric = str_trim(metric),
               units = str_trim(units))
    }
    
  } else if (nmetrics > 2){
    cbc_metric_units <- expand_grid(metric = cbc_res$data$structure$Table2$data,
                                    units = as.character(str_split(cbc_res$meta$units, ", ", simplify = TRUE))) %>%
      mutate(metric = str_trim(metric),
             units = str_trim(units))
  } else if (nmetrics < 2){
    cbc_metric_units <- expand_grid(metric = "Amount",
                                    units = as.character(str_split(cbc_res$meta$units, ", ", simplify = TRUE))) %>%
      mutate(metric = str_trim(metric),
             units = str_trim(units))
  }
  
  cbc_res_colnames <- expand_grid(varname = cbc_res$data$structure$Table1$data,
                                  cbc_metric_units) %>%
    mutate(varnum = paste0("V", row_number()+1),
           varname = str_trim(varname))
  
  cbc_res_data <- cbc_res$data$dataSets %>%
    as_tibble() %>%
    pivot_longer(cols = -V1, names_to = "varnum", values_to = "value") %>%
    mutate(value = as.numeric(value)) %>%
    left_join(cbc_res_colnames, by = c("varnum")) %>%
    select(-varnum) %>%
    pivot_wider(names_from = varname, values_from = value) %>%
    rename(period = V1) %>%
    mutate(data_series_title = cbc_res$meta$title) %>%
    relocate(data_series_title, .after = "period")
  
  return(cbc_res_data)
}