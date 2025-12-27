##################################
## Script name: get_cbc_data.R
##
## Description: Function to create an API call to the CBC database and format the retrieved data series.
##


get_cbc_data <- function(cbc_api_base, cbc_data_series) {
  cbc_res <- fromJSON(rawToChar(GET(sprintf("%s/%s", cbc_api_base, cbc_data_series))$content))
  
  cbc_metric_units <- tibble(metric = cbc_res$data$structure$Table2$data,
                             units = as.character(str_split(cbc_res$meta$units, ", ", simplify = TRUE))) %>%
    mutate(units = str_trim(units))
  
  cbc_res_colnames <- expand_grid(varname = cbc_res$data$structure$Table1$data,
                                  cbc_metric_units) %>%
    mutate(varnum = paste0("V", row_number()+1))
  
  cbc_res_data <- cbc_res$data$dataSets %>%
    as_tibble() %>%
    pivot_longer(cols = -V1, names_to = "varnum", values_to = "value") %>%
    mutate(value = as.numeric(value)) %>%
    left_join(cbc_res_colnames, by = c("varnum")) %>%
    select(-varnum) %>%
    pivot_wider(names_from = varname, values_from = value)
  
  return(cbc_res_data)
}