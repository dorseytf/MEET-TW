##################################
## Script name: get_dgbas_api_data.R
##
## Description: Function to create an API call to the DGBAS database and format the retrieved data series.
##

get_dgbas_api_data <- function(dgbas_api_call) {
  dgbas_res <- fromJSON(rawToChar(GET(dgbas_api_call)$content))
  
  nmetrics <- length(cbc_res$data$structure$Table2$data)
  n_structures <- length(cbc_res$data$structure)
  
  
  
}
