##################################
## Script name: get_dgbas_xml_data.R
##
## Description: Function to parse XML files from the DGBAS database and format the retrieved data series.
##


get_dgbas_xml_data <- function(dgbas_xml_link) {
  # Read Raw XML
  data_xml_raw <- read_xml(dgbas_xml_link)
  
  # Get the attributes of each series in the dataset
  df_series <- tibble(series = xml_find_all(data_xml_raw, ".//Series") %>% xml_attr("item"),
                      unit   = xml_find_all(data_xml_raw, ".//Series") %>% xml_attr("unit"),
                      freq   = xml_find_all(data_xml_raw, ".//Series") %>% xml_attr("freq"))
  
  # Convert the data to a tibble
  data_xml_tbl <- data_xml_raw %>%
    as_list() %>%
    as_tibble() %>%
    unnest_longer(GenericData) %>%
    filter(GenericData_id == "Series") %>%
    bind_cols(df_series) %>%
    select(-GenericData_id) %>%
    unnest_longer(GenericData) %>%
    unnest_wider(GenericData) %>%
    unnest(cols = names(.)) %>%
    unnest(cols = names(.)) %>%
    type_convert() %>%
    select(series, unit, freq, period, value, note)
  
  return(data_xml_tbl)
}