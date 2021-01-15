##we need to rename our habitat priorities spreadsheet with the new ids from pscis

source('R/packages.R')
source('R/functions.R')
source('R/tables.R')

hab_loc <- habitat_confirmations %>%
  purrr::pluck("step_1_ref_and_loc_info") %>%
  dplyr::filter(!is.na(site_number))%>%
  mutate(survey_date = janitor::excel_numeric_to_date(as.numeric(survey_date))) %>%
  tidyr::separate(alias_local_name, into = c('site', 'location', 'fish'), remove = F) %>%
  select(site:fish) %>%
  mutate(site = as.numeric(site))

hab_site_corrected <- left_join(
  hab_loc,
  xref_pscis_my_crossing_modelled,
  by = c('site' = 'external_crossing_reference')
) %>%
  mutate(stream_crossing_id = case_when(
    is.na(stream_crossing_id) ~ site,
    T ~ stream_crossing_id
  )) %>%
  mutate(site_corrected = paste(stream_crossing_id, location, fish, sep = '_')) %>%
  mutate(site_corrected = stringr::str_replace_all(site_corrected, '_NA', '')) %>%
  tibble::rownames_to_column() %>%
  readr::write_csv(file = paste0(getwd(), '/data/extracted_inputs/hab_site_corrected.csv'))




