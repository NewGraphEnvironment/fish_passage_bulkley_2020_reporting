##now that we have loaded our phase 1 data to pscis we need to pull out the crossing id's
##this is probably better achieved by pulling from postgres after loading with 00240-load-bcdata.R
##but we will leave as is for now since it is done


source('R/packages.R')
source('R/functions.R')


get_this <- bcdc_tidy_resources('pscis-assessments') %>%
  filter(bcdata_available == T)  %>%
  pull(package_id)

dat <- bcdata::bcdc_get_data(get_this)

##grab the pscis id's and burn to file
xref_pscis_my_crossing_modelled <- dat %>%
  purrr::set_names(nm = tolower(names(.))) %>%
  dplyr::filter(funding_project_number == "Bulkley_6-288_Phase1") %>% ##we don't need these - funding_project_number == "Bulkley_6-288_Reassessments"
  select(external_crossing_reference, stream_crossing_id) %>%
  mutate(external_crossing_reference = as.integer(external_crossing_reference)) %>%
  sf::st_drop_geometry() %>%
  readr::write_csv(file = paste0(getwd(), '/data/extracted_inputs/xref_pscis_my_crossing_modelled.csv'))

