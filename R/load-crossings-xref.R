##now that we have loaded our phase 1 data to pscis we need to pull out the crossing id's


source('R/packages.R')
source('R/functions.R')
source('R/tables.R')


get_this <- bcdc_tidy_resources('pscis-assessments') %>%
  filter(bcdata_available == T)  %>%
  pull(package_id)

dat <- bcdata::bcdc_get_data(get_this)

xref_pscis_my_crossing_modelled <- dat %>%
  purrr::set_names(nm = tolower(names(.))) %>%
  dplyr::filter(funding_project_number == "BCFP-003_phase1") %>%
  select(external_crossing_reference, stream_crossing_id) %>%
  mutate(external_crossing_reference = as.integer(external_crossing_reference)) %>%
  sf::st_drop_geometry()

##I think we need to add our phase1b file in
##here we find identical columns because we will want to remove them from one of the lists.
# rows_to_remove <- intersect(xref_pscis_my_crossing_modelled$external_crossing_reference, pscis$my_crossing_reference)
rows_to_keep <- setdiff(pscis$my_crossing_reference, xref_pscis_my_crossing_modelled$external_crossing_reference)  ##make sure to keep the joining column

pscis_rows_to_add <- pscis %>%
  filter(my_crossing_reference %in% rows_to_keep) %>%
  select(my_crossing_reference)

##add the new info to the xref
xref_pscis_my_crossing_modelled <- bind_rows(
  xref_pscis_my_crossing_modelled,
  pscis_rows_to_add
) %>%
  mutate(my_crossing_reference = as.integer(my_crossing_reference)) %>%
  mutate(external_crossing_reference = case_when(
    is.na(external_crossing_reference) ~ my_crossing_reference,
    T ~ external_crossing_reference
  )) %>%
  select(-my_crossing_reference)

#get our modelled crossing info
xref_bcfishpass <- readr::read_csv(file = paste0(getwd(), '/data/bcfishpass-phase2.csv')) %>%
  select(misc_point_id, crossing_id, my_crossing_reference) %>%
  mutate(my_crossing_reference = as.integer(my_crossing_reference))


##join to the pscis id's and save to a file

xref_pscis_my_crossing_modelled <- left_join(xref_pscis_my_crossing_modelled,
          xref_bcfishpass,
          by = c('external_crossing_reference' = 'my_crossing_reference')
) %>%
  rename(my_crossing_reference = external_crossing_reference) %>%
  mutate(across(everything(), as.integer)) %>%
  readr::write_csv(file = paste0(getwd(), '/data/raw_input/xref_pscis_my_crossing_modelled.csv'))




