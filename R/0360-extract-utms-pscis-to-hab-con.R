##we need to get our utms for the hab con spreadsheet.  us crossings are the same as the pscis inputs

source('R/0320-tables-phase2.R')




utm_prep <- left_join(
  hab_loc %>% filter(alias_local_name %ilike% 'us' &
                       !alias_local_name %ilike% 'ef') %>%
    tidyr::separate(alias_local_name, into = c('site', 'location', 'ef'), remove = F) %>%
    mutate(site = as.numeric(site)),
  select(pscis_phase2, pscis_crossing_id, utm_zone, easting, northing),
  by = c('site' = 'pscis_crossing_id')
)

utm <- left_join(
  hab_loc %>%
           tidyr::separate(alias_local_name, into = c('site', 'location', 'ef'), remove = F) %>%
           mutate(site = as.numeric(site)) %>%
    select(site, location, reference_number:alias_local_name),
  select(utm_prep, site, location, utm_zone.y, easting, northing),
  by = c('site', 'location')) %>%
  mutate(easting = round(easting, 0),
         northing = round(northing, 0)) %>%
    readr::write_csv(file = paste0(getwd(), '/data/extracted_inputs/utm_inputs.csv'), na = "")


