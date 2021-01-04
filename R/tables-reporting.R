# source('R/packages.R')
# source('R/functions.R')
# source('R/tables.R')
# source('R/tables-phase2-cost-estimate.R')
# source('R/tables-phase2.R')

##these orignally had modelled rather than pscis ids
# xref_pscis_my_crossing_modelled %>%
#   filter(my_crossing_reference %in% c(4605732, 4600070, 4600183))

##summary table for the culvert status

####-----------overview table------------

tab_overview_prep1 <- pscis2 %>%
  select(pscis_crossing_id, stream_name, road_name, road_tenure, easting, northing, habitat_value)

tab_overview_prep2 <- habitat_confirmations_priorities %>%
  filter(location == 'us') %>%
  select(site, species_codes, upstream_habitat_length_m, priority, comments) %>%
  mutate(upstream_habitat_length_km = round(upstream_habitat_length_m/1000,1))

tab_overview <- left_join(
  tab_overview_prep1,
  tab_overview_prep2,
  by = c('pscis_crossing_id' = 'site')
) %>%
  mutate(utm = paste0(easting, ' ', northing)) %>%
  select(Site = pscis_crossing_id,
         Stream = stream_name,
         Road = road_name,
         Tenure = road_tenure,
         `UTM (11U)` = utm,
         `Fish Species` = species_codes,
         `Habitat Gain (km)` = upstream_habitat_length_km,
         `Habitat Value` = habitat_value,
         Priority = priority,
         Comments = comments )
  # mutate(test = paste0('[', Site, ']', '(Appendix 1 - Site Assessment Data and Photos)'))##hmm.. thought this worked
# %>%
#   replace(., is.na(.), "-")


rm(tab_overview_prep1, tab_overview_prep2)

####---------habitat summary--------------------------------

tab_hab_summary <- left_join(
  hab_site %>%
  select(site, location, avg_channel_width_m, avg_wetted_width_m,
         average_residual_pool_depth_m, average_gradient_percent, total_cover),

  habitat_confirmations_priorities %>%
    select(site, location, survey_length_m, hab_value),

  by = c('site', 'location')
) %>%
  mutate(location = case_when(
    location == 'us' ~ 'Upstream',
    T ~ 'Downstream'
  )) %>%
  arrange(site) %>%
  select(Site = site,
         Location = location,
         `Length Surveyed (m)` = survey_length_m,
         `Channel Width (m)` = avg_channel_width_m,
         `Wetted Width (m)` = avg_wetted_width_m,
         `Pool Depth (m)` = average_residual_pool_depth_m,
         `Gradient (%)` = average_gradient_percent,
         `Total Cover` = total_cover,
         `Habitat Value` = hab_value) %>%
  replace(., is.na(.), "-")


##we need an sf object with details for the interactive map
##prep the location data
hab_loc_prep <- left_join(
  hab_loc %>%
  tidyr::separate(alias_local_name, into = c('site', 'location', 'ef'), remove = F) %>%
  filter(!alias_local_name %ilike% 'ef' &
           alias_local_name %ilike% 'us') %>%
  mutate(site = as.integer(site)),
  select(filter(habitat_confirmations_priorities, location == 'us'),
         site, priority, comments),
  by = 'site'
)



tab_hab_map <- left_join(
  tab_cost_est_phase2,
  select(
    hab_loc_prep, site, priority, utm_easting, utm_northing, comments),
  by = c('pscis_crossing_id' = 'site')
) %>%
  sf::st_as_sf(coords = c("utm_easting", "utm_northing"),
               crs = 26911, remove = F) %>%
  sf::st_transform(crs = 4326)

tab_phase1_map_prep <- left_join(
  phase1_priorities,
  select(pscis, my_crossing_reference, stream_name, road_name),
  by = 'my_crossing_reference'
)

tab_phase1_map <- tab_phase1_map_prep %>%
  mutate(pscis_crossing_id = as.character(pscis_crossing_id),
         my_crossing_reference = as.character(my_crossing_reference)) %>%
  mutate(ID = case_when(
    !is.na(pscis_crossing_id) ~ pscis_crossing_id,
    T ~ paste0('*', my_crossing_reference
    ))) %>%
  sf::st_as_sf(coords = c("utm_easting", "utm_northing"),
               crs = 26911, remove = F) %>%
  sf::st_transform(crs = 4326) %>%
  mutate(priority_phase1 = case_when(priority_phase1 == 'mod' ~ 'moderate',
                                     T ~ priority_phase1)) %>%
  dplyr::mutate(link = paste0('<a href =',
    'https://github.com/NewGraphEnvironment/fish_passage_elk_2020_reporting_cwf/tree/master/data/photos/', my_crossing_reference,
    '/crossing_all.JPG', '>', 'Link', '</a>'))

rm(tab_phase1_map_prep)
