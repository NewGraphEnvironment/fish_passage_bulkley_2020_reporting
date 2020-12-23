source('R/packages.R')
source('R/functions.R')
# source('R/tables.R')

pscis2 <- import_pscis(workbook_name = 'pscis_phase2.xlsm') %>%
  tibble::rownames_to_column() %>%
  arrange(pscis_crossing_id) %>%
  mutate(rowname = as.numeric(rowname))


##this is made from extract-bcfishpass-phase2.R
bcfishpass_phase2 <- readr::read_csv(file = paste0(getwd(), '/data/bcfishpass-phase2.csv'))

##this is made from extract-fwa-watershed-ltree.R
# wsheds <- sf::st_read(dsn = 'data/fishpass_mapping.gpkg', layer = 'hab_wshds_ltree')
# wsheds_up1 <- sf::st_read(dsn = 'data/fishpass_mapping.gpkg', layer = 'hab_wshds_ltree_up1')




##burned to a kml so we can easily add elevation info
# st_write(wsheds, append = TRUE, driver = 'kml', dsn = "data/raw_input/wsheds.kml")
# st_write(wsheds_up1, append = TRUE, driver = 'kml', dsn = "data/raw_input/wsheds_up1.kml")

####--------------bring in the habitat and fish data------------------
habitat_confirmations <-  readxl::excel_sheets(path = "./data/habitat_confirmations.xls") %>%
  purrr::set_names() %>%
  purrr::map(read_excel,
             path = "./data/habitat_confirmations.xls",
             .name_repair = janitor::make_clean_names) %>%
  purrr::set_names(janitor::make_clean_names(names(.))) %>%
  purrr::map(at_trim_xlsheet2) %>% #moved to functions from https://github.com/NewGraphEnvironment/altools to reduce dependencies
  purrr::map(plyr::colwise(type.convert))


hab_site_prep <-  habitat_confirmations %>%
  purrr::pluck("step_4_stream_site_data") %>%
  # tidyr::separate(local_name, into = c('site', 'location'), remove = F) %>%
  mutate(average_gradient_percent = round(average_gradient_percent * 100, 1)) %>%
  mutate_if(is.numeric, round, 1) %>%
  select(-gazetted_names:-site_number, -feature_type:-utm_method) ##remove the feature utms so they don't conflict with the site utms

hab_loc <- habitat_confirmations %>%
  purrr::pluck("step_1_ref_and_loc_info") %>%
  dplyr::filter(!is.na(site_number))%>%
  mutate(survey_date = janitor::excel_numeric_to_date(as.numeric(survey_date)))

hab_site <- left_join(
  hab_loc,
  hab_site_prep,
  by = 'reference_number'
) %>%
  tidyr::separate(alias_local_name, into = c('site', 'location'), remove = F) %>%
  mutate(site = as.numeric(site)) %>%
  dplyr::filter(!alias_local_name %like% '_ef') ##get rid of the ef sites

##summarized the fish collection information
##----this happens after it is all sorted out as per "extract-fish.R"
hab_fish_collect_prep <- habitat_confirmations %>%
  purrr::pluck("step_2_fish_coll_data") %>%
  dplyr::filter(!is.na(site_number)) %>%
  tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F) %>%
  mutate(site_id = paste0(site, location)) %>%
  distinct(site_id, species, .keep_all = T)

##prep the location info so it is ready to join to the fish data
hab_loc2 <- hab_loc %>%
  tidyr::separate(alias_local_name, into = c('site', 'location', 'ef'), remove = F) %>%
  mutate(site_id = paste0(site, location)) %>%
  filter(alias_local_name %like% 'ef1')

##join the tables together
hab_fish_collect_prep2 <- left_join(
  select(hab_loc2, reference_number, site_id, utm_zone:utm_northing),
  select(hab_fish_collect_prep, site_id, species),
  by = 'site_id'
)


##add the species code
hab_fish_codes <- habitat_confirmations %>%
  purrr::pluck("species_by_group") %>% ##changed from specie_by_common_name because BB burbot was wrong!!
  select(-step)


hab_fish_collect <- left_join(
  hab_fish_collect_prep2,
  select(hab_fish_codes, common_name, species_code),
  by = c('species' = 'common_name')
)


# hab_fish_collect_prep <- habitat_confirmations %>%
#   purrr::pluck("step_2_fish_coll_data") %>%
#   dplyr::filter(!is.na(site_number)) %>%
#   select(-gazetted_name:-site_number)

# hab_fish_indiv_prep <- habitat_confirmations %>%
#   purrr::pluck("step_3_individual_fish_data") %>%
#   dplyr::filter(!is.na(site_number)) %>%
#   select(-gazetted_names:-site_number)



# hab_fish_indiv_prep2 <- left_join(
#   hab_fish_indiv_prep,
#   hab_loc,
#   by = 'reference_number'
# )
#
# hab_fish_collect_prep2 <- left_join(
#   hab_fish_collect_prep,
#   hab_loc,
#   by = 'reference_number'
# )


# hab_fish_indiv <- left_join(
#   hab_fish_indiv_prep2,
#   select(hab_fish_codes, common_name:species_code),
#   by = c('species' = 'common_name')
# ) %>%
#   dplyr::distinct(alias_local_name, utm_zone, utm_easting, utm_northing, species_code)

# hab_fish_collect <- left_join(
#   hab_fish_collect_prep2,
#   select(hab_fish_codes, common_name:species_code),
#   by = c('species' = 'common_name')
# ) %>%
#   dplyr::distinct(reference_number, alias_local_name, site_number, utm_zone, utm_easting, utm_northing, species_code)


hab_features <- habitat_confirmations %>%
  purrr::pluck("step_4_stream_site_data") %>%
  select(reference_number,local_name, feature_type:utm_northing) %>%
  filter(!is.na(feature_type))


####--------import priorities spreadsheet--------------
habitat_confirmations_priorities <- readxl::read_excel(
  path = "./data/habitat_confirmations_priorities.xlsx",
  .name_repair = janitor::make_clean_names) %>%
  filter(!local_name %like% 'ef') %>% ##ditch the ef sites
  tidyr::separate(local_name, into = c('site', 'location'), remove = F) %>%
  mutate(site = as.numeric(site),
         upstream_habitat_length_km = round(upstream_habitat_length_m/1000,1))


##add the priorities to the site data
hab_site_priorities <- left_join(
  select(habitat_confirmations_priorities, reference_number, priority),
  select(hab_site, reference_number, alias_local_name, site, utm_zone:utm_northing),
  by = 'reference_number'
) %>%
  filter(!is.na(priority))


##clean up the objects
rm(hab_site_prep,
   # hab_fish_indiv_prep,
   # hab_fish_indiv_prep2,
   hab_fish_collect_prep,
   hab_fish_collect_prep2,
   hab_loc2)


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

