# source('R/packages.R')
# source('R/functions.R')
# source('R/0255-load-pscis.R')
source('R/0310-tables.R')

# pscis2 <- import_pscis(workbook_name = 'pscis_phase2.xlsm') %>%
#   tibble::rownames_to_column() %>%
#   arrange(pscis_crossing_id) %>%
#   mutate(rowname = as.numeric(rowname))


# ##this is made from extract-bcfishpass-phase2.R
# bcfishpass_phase2 <- readr::read_csv(file = paste0(getwd(), '/data/bcfishpass-phase2.csv'))

##this is made from extract-fwa-watershed-ltree.R
# wsheds <- sf::st_read(dsn = 'data/fishpass_mapping.gpkg', layer = 'hab_wshds_ltree')
# wsheds_up1 <- sf::st_read(dsn = 'data/fishpass_mapping.gpkg', layer = 'hab_wshds_ltree_up1')



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
# hab_fish_collect_prep <- habitat_confirmations %>%
#   purrr::pluck("step_2_fish_coll_data") %>%
#   dplyr::filter(!is.na(site_number)) %>%
#   tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F) %>%
#   mutate(site_id = paste0(site, location)) %>%
#   distinct(site_id, species, .keep_all = T) %>%
#   mutate(across(c(date_in,date_out), janitor::excel_numeric_to_date)) %>%
#   mutate(across(c(time_in,time_out), chron::times))


hab_fish_collect_prep <- habitat_confirmations %>%
  purrr::pluck("step_2_fish_coll_data") %>%
  dplyr::filter(!is.na(site_number)) %>%
  tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F) %>%
  mutate(site_id = paste0(site, location)) %>%
  distinct(local_name, species, .keep_all = T) %>% ##changed this to make it work as a feed for the extract-fish.R file
  mutate(across(c(date_in,date_out), janitor::excel_numeric_to_date)) %>%
  mutate(across(c(time_in,time_out), chron::times))


##prep the location info so it is ready to join to the fish data
hab_loc2 <- hab_loc %>%
  tidyr::separate(alias_local_name, into = c('site', 'location', 'ef'), remove = F) %>%
  mutate(site_id = paste0(site, location)) %>%
  filter(alias_local_name %like% 'ef') ##changed this from ef1

##join the tables together
hab_fish_collect_prep2 <- left_join(
  select(hab_loc2, reference_number, site_id, utm_zone:utm_northing),
  select(hab_fish_collect_prep %>% distinct(site_id, species, .keep_all = T), site_id, species),
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


hab_fish_collect_prep1 <- habitat_confirmations %>%
  purrr::pluck("step_2_fish_coll_data") %>%
  dplyr::filter(!is.na(site_number)) %>%
  select(-gazetted_name:-site_number)

hab_fish_indiv_prep <- habitat_confirmations %>%
  purrr::pluck("step_3_individual_fish_data") %>%
  dplyr::filter(!is.na(site_number)) %>%
  select(-gazetted_names:-site_number)



hab_fish_indiv_prep2 <- left_join(
  hab_fish_indiv_prep,
  hab_loc,
  by = 'reference_number'
)

hab_fish_collect_prep2 <- left_join(
  hab_fish_collect_prep1,
  hab_loc,
  by = 'reference_number'
)


hab_fish_indiv <- left_join(
  hab_fish_indiv_prep2,
  select(hab_fish_codes, common_name:species_code),
  by = c('species' = 'common_name')
) %>%
  dplyr::distinct(alias_local_name, utm_zone, utm_easting, utm_northing, species_code)

hab_fish_collect <- left_join(
  hab_fish_collect_prep2,
  select(hab_fish_codes, common_name:species_code),
  by = c('species' = 'common_name')
) %>%
  dplyr::distinct(reference_number, alias_local_name, site_number, utm_zone, utm_easting, utm_northing, species_code)


hab_features <- left_join(
  habitat_confirmations %>%
  purrr::pluck("step_4_stream_site_data") %>%
  select(reference_number,local_name, feature_type:utm_northing) %>%
  filter(!is.na(feature_type)),

  xref_obstacle_names,

  by = c('feature_type' = 'spreadsheet_feature_type')
)

##add the priorities to the site data
hab_site_priorities <- left_join(
  select(habitat_confirmations_priorities, reference_number, local_name, priority),
  select(hab_site, reference_number, alias_local_name, site, utm_zone:utm_northing),
  by = 'reference_number'
) %>%
  filter(!local_name %like% '_ds') %>%
  select(-local_name)
  # filter(!is.na(priority))  ##this is how we did it before.  changed it to get a start on it


##clean up the objects
rm(hab_site_prep,
   # hab_fish_indiv_prep,
   # hab_fish_indiv_prep2,
   hab_fish_collect_prep2,
   hab_loc2)


##these orignally had modelled rather than pscis ids
# xref_pscis_my_crossing_modelled %>%
#   filter(my_crossing_reference %in% c(4605732, 4600070, 4600183))


# hab_fish_sum <- habitat_confirmations %>%
#   purrr::pluck("step_3_individual_fish_data") %>%
#   dplyr::filter(!is.na(site_number))
#   select(-gazetted_names:-site_number)
