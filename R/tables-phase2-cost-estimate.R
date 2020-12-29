##we are going to need to sub in our lenght estimates from the priorities spreadsheet but leave this for now
##issue - we are biulding the multiplier twice.  bad practice - leave for now


# make_tab_cost_est_phase2 <- function(dat = pscis2){
bcfishpass_rd <- bcfishpass_phase2 %>%
  filter(source %like% 'phase2') %>%  ##specific to phase 2
  select(pscis_crossing_id, my_crossing_reference, crossing_id, distance, road_name_full,
         road_class, road_name_full, road_surface, file_type_description, forest_file_id,
         client_name, client_name_abb, map_label, owner_name, admin_area_abbreviation,
         uphab_l_net_inf_000_030:uphab_gross_sub22) %>%
  mutate(uphab_net_sub22 = rowSums(select(., uphab_l_net_inf_000_030:uphab_l_net_obs_150_220))) %>%
  select(pscis_crossing_id:admin_area_abbreviation, uphab_gross_sub22, uphab_net_sub22)


###note that some of the rd info is not likely correct if the distance is >100m

##########-----------------this should not be in two files!!!----------------------------
pscis_rd <- left_join(
  select(pscis2, -my_crossing_reference),
  bcfishpass_rd,
  by = 'pscis_crossing_id'
) %>%
  dplyr::mutate(my_road_class = case_when(is.na(road_class) & !is.na(file_type_description) ~
                                            file_type_description,
                                          T ~ road_class)) %>%
  dplyr::mutate(my_road_class = case_when(is.na(my_road_class) & !is.na(owner_name) ~
                                            'rail',
                                          T ~ my_road_class)) %>%
  dplyr::mutate(my_road_surface = case_when(is.na(road_surface) & !is.na(file_type_description) ~
                                              'loose',
                                            T ~ road_surface)) %>%
  dplyr::mutate(my_road_surface = case_when(is.na(my_road_surface) & !is.na(owner_name) ~
                                            'rail',
                                          T ~ my_road_surface)) %>%
  distinct(.keep_all = T) ##we have multiiple entries with 2 spreadsheets put together

# test <- pscis_rd %>% filter(my_crossing_reference == 4605732)

####----tab cost multipliers for road surface-----
tab_cost_rd_mult <- pscis_rd %>%
  select(my_road_class, my_road_surface) %>%
  # mutate(road_surface_mult = NA_real_, road_class_mult = NA_real_) %>%
  mutate(road_class_mult = case_when(my_road_class == 'collector' ~ 4,
                                     my_road_class == 'arterial' ~ 10,
                                     my_road_class == 'highway' ~ 10,
                                       my_road_class == 'rail' ~ 5,
                                     T ~ 1))  %>%
  mutate(road_surface_mult = case_when(my_road_surface == 'loose' |
                                         my_road_surface == 'rough' ~
                                         1,
                                       T ~ 2)) %>%
  # mutate(road_type_mult = road_class_mult * road_surface_mult) %>%
  mutate(cost_m_1000s_bridge = road_surface_mult * road_class_mult * 12.5,
         cost_embed_cv = road_surface_mult * road_class_mult * 25) %>%
  # mutate(cost_1000s_for_10m_bridge = 10 * cost_m_1000s_bridge) %>%
  distinct( .keep_all = T) %>%
  arrange(cost_m_1000s_bridge)




##########-----------tab_cost_rd_mult should be in one file only or a function!----------##################

##make tyhe cost estimates
tab_cost_est_prep <- left_join(
  select(pscis_rd, pscis_crossing_id, my_crossing_reference, stream_name, road_name, my_road_class,
         my_road_surface, downstream_channel_width_meters, barrier_result,
         fill_depth_meters, crossing_fix, , habitat_value, recommended_diameter_or_span_meters),
  select(tab_cost_rd_mult, my_road_class, my_road_surface, cost_m_1000s_bridge, cost_embed_cv),
  by = c('my_road_class','my_road_surface')
)


tab_cost_est_prep2 <- left_join(
  tab_cost_est_prep,
  select(xref_structure_fix, crossing_fix, crossing_fix_code),
  by = c('crossing_fix')
) %>%
  mutate(cost_est_1000s = case_when(
    crossing_fix_code == 'SS-CBS' ~ cost_embed_cv,
    crossing_fix_code == 'OBS' ~ cost_m_1000s_bridge * recommended_diameter_or_span_meters)
  ) %>%
  mutate(cost_est_1000s = round(cost_est_1000s, 0))

##add in the priorities upstream habitat distance data.  This is a good reason for the data to be input first so that we can use the net distance!!
# tab_cost_est_prep3 <- left_join(
#   tab_cost_est_prep2,
#   select(bcfishpass_rd, pscis_crossing_id, uphab_gross_sub22, uphab_net_sub22),
#   by = 'pscis_crossing_id'
# ) %>%
#   mutate(cost_net = round(uphab_net_sub22/cost_est_1000s, 1),
#          cost_gross = round(uphab_gross_sub22/cost_est_1000s, 1),
#          cost_area_net = round((uphab_net_sub22 * downstream_channel_width_meters * 0.5)/cost_est_1000s, 1),
#          cost_area_gross = round((uphab_gross_sub22 * downstream_channel_width_meters * 0.5)/cost_est_1000s, 1))

##phase 2 specific
tab_cost_est_prep3 <- left_join(
  tab_cost_est_prep2,
  select(
    filter(habitat_confirmations_priorities, location == 'us'),
    site, upstream_habitat_length_m),
  by = c('pscis_crossing_id' = 'site')
) %>%
  mutate(cost_net = round(upstream_habitat_length_m/cost_est_1000s, 1),
         cost_area_net = round((upstream_habitat_length_m * downstream_channel_width_meters * 0.5)/cost_est_1000s, 1))




##add the priority info
tab_cost_est_phase2 <- tab_cost_est_prep3 %>%
  select(pscis_crossing_id, stream_name, road_name, downstream_channel_width_meters,
         crossing_fix_code, cost_est_1000s, upstream_habitat_length_m,
         cost_net, cost_area_net) %>%
  mutate(upstream_habitat_length_m = round(upstream_habitat_length_m,0))

tab_cost_est_phase2_report <- tab_cost_est_phase2 %>%
  rename(`PSCIS ID` = pscis_crossing_id,
         Stream = stream_name,
         Road = road_name,
         `Stream Width (m)` = downstream_channel_width_meters,
         Fix = crossing_fix_code,
        `Cost Est (in $K)` =  cost_est_1000s,
         `Habitat Upstream (m)` = upstream_habitat_length_m,
         `Cost Benefit (m / $K)` = cost_net,
         `Cost Benefit (m2 / $K)` = cost_area_net)

rm(tab_cost_est_prep, tab_cost_est_prep2, tab_cost_est_prep3,
   bcfishpass_rd, pscis_rd)

