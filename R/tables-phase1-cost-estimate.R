
## we need to screen out the crossings that are not matched well

# make_tab_cost_est_phase1 <- function(dat = pscis){
bcfishpass_rd <- bcfishpass_phase2 %>%
  select(pscis_crossing_id, my_crossing_reference, crossing_id, distance, road_name_full,
         road_class, road_name_full, road_surface, file_type_description, forest_file_id,
         client_name, client_name_abb, map_label, owner_name, admin_area_abbreviation,
         uphab_l_net_inf_000_030:uphab_gross_sub22, distance) %>%
  mutate(uphab_net_sub22 = rowSums(select(., uphab_l_net_inf_000_030:uphab_l_net_obs_150_220))) %>%
  filter(distance < 100) %>%
  select(my_crossing_reference:admin_area_abbreviation, uphab_gross_sub22, uphab_net_sub22)

###note that some of the rd info is not likely correct if the distance is >100m
pscis_rd <- left_join(
  pscis,
  bcfishpass_rd,
  by = 'my_crossing_reference'
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
                                          T ~ my_road_surface))

# test <- pscis_rd %>% filter(my_crossing_reference == 4605732)

####----tab cost multipliers for road surface-----
tab_cost_rd_mult <- pscis_rd %>%
  select(my_road_class, my_road_surface) %>%
  # mutate(road_surface_mult = NA_real_, road_class_mult = NA_real_) %>%
  mutate(road_class_mult = case_when(my_road_class == 'local' ~ 4,
                                     my_road_class == 'collector' ~ 4,
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
  arrange(cost_m_1000s_bridge, my_road_class)
  # readr::write_csv(file = paste0(getwd(), '/data/raw_input/tab_cost_rd_mult.csv')) %>%
# kable() %>%
# kable_styling(latex_options = c("striped", "scale_down")) %>%
# kableExtra::save_kable("fig/tab_cost_rd_mult.png")


####-----------report table--------------------
tab_cost_rd_mult_report <- tab_cost_rd_mult %>%
  rename(
    Class = my_road_class,
    Surface = my_road_surface,
    `Class Multiplier` = road_class_mult,
    `Surface Multiplier` = road_surface_mult,
    `Bridge $K/m` = cost_m_1000s_bridge,
    `Streambed Simulation $K` = cost_embed_cv
  ) %>%
  filter(!is.na(Class)) %>%
  mutate(Class = stringr::str_to_title(Class),
         Surface = stringr::str_to_title(Surface)
         )



##make tyhe cost estimates
tab_cost_est_prep <- left_join(
  select(pscis_rd, my_crossing_reference, stream_name, road_name, my_road_class,
         my_road_surface, downstream_channel_width_meters, barrier_result,
         fill_depth_meters, crossing_fix, , habitat_value, recommended_diameter_or_span_meters),
  select(tab_cost_rd_mult, my_road_class, my_road_surface, cost_m_1000s_bridge, cost_embed_cv),
  by = c('my_road_class','my_road_surface')
)

# test <- tab_cost_est_prep %>% filter(my_crossing_reference == 4605732)


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

##add in the model data.  This is a good reason for the data to be input first so that we can use the net distance!!
tab_cost_est_prep3 <- left_join(
  tab_cost_est_prep2,
  select(bcfishpass_rd, my_crossing_reference, uphab_gross_sub22, uphab_net_sub22),
  by = 'my_crossing_reference'
) %>%
  mutate(cost_net = round(uphab_net_sub22/cost_est_1000s, 1),
         cost_gross = round(uphab_gross_sub22/cost_est_1000s, 1),
         cost_area_net = round((uphab_net_sub22 * downstream_channel_width_meters * 0.5)/cost_est_1000s, 1),
         cost_area_gross = round((uphab_gross_sub22 * downstream_channel_width_meters * 0.5)/cost_est_1000s, 1))

##add the priority info
tab_cost_est <- left_join(
  tab_cost_est_prep3,
  select(phase1_priorities, my_crossing_reference, priority_phase1),
  by = 'my_crossing_reference'
) %>%
  select(my_crossing_reference, stream_name, road_name, downstream_channel_width_meters, priority_phase1,
         crossing_fix_code, cost_est_1000s, uphab_net_sub22,
         cost_net, cost_area_net) %>%
  mutate(uphab_net_sub22 = round(uphab_net_sub22,0)) %>%
  rename(
         Priority = priority_phase1,
         Stream = stream_name,
         Road = road_name,
         `Stream Width (m)` = downstream_channel_width_meters,
         Fix = crossing_fix_code,
        `Cost Est ( $K)` =  cost_est_1000s,
         `Habitat Upstream (m)` = uphab_net_sub22,
         `Cost Benefit (m / $K)` = cost_net,
         `Cost Benefit (m2 / $K)` = cost_area_net) %>%
  filter(!is.na(Priority))

tab_cost_est_phase1 <- left_join(
  tab_cost_est,
  select(bcfishpass_phase2, my_crossing_reference, stream_crossing_id),
  by = 'my_crossing_reference'
) %>%
  select(`PSCIS ID` = stream_crossing_id, everything(), -my_crossing_reference) %>%
  arrange(`PSCIS ID`)


##clean up workspace
rm(ab_cost_est, tab_cost_est_prep, tab_cost_est_prep2, tab_cost_est_prep3,
   bcfishpass_rd, pscis_rd)
