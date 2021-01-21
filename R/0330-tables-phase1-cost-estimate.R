source('R/packages.R')
source('R/functions.R')
source('R/0310-tables.R')
# source('R/tables-phase2.R')

# bcfishpass <- readr::read_csv(file = paste0(getwd(), '/data/extracted_inputs/bcfishpass.csv')) %>%
#   filter(!source %like% 'phase2')

#
# pscis_rd_phase1_reassessments <- pscis_rd %>%
#   filter(!source %ilike% 'phase2')

##make tyhe cost estimates
tab_cost_est_prep <- left_join(
  pscis_rd,
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

##add in the model data.  This is a good reason for the data to be input first so that we can use the net distance!!
tab_cost_est_prep3 <- left_join(
  tab_cost_est_prep2,
  select(bcfishpass_rd, pscis_crossing_id, my_crossing_reference, steelhead_network_km, steelhead_belowupstrbarriers_network_km),
  by = c('pscis_crossing_id', 'my_crossing_reference')
) %>%
  mutate(cost_net = round(steelhead_belowupstrbarriers_network_km * 1000/cost_est_1000s, 1),
         cost_gross = round(steelhead_network_km * 1000/cost_est_1000s, 1),
         cost_area_net = round((steelhead_belowupstrbarriers_network_km * 1000 * downstream_channel_width_meters * 0.5)/cost_est_1000s, 1), ##this is a triangle area!
         cost_area_gross = round((steelhead_network_km * 1000 * downstream_channel_width_meters * 0.5)/cost_est_1000s, 1)) ##this is a triangle area!

# # ##add the xref stream_crossing_id
# tab_cost_est_prep4 <- left_join(
#   tab_cost_est_prep3,
#   xref_pscis_my_crossing_modelled,
#   by = c('my_crossing_reference' = 'external_crossing_reference')
# ) %>%
#   mutate(stream_crossing_id = case_when(
#     is.na(stream_crossing_id) ~ pscis_crossing_id,
#     T ~ stream_crossing_id
#   ))

##add the priority info
tab_cost_est_phase1 <- left_join(
  select(phase1_priorities, pscis_crossing_id, priority_phase1), #my_crossing_reference
  tab_cost_est_prep3,
  by = 'pscis_crossing_id'
) %>%
  mutate(steelhead_network_km = round(steelhead_network_km,2)) %>%
  arrange(pscis_crossing_id) %>%
  select(pscis_crossing_id, stream_name, road_name, barrier_result, habitat_value, downstream_channel_width_meters, priority_phase1,
         crossing_fix_code, cost_est_1000s, steelhead_network_km,
         cost_gross, cost_area_gross, source)

# too_far_away <- tab_cost_est %>% filter(distance > 100) %>% ##after review all crossing match!!!!! Baren rail is the hwy but that is fine. added source, distance, crossing_id above
#   filter(source %like% 'phase2')

tab_cost_est_phase1_report <- tab_cost_est_phase1 %>%
  rename(
    `PSCIS ID` = pscis_crossing_id,
    Priority = priority_phase1,
    Stream = stream_name,
    Road = road_name,
    Result = barrier_result,
    `Habitat value` = habitat_value,
    `Stream Width (m)` = downstream_channel_width_meters,
    Fix = crossing_fix_code,
    `Cost Est ( $K)` =  cost_est_1000s,
    `Habitat Upstream (km)` = steelhead_network_km,
    `Cost Benefit (m / $K)` = cost_gross,
    `Cost Benefit (m2 / $K)` = cost_area_gross) %>%
  filter(!is.na(Priority))
  # st_drop_geometry()  ##added this


tab_cost_est_phase1 <- tab_cost_est_phase1_report %>%
  filter(!source %like% 'phase2') %>%
  select(-source)


##clean up workspace
rm(tab_cost_est_prep3)
