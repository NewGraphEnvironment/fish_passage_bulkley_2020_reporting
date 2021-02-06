##we are going to need to sub in our lenght estimates from the priorities spreadsheet but leave this for now
##issue - we are biulding the multiplier twice.  bad practice - leave for now
##this is made from extract-bcfishpass-phase2.R

source('R/0330-tables-phase1-cost-estimate.R')
##phase 2 specific
tab_cost_est_prep3 <- left_join(
  tab_cost_est_prep2,
  select(
    filter(habitat_confirmations_priorities, location == 'us'),
    site, upstream_habitat_length_m),
  by = c('pscis_crossing_id' = 'site')
)

tab_cost_est_prep4 <- left_join(
  tab_cost_est_prep3,
  select(hab_site %>% filter(!alias_local_name %like% 'ds' & !alias_local_name %like% 'ef'), site, avg_channel_width_m),
  by = c('pscis_crossing_id' = 'site')
) %>%
  mutate(cost_net = round(upstream_habitat_length_m/cost_est_1000s, 1),
         cost_area_net = round((upstream_habitat_length_m * avg_channel_width_m)/cost_est_1000s, 1)) #downstream_channel_width_meters


##add the priority info
tab_cost_est_phase2 <- tab_cost_est_prep4 %>%
  select(pscis_crossing_id, stream_name, road_name, barrier_result, habitat_value, avg_channel_width_m,
         crossing_fix_code, cost_est_1000s, upstream_habitat_length_m,
         cost_net, cost_area_net, source) %>%
  mutate(upstream_habitat_length_m = round(upstream_habitat_length_m,0))

tab_cost_est_phase2_report <- tab_cost_est_phase2 %>%
  filter(source %like% 'phase2') %>%
  rename(`PSCIS ID` = pscis_crossing_id,
         Stream = stream_name,
         Road = road_name,
         Result = barrier_result,
         `Habitat value` = habitat_value,
         `Stream Width (m)` = avg_channel_width_m,
         Fix = crossing_fix_code,
        `Cost Est (in $K)` =  cost_est_1000s,
         `Habitat Upstream (m)` = upstream_habitat_length_m,
         `Cost Benefit (m / $K)` = cost_net,
         `Cost Benefit (m2 / $K)` = cost_area_net) %>%
  select(-source)


rm(tab_cost_est_prep, tab_cost_est_prep2, tab_cost_est_prep3)

