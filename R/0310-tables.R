source('R/packages.R')
source('R/functions.R')
source('R/0255-load-pscis.R')

##this isa remnant fie from 0280-extract-bcfishpass.R
bcfishpass <- readr::read_csv(file = paste0(getwd(), '/data/extracted_inputs/bcfishpass.csv'))

# bcfishpass_phase2 <- bcfishpass %>%
#   filter(source %like% 'phase2')


##this is our new db made from 0282-extract-bcfishpass2-crossing-corrections.R and 0290
conn <- rws_connect("data/bcfishpass.sqlite")
bcfishpass_phase2 <- readwritesqlite::rws_read_table("bcfishpass_morr_bulk", conn = conn) %>%
  filter(stream_crossing_id %in% (pscis_phase2 %>% pull(pscis_crossing_id))) %>%
  mutate(downstream_route_measure = as.integer(downstream_route_measure))
bcfishpass_all <- readwritesqlite::rws_read_table("bcfishpass_morr_bulk", conn = conn) %>%
  mutate(downstream_route_measure = as.integer(downstream_route_measure))
# bcfishpass_archived <- readwritesqlite::rws_read_table("bcfishpass_morr_bulk_archive", conn = conn) %>%
#   mutate(downstream_route_measure = as.integer(downstream_route_measure))
pscis_historic_phase1 <- readwritesqlite::rws_read_table("pscis_historic_phase1", conn = conn)
bcfishpass_spawn_rear_model <- readwritesqlite::rws_read_table("bcfishpass_spawn_rear_model", conn = conn)
fiss_sum <- readwritesqlite::rws_read_table("fiss_sum", conn = conn)
rws_disconnect(conn)


bcfishpass_rd <- bcfishpass %>%
  select(pscis_crossing_id, my_crossing_reference, crossing_id, distance, road_name_full,
         road_class, road_name_full, road_surface, file_type_description, forest_file_id,
         client_name, client_name_abb, map_label, owner_name, admin_area_abbreviation,
         steelhead_network_km, steelhead_belowupstrbarriers_network_km, distance) %>%
  # filter(distance < 100) %>% ## we need to screen out the crossings that are not matched well
  select(pscis_crossing_id, my_crossing_reference:admin_area_abbreviation, steelhead_network_km, steelhead_belowupstrbarriers_network_km)

####----tab cost multipliers for road surface-----
tab_cost_rd_mult <- readr::read_csv(file = paste0(getwd(), '/data/extracted_inputs/tab_cost_rd_mult.csv'))


####-----------report table--------------------
tab_cost_rd_mult_report <- tab_cost_rd_mult %>%
  mutate(cost_m_1000s_bridge = cost_m_1000s_bridge * 10) %>%
  rename(
    Class = my_road_class,
    Surface = my_road_surface,
    `Class Multiplier` = road_class_mult,
    `Surface Multiplier` = road_surface_mult,
    `Bridge $K/10m` = cost_m_1000s_bridge,
    `Streambed Simulation $K` = cost_embed_cv
  ) %>%
  filter(!is.na(Class)) %>%
  mutate(Class = stringr::str_to_title(Class),
         Surface = stringr::str_to_title(Surface)
  )

###note that some of the rd info is not likely correct if the distance is >100m
pscis_rd <- readr::read_csv(file = paste0(getwd(), '/data/extracted_inputs/pscis_rd.csv'))
  # filter(distance < 100)

####--------import priorities spreadsheet--------------
# habitat_confirmations_priorities <- readxl::read_excel(
#   path = "./data/habitat_confirmations_priorities.xlsx",
#   .name_repair = janitor::make_clean_names) %>%
#   filter(!local_name %like% 'ef') %>% ##ditch the ef sites
#   tidyr::separate(local_name, into = c('site', 'location'), remove = F) %>%
#   mutate(site = as.numeric(site),
#          upstream_habitat_length_km = round(upstream_habitat_length_m/1000,1))

habitat_confirmations_priorities <- readr::read_csv(
  file = "./data/habitat_confirmations_priorities.csv") %>%
  filter(!local_name %like% 'ef') %>% ##ditch the ef sites
  tidyr::separate(local_name, into = c('site', 'location'), remove = F) %>%
  mutate(site = as.numeric(site),
         upstream_habitat_length_km = round(upstream_habitat_length_m/1000,1))

####--------------bring in the habitat and fish data------------------
habitat_confirmations <-  readxl::excel_sheets(path = "./data/habitat_confirmations.xls") %>%
  purrr::set_names() %>%
  purrr::map(read_excel,
             path = "./data/habitat_confirmations.xls",
             .name_repair = janitor::make_clean_names) %>%
  purrr::set_names(janitor::make_clean_names(names(.))) %>%
  purrr::map(at_trim_xlsheet2) %>% #moved to functions from https://github.com/NewGraphEnvironment/altools to reduce dependencies
  purrr::map(plyr::colwise(type.convert))


#------------------make the tables for the methods----------
tab_habvalue <- tibble::tibble(`Habitat Value` = c('High', 'Medium', 'Low'),
                                `Fish Habitat Criteria` = c(
                                  'The presence of high value spawning or rearing habitat (e.g., locations with abundance of suitably sized gravels, deep pools, undercut banks, or stable debris) which are critical to the fish population.',
                                  'Important migration corridor. Presence of suitable spawning habitat. Habitat with moderate rearing potential for the fish species present.', 'No suitable spawning habitat, and habitat with low rearing potential (e.g., locations without deep pools, undercut banks, or stable debris, and with little or no suitably sized spawning gravels for the fish species present).'
                                )
)




tab_barrier_scoring <- dplyr::tribble(
  ~Risk,   ~Embedded,                                 ~Value,  ~`Outlet Drop (cm)`, ~Value, ~SWR,    ~Value, ~`Slope (%)`, ~Value, ~`Length (m)`, ~Value,
  "LOW",  ">30cm or >20% of diameter and continuous",  "0",       "<15",              '0',  '<1.0',    '0',      '<1',      '0',     '<15',         '0',
  "MOD",  "<30cm or 20% of diameter but continuous",   "5",       "15-30",            '5',  '1.0-1.3', '3',      '1-3',     '5',     '15-30',       '3',
  "HIGH", "No embedment or discontinuous",             "10",      ">30",             '10',  '>1.3',    '6',       '>3',     '10',    '>30',         '6',
)
  # kable() %>%
  # kable_styling(latex_options = c("striped", "scale_down")) %>%
  # kableExtra::save_kable("fig/tab_barrier_scoring.png")

tab_barrier_result <- dplyr::tribble(
  ~`Cumlative Score`, ~Result,
  '0-14',             'passable',
  '15-19',            'potential barrier',
  '>20',              'barrier'
)

##workflows to create these tables can be found at https://github.com/NewGraphEnvironment/fish_passage_elk_2020_reporting_cwf/blob/master/R/tables.R


####---------make a table to cross reference column names for ---------------
xref_names <- tibble::tribble(
                          ~bcdata,                               ~spdsht,                 ~report, ~id_join, ~id_side,
                             "id",                                    NA,                      NA,       NA,       NA,
         "funding_project_number",                                    NA,                      NA,       NA,       NA,
                "funding_project",                                    NA,                      NA,       NA,       NA,
                     "project_id",                                    NA,                      NA,       NA,       NA,
                 "funding_source",                                    NA,                      NA,       NA,       NA,
         "responsible_party_name",                                    NA,                      NA,       NA,       NA,
                "consultant_name",                                    NA,                      NA,       NA,       NA,
                "assessment_date",                                "date",                  "Date",       1L,       1L,
             "stream_crossing_id",                   "pscis_crossing_id",              "PSCIS ID",       2L,       1L,
                  "assessment_id",                                    NA,                      NA,       NA,       NA,
    "external_crossing_reference",               "my_crossing_reference",           "External ID",       3L,       1L,
                   "crew_members",                        "crew_members",                  "Crew",       5L,       1L,
                       "utm_zone",                            "utm_zone",              "UTM Zone",       6L,       1L,
                    "utm_easting",                             "easting",               "Easting",       7L,       1L,
                   "utm_northing",                            "northing",              "Northing",       8L,       1L,
        "location_confidence_ind",                                    NA,                      NA,       NA,       NA,
                    "stream_name",                         "stream_name",                "Stream",       9L,       1L,
                      "road_name",                           "road_name",                  "Road",      10L,       1L,
                   "road_km_mark",                        "road_km_mark",                      NA,       NA,       NA,
                    "road_tenure",                         "road_tenure",           "Road Tenure",      11L,       1L,
             "crossing_type_code",                       "crossing_type",         "Crossing Type",       NA,       NA,
             "crossing_type_desc",                                    NA,                      NA,       NA,       NA,
          "crossing_subtype_code",                    "crossing_subtype",     "Crossing Sub Type",       1L,       2L,
          "crossing_subtype_desc",                                    NA,                      NA,       NA,       NA,
               "diameter_or_span",             "diameter_or_span_meters",          "Diameter (m)",       2L,       2L,
                "length_or_width",              "length_or_width_meters",            "Length (m)",       3L,       2L,
    "continuous_embeddedment_ind",      "continuous_embeddedment_yes_no",              "Embedded",       5L,       2L,
     "average_depth_embededdment",   "average_depth_embededdment_meters",    "Depth Embedded (m)",       6L,       2L,
           "resemble_channel_ind",             "resemble_channel_yes_no",      "Resemble Channel",       7L,       2L,
                "backwatered_ind",                  "backwatered_yes_no",           "Backwatered",       8L,       2L,
         "percentage_backwatered",              "percentage_backwatered",   "Percent Backwatered",       9L,       2L,
                     "fill_depth",                   "fill_depth_meters",        "Fill Depth (m)",      10L,       2L,
                    "outlet_drop",                  "outlet_drop_meters",       "Outlet Drop (m)",      11L,       2L,
              "outlet_pool_depth",             "outlet_pool_depth_0_01m", "Outlet Pool Depth (m)",      12L,       2L,
                 "inlet_drop_ind",                   "inlet_drop_yes_no",            "Inlet Drop",      13L,       2L,
                  "culvert_slope",               "culvert_slope_percent",             "Slope (%)",      14L,       2L,
       "downstream_channel_width",     "downstream_channel_width_meters",     "Channel Width (m)",      12L,       1L,
                   "stream_slope",                        "stream_slope",      "Stream Slope (%)",      13L,       1L,
            "beaver_activity_ind",              "beaver_activity_yes_no",       "Beaver Activity",      14L,       1L,
              "fish_observed_ind",                "fish_observed_yes_no",          "Fish Sighted",       NA,       NA,
               "valley_fill_code",                         "valley_fill",           "Valley Fill",      15L,       2L,
          "valley_fill_code_desc",                                    NA,                      NA,       NA,       NA,
             "habitat_value_code",                       "habitat_value",         "Habitat Value",      15L,       1L,
             "habitat_value_desc",                                    NA,                      NA,       NA,       NA,
             "stream_width_ratio",                  "stream_width_ratio",                   "SWR",       NA,       NA,
       "stream_width_ratio_score",                                    NA,                 "Score",       NA,       NA,
           "culvert_length_score",                "culvert_length_score",                 "Score",       NA,       NA,
                    "embed_score",                         "embed_score",                 "Score",       NA,       NA,
              "outlet_drop_score",                   "outlet_drop_score",                 "Score",       NA,       NA,
            "culvert_slope_score",                 "culvert_slope_score",                 "Score",       NA,       NA,
                    "final_score",                         "final_score",           "Final score",      16L,       1L,
            "barrier_result_code",                      "barrier_result",        "Barrier Result",      16L,       2L,
     "barrier_result_description",                                    NA,                      NA,       NA,       NA,
              "crossing_fix_code",                                    NA,                      NA,       NA,       NA,
              "crossing_fix_desc",                        "crossing_fix",              "Fix type",      17L,       1L,
   "recommended_diameter_or_span", "recommended_diameter_or_span_meters",   "Fix Span / Diameter",      17L,       2L,
             "assessment_comment",                  "assessment_comment",               "Comment",       NA,       NA,
                     "ecocat_url",                                    NA,                      NA,       NA,       NA,
                 "image_view_url",                                    NA,                      NA,       NA,       NA,
           "current_pscis_status",                                    NA,                      NA,       NA,       NA,
     "current_crossing_type_code",                                    NA,                      NA,       NA,       NA,
     "current_crossing_type_desc",                                    NA,                      NA,       NA,       NA,
  "current_crossing_subtype_code",                                    NA,                      NA,       NA,       NA,
  "current_crossing_subtype_desc",                                    NA,                      NA,       NA,       NA,
    "current_barrier_result_code",                                    NA,                      NA,       NA,       NA,
    "current_barrier_description",                                    NA,                      NA,       NA,       NA,
                   "feature_code",                                    NA,                      NA,       NA,       NA,
                       "objectid",                                    NA,                      NA,       NA,       NA,
               "se_anno_cad_data",                                    NA,                      NA,       NA,       NA,
                       "geometry",                                    NA,                      NA,       NA,       NA
  )


xref_structure_fix <- tibble::tribble(
                        ~crossing_fix_code,                                ~crossing_fix_desc,                                     ~crossing_fix,
                                      "RM",                    "Remove / Deactivate Crossing",                      "Remove/Deactivate Crossing",
                                     "OBS",          "Replace with new open bottom structure",          "Replace with New Open Bottom Structure",
                                  "SS-CBS", "Replace structure with streambed simulation CBS", "Replace Structure with Streambed Simulation CBS",
                                      "EM",          "Add substrate to further imbed the CBS",          "Add Substrate to Further embed the CBS",
                                      "BW",     "Install downstream weir(s) to backwater CBS",     "Install Downstream Weir(s) to Backwater CBS"
                        )


##this is made from load-obstacles-xref.R
xref_obstacle_names <- tibble::tribble(
                                                    ~fiss_obstacle_name, ~spreadsheet_feature_type,
                                                          "Beaver Dams",                        NA,
                                                     "Velocity barrier",                        NA,
                                                                "Wedge",                        NA,
                                                               "Bridge",                        NA,
                                                            "Hydro Dam",                        NA,
                                         "Water Management Storage Dam",                        NA,
                                                        "Not Specified",                        NA,
                                                                 "Bars",                        NA,
                                                              "LWD Jam",                 "LWD Jam",
                                                                "OTHER",                        NA,
                                              "Irrigation District Dam",                        NA,
                                                 "Dam - Unknown Origin",                        NA,
                                                              "Cascade",                        NA,
                                                                 "Pump",                        NA,
                                                              "Log jam",                        NA,
                                                     "Cascade or Chute",                        NA,
                                                    "Persistent Debris",                        NA,
                                                            "Hydro dam",                        NA,
                                                                "Rocks",                        NA,
                         "Persistent debris; present for several years",                        NA,
                                                                 "Weir",                        NA,
                                                                "Falls",                   "falls",
                                                                 "Logs",                        NA,
                                                              "Log Jam",                        NA,
                                                              "Culvert",                        NA,
                                                                 "Rock",                        NA,
                                                               "Canyon",                        NA,
                                                           "Beaver Dam",                        NA,
                                                "Regional District Dam",                        NA,
                                                          "Underground",                        NA,
                                                         "Woody Debris",                        NA,
                                                        "Cascade/Chute",        "cascade or Chute",
                                                          "Private Dam",                        NA,
                                                             "Gradient",                        NA,
                                             "Fisheries Management Dam",                        NA,
                                                           "BEAVER DAM",                        NA,
                                          "Landslide or bank sloughing",                        NA,
                                                     "Velocity Barrier",                        NA,
                                                                  "Dam",                        NA
                         )


####------------make a table to summarize priorization of phase 1 sites
##uses habitat value to initially screen but then refines based on what are likely not barriers to most most the time
phase1_priorities <- pscis_phase1_reassessments %>%
  # filter(!source %ilike% 'phase2') %>% ##we don't want the phase 1 action
  select(pscis_crossing_id, my_crossing_reference, utm_zone:northing, habitat_value, barrier_result, source) %>%
  mutate(priority_phase1 = case_when(habitat_value == 'High' & barrier_result != 'Passable' ~ 'high',
                                     habitat_value == 'Medium' & barrier_result != 'Passable' ~ 'mod',
                                     habitat_value == 'Low' & barrier_result != 'Passable' ~ 'low',
                                     T ~ NA_character_)) %>%
  mutate(priority_phase1 = case_when(habitat_value == 'High' & barrier_result == 'Potential' ~ 'mod',
                                     T ~ priority_phase1)) %>%
  mutate(priority_phase1 = case_when(habitat_value == 'Medium' & barrier_result == 'Potential' ~ 'low',
                                     T ~ priority_phase1)) %>%
  # mutate(priority_phase1 = case_when(my_crossing_reference == 99999999999 ~ 'high', ##this is where we can make changes to the defaults
  #                                    T ~ priority_phase1)) %>%
  dplyr::rename(utm_easting = easting, utm_northing = northing)


##turn spreadsheet into list of data frames
pscis_phase1_for_tables <- pscis_all %>%
  filter(source %ilike% 'phase1') %>%
  arrange(pscis_crossing_id)

pscis_split <- pscis_phase1_for_tables  %>% #pscis_phase1_reassessments
  # sf::st_drop_geometry() %>%
  # mutate_if(is.numeric, as.character) %>% ##added this to try to get the outlet drop to not disapear
  # tibble::rownames_to_column() %>%
  dplyr::group_split(pscis_crossing_id) %>%
  purrr::set_names(pscis_phase1_for_tables$pscis_crossing_id)

##make result summary tables for each of the crossings
tab_summary <- pscis_split %>%
  purrr::map(make_tab_summary)

tab_summary_comments <- pscis_split %>%
  purrr::map(make_tab_summary_comments)

##had a hickup where R cannot handle the default size of the integers we used for numbers so we had to change site names!!
tab_photo_url <- list.files(path = paste0(getwd(), '/data/photos/'), full.names = T) %>%
  basename() %>%
  as_tibble() %>%
  mutate(value = as.integer(value)) %>%  ##need this to sort
  dplyr::arrange(value)  %>%
  mutate(photo = paste0('![](data/photos/', value, '/crossing_all.JPG)')) %>%
  filter(value %in% pscis_phase1_for_tables$my_crossing_reference)  %>% ##we don't want all the photos - just the phase 1 photos for this use case!!!
  left_join(., xref_pscis_my_crossing_modelled, by = c('value' = 'external_crossing_reference'))  %>% ##we need to add the pscis id so that we can sort the same
  arrange(stream_crossing_id) %>%
  select(-value) %>%
  # pull(photo)
  dplyr::group_split(stream_crossing_id)
  # purrr::set_names(nm = . %>% bind_rows() %>% arrange(value) %>% pull(stream_crossing_id)) %>%
  # bind_rows()
  # arrange(stream_crossing_id) %>%
  # dplyr::group_split(value)




##these are the reassessments!!!!!
##built from funciton in functions.R file
tabs_phase1 <- mapply(print_tab_summary_all, tab_sum = tab_summary, comments = tab_summary_comments, photos = tab_photo_url)

##built from funciton in functions.R file
tabs_phase1_pdf <- mapply(print_tab_summary_all_pdf, tab_sum = tab_summary, comments = tab_summary_comments, photos = tab_photo_url)


tab_plan_raw <- readr::read_csv(file = 'data/extracted_inputs/planning_results.csv', guess_max = 1500)

tab_plan_sf <- tab_plan_raw %>%
  filter(!is.na(my_text)) %>%
  arrange(stream_crossing_id, modelled_crossing_id) %>%
  st_as_sf(crs = 3005, coords = c("long", "lat")) %>%
  st_transform(crs = 4326) %>%
  mutate(my_priority = case_when(my_priority == 'mod' ~ 'moderate',
                                 T ~ my_priority)) %>%
  dplyr::mutate(image_view_url = case_when(is.na(image_view_url) ~ NA_character_,
                                           T ~ paste0('<a href =', image_view_url,'>', 'PSCIS Image link', '</a>'))) %>%
  select(
         Priority = my_priority,
         `PSCIS ID` = stream_crossing_id,
         `Modelled ID` = modelled_crossing_id,
         `Species` = observedspp_upstr,
         `Order` = stream_order,
         `Upstream habitat (km)` = salmon_network_km,
         `Channel width` = downstream_channel_width,
         `Habitat value` = habitat_value_code,
         `Image link` = image_view_url,
         Comments = my_text)
