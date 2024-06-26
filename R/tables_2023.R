## Scripts to update and add data from the recent sampling year.
## Creating objects needed for fpr_table_fish_site(), fpr_table_fish_density() and fpr_plot_fish_box()
## so that we can add tables with most recent data to the report.
## occurring on shared server only


# source("R/packages.R")

year = "2023"

# import habitat and fish data------------------
habitat_confirmations <- fpr_import_hab_con(path = paste0("data/2023/habitat_confirmations.xls"),
                                            col_filter_na = T,
                                            row_empty_remove = T,
                                            backup = FALSE)


hab_loc <- habitat_confirmations %>%
  purrr::pluck("step_1_ref_and_loc_info") %>%
  dplyr::filter(!is.na(site_number))%>%
  mutate(survey_date = janitor::excel_numeric_to_date(as.numeric(survey_date)))


##add the species code
hab_fish_codes <- fishbc::freshwaterfish %>%
  select(species_code = Code, common_name = CommonName) %>%
  tibble::add_row(species_code = 'NFC', common_name = 'No Fish Caught') %>%
  mutate(common_name = case_when(common_name == 'Cutthroat Trout' ~ 'Cutthroat Trout (General)', T ~ common_name))


## Needed for fpr_table_fish_density() and fpr_plot_fish_box()
## fish densities ----------------------------------------------------------

hab_fish_indiv_prep <- habitat_confirmations %>%
  purrr::pluck("step_3_individual_fish_data") %>%
  dplyr::filter(!is.na(site_number)) %>%
  select(-gazetted_names:-site_number)

hab_fish_indiv_prep2 <- left_join(
  hab_fish_indiv_prep,
  hab_loc,
  by = 'reference_number'
)

hab_fish_indiv_prep3 <- left_join(
  hab_fish_indiv_prep2,
  select(hab_fish_codes, common_name:species_code),
  by = c('species' = 'common_name')
) %>%
  dplyr::select(reference_number,
                alias_local_name,
                site_number,
                sampling_method,
                method_number,
                haul_number_pass_number,
                species_code,
                length_mm,
                weight_g) ##added method #

hab_fish_collect_info <- habitat_confirmations %>%
  purrr::pluck("step_2_fish_coll_data") %>%
  dplyr::filter(!is.na(site_number)) %>%
  # select(-gazetted_name:-site_number) %>%
  dplyr::distinct(reference_number, sampling_method, method_number, haul_number_pass_number, .keep_all = T)

# join the indiv fish data to existing site info
hab_fish_indiv <- full_join(
  select(hab_fish_indiv_prep3,
         reference_number,
         sampling_method,
         method_number,
         haul_number_pass_number,
         species_code,
         length_mm,
         weight_g),
  select(hab_fish_collect_info,
         reference_number,
         local_name,
         temperature_c:model, ##added date_in:time_out
         comments
  ),
  by = c(
    "reference_number",
    # 'alias_local_name' = 'local_name',
    "sampling_method",
    "method_number",
    "haul_number_pass_number")
) %>%
  mutate(species_code = as.character(species_code)) %>%
  mutate(species_code = case_when(
    is.na(species_code) ~ 'NFC',
    T ~ species_code)
  ) %>%
  mutate(species_code = as.factor(species_code)) %>%
  mutate(life_stage = case_when(  ##this section comes from the histogram below - we include here so we don't need to remake the df
    length_mm <= 65 ~ 'fry',
    length_mm > 65 & length_mm <= 110 ~ 'parr',
    length_mm > 110 & length_mm <= 140 ~ 'juvenile',
    length_mm > 140 ~ 'adult',
    T ~ NA_character_
  ),
  life_stage = case_when(
    stringr::str_detect(species_code, 'L|SU|LSU') ~ NA_character_,
    TRUE ~ life_stage
  ))%>%
  mutate(life_stage = fct_relevel(life_stage,
                                  'fry',
                                  'parr',
                                  'juvenile',
                                  'adult')) %>%
  tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F) %>%
  mutate(site_id = paste0(site, '_', location))




# make a summary table for fish sampling data
tab_fish_summary <- hab_fish_indiv %>%
  group_by(site_id,
           ef,
           sampling_method,
           species_code) %>% ##added sampling method!
  summarise(count_fish = n()) %>%
  arrange(site_id, species_code, ef)

#rename so we can use with updated data
tab_fish_summary_2023 <- tab_fish_summary


# this will be joined to the abundance estimates and the confidence intervals
fish_abund_prep <- hab_fish_indiv %>%
  group_by(local_name,
           site_id,
           ef,
           sampling_method,
           haul_number_pass_number,
           species_code,
           life_stage,
           ef_seconds) %>% ##added sampling method!
  filter(sampling_method == 'electrofishing') %>%
  summarise(catch = n()) %>%
  arrange(site_id, species_code, ef) %>%
  # ungroup() %>%
  mutate(catch = case_when(
    species_code == 'NFC' ~ 0L,
    T ~ catch),
    # effort = catch/ef_seconds,
    id = paste0(local_name, '_', species_code, '_', life_stage)) %>%
  ungroup() %>%
  arrange(id)

# join the total number of passes to each event so that we know if it is a higher number than the pass of the catch
fish_abund_prep2 <- left_join(
  fish_abund_prep,

  fish_abund_prep %>%
    group_by(local_name) %>%
    summarise(pass_total = max(haul_number_pass_number)),
  by = 'local_name'
)

# make a dat to indicate if the nfc in the set for each species
fish_nfc_tag<- fish_abund_prep2 %>%
  mutate(nfc_pass = case_when(
    # species_code != 'NFC' &
    haul_number_pass_number == pass_total ~ F,
    T ~ T),
    nfc_pass = case_when(
      species_code == 'NFC' ~ T,
      T ~ nfc_pass)
  ) %>%
  select(local_name, species_code, life_stage, haul_number_pass_number, pass_total, nfc_pass) %>%
  arrange(desc(haul_number_pass_number)) %>%
  # filter(nfc_pass == T) %>%
  distinct(local_name, species_code, life_stage, .keep_all = T) %>%
  select(-haul_number_pass_number, -pass_total)

# calculate abundance for each site regardless of whether a nfc_pass occurred.
fish_abund_prep3 <- left_join(
  fish_abund_prep2 %>%
    group_by(local_name, species_code, life_stage) %>%
    summarise(catch = sum(catch)),

  fish_nfc_tag,

  by = c('local_name', 'species_code', 'life_stage')
)


# add back the size of the sites so we can do a density
fish_abund <- left_join(
  fish_abund_prep3,

  hab_fish_collect_info %>%
    select(local_name,
           # sampling_method,
           # haul_number_pass_number,
           ef_seconds:enclosure) %>%
    distinct(local_name, ef_length_m, .keep_all = T),

  by = c('local_name')
) %>%
  mutate(area_m2 = round(ef_length_m * ef_width_m,1),
         density_100m2 = round(catch/area_m2 * 100,1)) %>%
  tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F)

# Rename so we can call fpr_table_fish_density() and fpr_plot_fish_box() with updated data
fish_abund_2023 <- fish_abund



## Needed for fpr_table_fish_site()
### density results -----------------------------------------------------------

# need to summarize just the sites
tab_fish_sites_sum <- left_join(
  fish_abund_prep2 %>%
    select(local_name, pass_total) %>%
    distinct(),


  hab_fish_collect_info %>%
    select(local_name,
           ef_length_m:enclosure) %>%
    distinct(),

  by = 'local_name'
) %>%
  mutate(area_m2 = round(ef_length_m * ef_width_m,1)) %>%
  select(site = local_name, passes = pass_total, ef_length_m, ef_width_m, area_m2, enclosure)

rm(fish_abund_prep,
  fish_abund_prep2,
  fish_abund_prep3,
  fish_nfc_tag
)

# Rename so we can call fpr_table_fish_site() with updated data
tab_fish_sites_sum_2023 <- tab_fish_sites_sum




