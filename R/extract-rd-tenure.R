##need to pull the rd tenure info and burn to a csv so we can copy into our pscis phase2 spreadsheet

source('R/packages.R')
source('R/tables-phase2.R')


pscis_tenure_input <- left_join(
  select(pscis2, rowname, pscis_crossing_id),
  select(bcfishpass_phase2, pscis_crossing_id, my_road_tenure),
  by = 'pscis_crossing_id'
) %>%
  mutate(rowname = as.numeric(rowname) + 4) %>%
  dplyr::arrange(rowname)


##something wier with these 2
# test <- bcfishpass_phase2 %>% filter(pscis_crossing_id %in% c(62425, 50159)) %>%

##burn it all to a file we can use later
pscis_tenure_input %>% readr::write_csv(file = paste0(getwd(), '/data/raw_input/pscis2_rd_tenure.csv'))
