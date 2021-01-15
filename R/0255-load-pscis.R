##now that we have loaded our phase 1 data to pscis we need to pull out the crossing id's
##this is probably better achieved by pulling from postgres after loading with 00240-load-bcdata.R
##but we will leave as is for now since it is done


source('R/packages.R')
source('R/functions.R')


####---------------import pscis data----------------
pscis_phase1 <- import_pscis(workbook_name = 'pscis_phase1.xlsm')
# filter(!my_crossing_reference %in% dups)

pscis_phase2 <- import_pscis(workbook_name = 'pscis_phase2.xlsm')

pscis_reassessments <- import_pscis(workbook_name = 'pscis_reassessments.xlsm')

pscis_combined <- bind_rows(
  pscis_phase1,
  pscis_phase2,
  pscis_reassessments
) %>%
  distinct(.keep_all = T) %>%
  sf::st_as_sf(coords = c("easting", "northing"),
               crs = 26909, remove = F) %>% ##don't forget to put it in the right crs buds
  sf::st_transform(crs = 3005) ##convert to match the bcfishpass format

##lets add in the xref pscis id info
##this is made from load-crossings-xref.R
xref_pscis_my_crossing_modelled <- readr::read_csv(file = paste0(getwd(), '/data/extracted_inputs/xref_pscis_my_crossing_modelled.csv'))


pscis_all <- left_join(
  pscis_combined,
  xref_pscis_my_crossing_modelled,
  by = c('my_crossing_reference' = 'external_crossing_reference')
) %>%
  mutate(pscis_crossing_id = case_when(
    is.na(pscis_crossing_id) ~ stream_crossing_id,
    T ~ pscis_crossing_id
  )) %>%
  mutate(amalgamated_crossing_id = case_when(
    !is.na(my_crossing_reference) ~ my_crossing_reference,
    T ~ pscis_crossing_id
  )) %>%
  select(-stream_crossing_id) %>%
  arrange(pscis_crossing_id)

pscis_phase1_reassessments <- pscis_all %>%
  filter(!source %ilike% 'phase2')

