source('R/functions.R')
source('R/packages.R')
source('R/0320-tables-phase2.R')


conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "postgis",
  host = "localhost",
  port = "5432",
  user = "postgres",
  password = "postgres"
)


##get the observations from the fiss layer
fish_species_watershed <- sf::st_read(conn,
                                      query = "SELECT DISTINCT ws.watershed_group_code, x.species_code,x.species_name
                   FROM whse_fish.fiss_fish_obsrvtn_pnt_sp x
                   INNER JOIN
                   whse_basemapping.fwa_watershed_groups_poly ws
                   ON ST_intersects(x.geom, ws.geom)
                   WHERE ws.watershed_group_code IN
                             ('BULK','MORR')")


##lets bust it up and join it back together
fish_spp <- full_join(
  fish_species_watershed %>% filter(watershed_group_code == 'BULK') %>% rename(Bulkley = watershed_group_code),
  fish_species_watershed %>% filter(watershed_group_code == 'MORR')%>% rename(Morice = watershed_group_code),
  by = c('species_code', 'species_name')
)

fish_all <- fishbc::freshwaterfish
fish_cdc <- fishbc::cdc

fish_spp2 <- left_join(fish_spp,
                       fish_all,
                       by = c("species_code" = "Code")) %>%
  filter(!is.na(Class) & !species_code %in% c('TR', 'CBA')) %>% ##mottled sculpin has some sort of error going on
           # !species_code %in% c('ACT', 'ST', 'TR', 'C', 'GR', 'WF', 'WST')) %>%
  # mutate(species_name = case_when(species_name == 'Westslope (Yellowstone) Cutthroat Trout' ~ 'Westslope Cutthroat Trout',
  #                                 T ~ species_name)) %>%
  mutate(CDCode = case_when(species_code == 'BT' ~ 'F-SACO-11', ##pacific population yo
                            T ~ CDCode)) %>%
  select(species_code, species_name, Bulkley, Morice, CDCode)

fish_spp3 <- left_join(
  fish_spp2,
  fish_cdc,
  by = c('CDCode' = 'Species Code')
) %>%
  select(`Scientific Name`,
         'Species Name' = species_name,
         'Species Code' = species_code,
         `BC List`,
         `Provincial FRPA`,
         COSEWIC,
         SARA,
         Bulkley,
         Morice) %>%
  mutate(Bulkley = case_when(!is.na(Bulkley) ~ 'Yes',
                                 T ~ Bulkley),
         Morice = case_when(!is.na(Morice) ~ 'Yes',
                              T ~ Morice)) %>%
  mutate(COSEWIC = case_when(`Species Code` == 'CH' ~ NA_character_, ##designated units do not land in the skeena so remove
                             T ~ COSEWIC)) %>%
  arrange(`Scientific Name`, `Species Name`)
  # replace(., is.na(.), "--")

##print your table to input_raw for use in the report
fish_spp3 %>% readr::write_csv(file = paste0(getwd(), '/data/extracted_inputs/fiss_species_table.csv'))

dbDisconnect(conn = conn)
