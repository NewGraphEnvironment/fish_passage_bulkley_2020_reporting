source('R/functions.R')
source('R/packages.R')
source('R/tables-phase2.R')


conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "postgis",
  host = "localhost",
  port = "5432",
  user = "postgres",
  password = "postgres"
)


##we are going to use the whole elk for now but should refine for our study area
fish_species_watershed <- sf::st_read(conn,
                                      query = "SELECT nws.gnis_name,nws.fwa_watershed_code, nws.gnis_id, x.species_code,x.species_name,x.observation_date
                   FROM whse_fish.fiss_fish_obsrvtn_pnt_sp x
                   INNER JOIN
                   whse_basemapping.fwa_named_watersheds_poly nws
                   ON ST_intersects(x.geom, nws.geom)
                   WHERE nws.gnis_id IN
                             ('16880')
                           GROUP BY x.species_code,x.species_name,nws.gnis_name,nws.gnis_id,x.observation_date,nws.fwa_watershed_code
                           ORDER BY nws.gnis_name,nws.fwa_watershed_code,x.species_code")

# fish_species_lookup <- dbGetQuery(conn,
#                                   "Select * from whse_fish.species_codes_lookup")

fish_species_lookup <- hab_fish_codes %>%
  select(common_name, species_code, scientific_name)


# fish_species_watershed <- merge (fish_species_watershed,
#                                 fish_species_lookup[,c("SPECIES_CODE","SCIENTIFIC_NAME")],
#                                 by.x = c("species_code"), by.y = c("SPECIES_CODE"),
#                                 all.x = TRUE)

fish_species_watershed <- left_join(fish_species_watershed,
                                    fish_species_lookup,
                                    by = "species_code")

##we need to remove Family: from the SCIENTIFIC_NAME column to facilitate a nice sort/lookup
##we could look at adding it after in brackets maybe
# fish_species_watershed$scientific_name <- gsub("Family: ", "", fish_species_watershed$scientific_name)

##select rows that have no scientific name
no_scientific <- fish_species_watershed[is.na(fish_species_watershed$scientific_name),]



#use pipes to group
fish_table <- fish_species_watershed %>%
  dplyr::group_by(scientific_name, species_name,gnis_name,species_code) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::arrange(gnis_name) %>% ##ditch the rare occurance which look like errors
  dplyr::filter(count > 1 &
                  species_name != 'Dolly Varden' &
                  species_name != 'Rainbow Smelt' &
                  !stringr::str_detect(species_name, "General") &
                  !species_code %in% 'TR') %>%
  ungroup() %>%
  filter(!is.na(scientific_name)) %>%
  select('Scientific Name' = scientific_name, 'Species Name' = species_name,
         'Species Code' = species_code) %>%
  mutate_all(~replace_na(.,"-")) %>%
  mutate_all(~stringr::str_replace_all(.,"NA", "-"))


##print your table to input_raw for use in the report
fish_table %>% readr::write_csv(file = paste0(getwd(), '/data/raw_input/fiss_species_table.csv'))


dbDisconnect(conn = conn)
