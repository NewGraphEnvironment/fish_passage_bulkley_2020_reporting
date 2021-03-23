##we need the watershed codes for our sites so we can submit our fish data to get our channel widths to feed the model
source('R/functions.R')
source('R/packages.R')
source('R/private_info.R')

conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = dbname,
  host = host,
  port = port,
  user = user,
  password = password
)

dbGetQuery(conn,
           "SELECT column_name,data_type
           FROM information_schema.columns
           WHERE table_name='crossings'")



wscodes <- dbGetQuery(conn,
                      "SELECT DISTINCT ON (stream_crossing_id)
a.stream_crossing_id,
a.linear_feature_id,
b.watershed_code_50k,
substring(b.watershed_code_50k from 1 for 3)
||'-'||substring(b.watershed_code_50k from 4 for 6)
||'-'||substring(b.watershed_code_50k from 10 for 5)
||'-'||substring(b.watershed_code_50k from 15 for 5)
||'-'||substring(b.watershed_code_50k from 20 for 4)
||'-'||substring(b.watershed_code_50k from 24 for 4)
||'-'||substring(b.watershed_code_50k from 28 for 3)
||'-'||substring(b.watershed_code_50k from 31 for 3)
||'-'||substring(b.watershed_code_50k from 34 for 3)
||'-'||substring(b.watershed_code_50k from 37 for 3)
||'-'||substring(b.watershed_code_50k from 40 for 3)
||'-'||substring(b.watershed_code_50k from 43 for 3) as watershed_code_50k_parsed,
b.blue_line_key_20k,
b.watershed_key_20k,
b.blue_line_key_50k,
b.watershed_key_50k,
b.match_type
FROM bcfishpass.crossings a
LEFT OUTER JOIN whse_basemapping.fwa_streams_20k_50k b
ON a.linear_feature_id = b.linear_feature_id_20k
WHERE a.watershed_group_code = 'BULK'
ORDER BY a.stream_crossing_id, b.match_type;"
)  %>%
  filter(stream_crossing_id %in% (pscis_phase2 %>% pull(pscis_crossing_id)))

##now we need to join to our habitat_confirmations sheet in the right order so we can copy and past into the spreadsheet
wsc_join <- left_join(
  hab_loc %>%
  tidyr::separate(alias_local_name, into = c('site', 'location', 'ef'), remove = F) %>%
    mutate(site = as.integer(site)),
  select(wscodes, stream_crossing_id, watershed_code_50k, watershed_code_50k_parsed),
  by = c('site' = 'stream_crossing_id')) %>%
  select(reference_number, gazetted_name, alias_local_name, site, watershed_code_50k, watershed_code_50k_parsed, utm_zone:utm_northing) %>%
  sf::st_as_sf(coords = c("utm_easting", "utm_northing"),
                            crs = 26909, remove = F) %>% ##don't forget to put it in the right crs buds
               sf::st_transform(crs = 4326) %>%
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2]) %>%  ##add teh coords to df so we qa in hab wizartd
  sf::st_drop_geometry() %>% ##convert to match the bcfishpass format
  readr::write_csv(file = paste0(getwd(), '/data/extracted_inputs/hab_con_wsc_codes.csv'), na = "")

##riddeck is 460-600600-23900-95600  00000MORR   00000BULK


