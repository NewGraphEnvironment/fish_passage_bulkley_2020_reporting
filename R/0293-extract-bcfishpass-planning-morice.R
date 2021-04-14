##we are going to screen crossings in the MoRR that might be good spots to have a
##look at when we are in the field there.


##we should load our coastal district major structures layer into postgres and match to the pscis points
##then screen out our bridges

source('R/private_info.R')
source('R/packages.R')



##local wsl
conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = dbname_wsl,
  host = host_wsl,
  port = port_wsl,
  user = user_wsl,
  password = password_wsl
)

##bcbarriers
conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = dbname,
  host = host,
  port = port,
  user = user,
  password = password
)

dbGetQuery(conn,
           "SELECT table_name
           FROM information_schema.tables
           WHERE table_schema='whse_basemapping'")

# # # # # # ##list column names in a table
DBI::dbGetQuery(conn,
                "SELECT column_name,data_type
           FROM information_schema.columns
           WHERE table_name='coastal_bridge_registry'")


##get all the crossing info from the database
crossings <- sf::st_read(conn,
                                          query = "SELECT *
              FROM bcfishpass.crossings WHERE watershed_group_code IN ('MORR','BULK');")


##get the pscis info so we can join to see things like channel width
pscis <- DBI::dbGetQuery(conn,
                         "SELECT
                         p.*, wg.watershed_group_code
                         FROM whse_fish.pscis_assessment_svw p,
                         whse_basemapping.fwa_watershed_groups_poly wg
                         WHERE wg.watershed_group_code IN ('MORR', 'BULK')
                         AND ST_Within (p.geom, wg.geom);")


DBI::dbDisconnect(conn = conn)

##we are habing issues with these data types for some reason so we need to either remove them or change them to character.
# convert_to_char <- c('wscode_ltree','localcode_ltree','dnstr_crossings','dnstr_barriers_anthropogenic','observedspp_dnstr','observedspp_upstr')
convert_to_char <- c('wscode_ltree',
                     'localcode_ltree',
                     'dnstr_barriers_anthropogenic',
                     'observedspp_dnstr',
                     'observedspp_upstr',
                     'modelled_crossing_type_source',
                     'dnstr_crossings',
                     'upstr_barriers_anthropogenic'
                     ) ##removed 'dnstr_crossings'

##join our bcfishpass and pscis layers together and running our screening criteria
crossings_review <- full_join(
  crossings,
  select(pscis,
         stream_crossing_id,
         external_crossing_reference,
         stream_name,
         road_name,
         barrier_result_code,
         downstream_channel_width,
         habitat_value_code,
         consultant_name,
         image_view_url),
  by = 'stream_crossing_id'
) %>%
  # select(-dplyr::contains(c('wct'))) %>%  ##get rid of all the wct info for now
  filter(crossing_type_code == 'CBS') %>%
  # filter(!crossing_subtype_code %in% c('FORD', "BRIDGE", "OBS", "DAM - OTHER", "DAM")) %>%
  filter(is.na(barrier_status) | barrier_status != 'PASSABLE') %>%
  filter(is.na(crossing_source) | crossing_source != 'HABITAT CONFIRMATION') %>%
  filter(is.na(barrier_result_code) | barrier_result_code != 'PASSABLE') %>%
  filter(is.na(dnstr_barriers_anthropogenic)) %>%
  filter(watershed_group_code == 'MORR') %>%
  filter(
    !is.na(observedspp_upstr) |
      # (total_slopeclass03_km + total_slopeclass05_km > 1) |  ##more than 1km of habitat < 5% gradient - we will do this in Q rather than here
      steelhead_network_km >= 1 |
      stream_order >= 3 |
      steelhead_lakereservoir_ha >= 5 |
      steelhead_wetland_ha >= 5 |
      downstream_channel_width > 2 |
      habitat_value_code == 'MEDIUM' |
      habitat_value_code == 'HIGH') %>%
  mutate(my_text = NA_character_,  ##these are added so we can line up the summary
         my_priority = NA_character_,
         my_stream_name = NA_character_,
         my_road_name = NA_character_,
         modelled_crossing_id_corr = NA_integer_,
         structure = NA_character_, #fixes
         notes = NA_character_, #fixes
         reviewer = 'airvine') %>%
  # mutate(utm_easting = case_when(
  #   is.na(utm_easting) ~ easting,
  #   T ~ utm_easting),
  #   utm_northing = case_when(
  #     is.na(utm_northing) ~ northing,
  #     T ~ utm_northing)) %>%
  # st_as_sf(coords = c("utm_easting", "utm_northing")) %>%
  # select(-easting, -northing)
  mutate(across(all_of(convert_to_char), as.character))

##burn to local db
##local wsl
conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = dbname_wsl,
  host = host_wsl,
  port = port_wsl,
  user = user_wsl,
  password = password_wsl
)

# add a unique id - we could just use the reference number
crossings_review$misc_point_id <- seq.int(nrow(crossings_review))




##this is what we do if we don't want to incorperate already input information
DBI::dbSendQuery(conn, 'CREATE SCHEMA IF NOT EXISTS Working;')
sf::st_write(obj = crossings_review, dsn = conn, Id(schema= "working", table = "morice_planning_20210412"))
#
## sf doesn't automagically create a spatial index or a primary key
res <- dbSendQuery(conn, "CREATE INDEX ON working.morice_planning_20210412 USING GIST (geom)")
dbClearResult(res)
res <- dbSendQuery(conn, "ALTER TABLE working.morice_planning_20210412 ADD PRIMARY KEY (misc_point_id)")
dbClearResult(res)
# dbDisconnect(conn = conn)


query <- "SELECT * FROM working.morice_planning_20210412"
dat_after_review<- st_read(conn, query = query) %>%
  dplyr::mutate(long = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])
# arrange(my_text) ##this is weird. it will not read out the

dat_after_review %>% readr::write_csv(file = 'data/extracted_inputs/planning_results.csv')
# dbDisconnect(conn = conn)

##we should screen out our crossings that are in the coastal bridge registry as bridges so we need to join to our crossings

# ##lets reed in our kmz https://mitchellgritts.com/posts/load-kml-and-kmz-files-into-r/
# cbr <- st_read('./data/inputs_raw/Nadina 2017.kmz', layer = )
# st_layers('./data/inputs_raw/Nadina 2017.kmz')
#
#
# input_file <- './data/inputs_raw/Nadina 2017.kmz'
# target_file <- 'data/.temp.kml.zip'
# fs::file_copy(input_file, target_file)
# unzip(target_file, )
#
# kml <- read_sf('doc.kml')
# fs::file_delete(target_file)
# fs::file_delete('doc.kml')
# test <- rgdal::readOGR('./data/inputs_raw/Nadina.kml')
# test <- plotKML::kml_open('./data/inputs_raw/Nadina 2017.kmz')
#
# pscis_info <- dbGetQuery(conn, "SELECT
#   a.misc_point_id,
#   b.pscis_model_combined_id,
#   ST_Distance(ST_Transform(a.geom,3005), b.geom) AS distance
# FROM
#   working.misc_points AS a
# CROSS JOIN LATERAL
#   (SELECT *
#    FROM fish_passage.pscis_model_combined
#    ORDER BY
#      a.geom <-> geom
#    LIMIT 1) AS b")


