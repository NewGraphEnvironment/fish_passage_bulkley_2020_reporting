##this is the same as the extract-bcfishpass-planning.R script but we move to a different table due to issues with the downsteam_crossing_ids

##we use this script to pull the potentially good candidates from the crossings layer of bcfishpass
##so we can view them in qgis and add a priority as well as cross reference stream_crossing_id with the modelled_crossing_id

##there is another script (extract-bcfishpass-planning2-report.R) to pull the data back out and dress it for a clean kml file and tables



##to look at pscis crossings only we use the query builder in Q to say "stream_crossing_id" IS NOT NULL
##when it is time to look at the modelled crossings only we use "stream_crossing_id" IS NULL
source('R/functions.R')
source('R/packages.R')
source('R/private_info.R')

##get the road info from the database
conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = dbname,
  host = host,
  port = port,
  user = user,
  password = password
)
#
# ##listthe schemas in the database
# dbGetQuery(conn,
#            "SELECT schema_name
#            FROM information_schema.schemata")
# #
# #
# # ##list tables in a schema
dbGetQuery(conn,
           "SELECT table_name
           FROM information_schema.tables
           WHERE table_schema='whse_fish'")
# # # # #
# # # # # ##list column names in a table
barriers_anthropogenic <- dbGetQuery(conn,
           "SELECT column_name,data_type
           FROM information_schema.columns
           WHERE table_schema = 'bcfishpass'
           and table_name='barriers_anthropogenic'")

crossings <- dbGetQuery(conn,
                                     "SELECT column_name,data_type
           FROM information_schema.columns
           WHERE table_schema = 'bcfishpass'
           and table_name='crossings'")

cols_diff <- setdiff(crossings$column_name, barriers_anthropogenic$column_name)

##get the watersheds of interest
##here is the elk
wshd_study_elk <- fwapgr::fwa_watershed_at_measure(blue_line_key = 356570562, downstream_route_measure = 22910, epsg = 3005) %>%
  mutate(study_area = 'elk')

##here is the flathead
wshd_study_flathead <- st_read(conn,
                               query = "SELECT * FROM whse_basemapping.fwa_named_watersheds_poly a
                               WHERE a.named_watershed_id = '4600'"
                               ) %>%
  mutate(study_area = 'flathead')

##map it to have a look
# ggplot2::ggplot() +
#   ggplot2::geom_sf(data = wshd_study_elk, lwd = 0.15, fill = 'steelblue', alpha = 0.5) +
#   ggplot2::geom_sf(data = wshd_study_flathead, lwd = 0.15, fill = 'red', alpha = 0.5)


##join the two polygons together
# wshd_study_area <- sf::st_union(wshd_study_elk, wshd_study_flathead)

# ##plot it to be sure
# ggplot2::ggplot() +
#   ggplot2::geom_sf(data = wshd_study_area, lwd = 0.15, fill = 'steelblue', alpha = 0.5)

####--------------elk-------------------------------------

##burn the study area into the db and pull out the bcfishpass info
##make it called dat to keep things flexy
dat <- wshd_study_elk

# add a unique id - we could just use the reference number
# dat$misc_point_id <- seq.int(nrow(dat))

# dbSendQuery(conn, paste0("CREATE SCHEMA IF NOT EXISTS ", "test_hack",";"))
# load to database
sf::st_write(obj = dat, dsn = conn, Id(schema= "ali", table = "misc"))

# sf doesn't automagically create a spatial index or a primary key
# res <- dbSendQuery(conn, "CREATE INDEX ON ali.misc USING GIST (geometry)")
# dbClearResult(res)
# res <- dbSendQuery(conn, "ALTER TABLE ali.misc ADD PRIMARY KEY (misc_point_id)")
# dbClearResult(res)


##find the pscis points https://gis.stackexchange.com/questions/210387/querying-points-inside-and-outside-of-polygons
query <- "SELECT p.*, a.study_area
  FROM ali.misc a
INNER JOIN whse_fish.pscis_assessment_svw p ON (ST_Within(p.geom, a.geometry));"

dat_pscis_elk <- st_read(conn, query = query)

query2 <- "SELECT p.*, a.study_area
  FROM ali.misc a
INNER JOIN whse_fish.pscis_habitat_confirmation_svw p ON (ST_Within(p.geom, a.geometry));"

dat_pscis_elk2 <- st_read(conn, query = query2)

##CHANGE - we will try to use bcfishpass.barriers_anthropogenic
#get a spatial version of the modelled info
query = "SELECT c.*, a.study_area
  FROM ali.misc a
INNER JOIN bcfishpass.barriers_anthropogenic c ON (ST_Within(c.geom, a.geometry));"

#get a spatial version of the modelled info
# query = "SELECT c.*, a.study_area
#   FROM ali.misc a
# INNER JOIN ali.crossings c ON (ST_Within(c.geom, a.geometry));"


dat_mod_elk <- st_read(conn, query = query)
# dat_mod_elk_ali <- st_read(conn, query = query2)
#
# test <- setdiff(select(dat_mod_elk_ali, stream_crossing_id, modelled_crossing_id, dam_id) %>% st_drop_geometry(),
#                 select(dat_mod_elk, stream_crossing_id, modelled_crossing_id, dam_id) %>% st_drop_geometry())


####------now do the flathead-------------
dat <- wshd_study_flathead %>%
  st_cast("POLYGON") %>% ##need to make a polygon for some reason
  rename(geometry = geom)
# # add a unique id - we could just use the reference number
# dat$misc_point_id <- seq.int(nrow(dat))

# dbSendQuery(conn, paste0("CREATE SCHEMA IF NOT EXISTS ", "test_hack",";"))
# load to database
sf::st_write(obj = dat, dsn = conn, Id(schema= "ali", table = "misc"))
# # sf doesn't automagically create a spatial index or a primary key
# res <- dbSendQuery(conn, "CREATE INDEX ON ali.misc USING GIST (geometry)")
# dbClearResult(res)
# res <- dbSendQuery(conn, "ALTER TABLE ali.misc ADD PRIMARY KEY (misc_point_id)")
# dbClearResult(res)


##find the pscis points https://gis.stackexchange.com/questions/210387/querying-points-inside-and-outside-of-polygons
query <- "SELECT p.*, a.study_area
  FROM ali.misc a
INNER JOIN whse_fish.pscis_assessment_svw p ON (ST_Within(p.geom, a.geometry));"

dat_pscis_flathead <- st_read(conn, query = query)

query2 <- "SELECT p.*, a.study_area
  FROM ali.misc a
INNER JOIN whse_fish.pscis_habitat_confirmation_svw p ON (ST_Within(p.geom, a.geometry));"

dat_pscis_flathead2 <- st_read(conn, query = query2)

##see who did assesssments in the flathead
unique(dat_pscis_flathead$consultant_name)
flat_vast <- dat_pscis_flathead %>%
  filter(consultant_name %ilike% 'VAST')  ##nothing large and nothing with high hab value - no mention in the report either

###lets join the two outputs together for each
dat_pscis <- bind_rows(
  dat_pscis_elk,
  dat_pscis_flathead
)

dat_pscis2 <- bind_rows(
  dat_pscis_elk2,
  dat_pscis_flathead2
)

##burn the pscis info to a dataframe so that we can do a summary in the report
# dat_pscis %>% readr::write_csv(file = paste0(getwd(), '/data/raw_input/pscis_study_area.csv'))
# dat_pscis2 %>% readr::write_csv(file = paste0(getwd(), '/data/raw_input/pscis_phase2_study_area.csv'))


#get a spatial version of the modelled info
# query = "SELECT c.*, a.study_area
#   FROM ali.misc a
# INNER JOIN ali.crossings c ON (ST_Within(c.geom, a.geometry));"

#get a spatial version of the modelled info
query = "SELECT c.*, a.study_area
  FROM ali.misc a
INNER JOIN bcfishpass.barriers_anthropogenic c ON (ST_Within(c.geom, a.geometry));"

dat_mod_flathead <- st_read(conn, query = query)





###lets join the two outputs together for each and add the coordinates so we can make a valid
dat_mod_prep <- bind_rows(
  dat_mod_elk,
  dat_mod_flathead
) %>%
  st_transform(crs = 26911)

coords <- as_tibble(st_coordinates(dat_mod_prep)) %>%
  rename(easting = X, northing = Y)

dat_mod_prep <- bind_cols(
  dat_mod_prep,
  coords
)

##we are habing issues with these data types for some reason so we need to either remove them or change them to character.
# convert_to_char <- c('wscode_ltree','localcode_ltree','dnstr_crossings','dnstr_barriers_anthropogenic','observedspp_dnstr','observedspp_upstr')
convert_to_char <- c('wscode_ltree','localcode_ltree','dnstr_barriers_anthropogenic','observedspp_dnstr','observedspp_upstr') ##removed 'dnstr_crossings'


##add the pscis info that we need for our analysis
dat_mod <- full_join(
  dat_mod_prep %>% st_drop_geometry(),
  select(dat_pscis %>%
           st_drop_geometry(),
         stream_crossing_id, external_crossing_reference, utm_zone:utm_northing, stream_name, road_name, barrier_result_code,
         downstream_channel_width, habitat_value_code, consultant_name, image_view_url),
  by = 'stream_crossing_id'
) %>%
  select(-dplyr::contains(c('salmon', 'steelhead'))) %>%  ##get rid of all the salmon and steelhead info
  filter(!crossing_type %in% c('FORD', "BRIDGE", "OBS", "DAM - OTHER", "DAM - HYDRO")) %>%
  filter(is.na(barrier_status) | barrier_status != 'PASSABLE') %>%
  filter(is.na(crossing_source) | crossing_source != 'HABITAT CONFIRMATION') %>%
  filter(is.na(barrier_result_code) | barrier_result_code != 'PASSABLE') %>%
  # filter(is.na(dnstr_barriers_anthropogenic)) %>% ##added later
  filter(
    !is.na(observedspp_upstr) |
      wct_network_km >= 1 |
      stream_order >= 3 |
      wct_lakereservoir_ha >= 5 |
      wct_wetland_ha >= 5 |
      downstream_channel_width > 2 |
      habitat_value_code == 'MEDIUM' |
      habitat_value_code == 'HIGH') %>%
  mutate(my_text = NA_character_,  ##these are added so we can line up the summary
         my_priority = NA_character_,
         my_stream_name = NA_character_,
         my_road_name = NA_character_,
         modelled_crossing_id_corr = NA_integer_,
         reviewer = 'ali') %>%
  mutate(utm_easting = case_when(
    is.na(utm_easting) ~ easting,
    T ~ utm_easting),
    utm_northing = case_when(
      is.na(utm_northing) ~ northing,
      T ~ utm_northing)) %>%
  st_as_sf(coords = c("utm_easting", "utm_northing")) %>%
  select(-easting, -northing) %>%
  mutate(across(all_of(convert_to_char), as.character))



  # replace(., is.na(.), TRUE)
  # filter(!is.na(stream_crossing_id)) %>% ##heres the clincher! diff is 600 crossings
  # filter(study_area == 'flathead')
# filter(wct_network_km >= 1)

# add a unique id - we could just use the reference number
dat_mod$misc_point_id <- seq.int(nrow(dat_mod))


##disconnect then burn to local db
dbDisconnect(conn = conn)

conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "postgis",
  host = "localhost",
  port = "5432",
  user = "postgres",
  password = "postgres"
)

##############################################################################################################
##############################################################################################################
##THIS IS WHERE WE MOVE AROUND IN TIME#####################################################################
##############################################################################################################
##############################################################################################################


##this is what we do if we don't want to incorperate already input information
# sf::st_write(obj = dat, dsn = conn, Id(schema= "working", table = "elk_flathead_planning_20210101", change the name!!))
#
# ## sf doesn't automagically create a spatial index or a primary key
# res <- dbSendQuery(conn, "CREATE INDEX ON working.elk_flathead_planning_20210101 USING GIST (geometry)")
# dbClearResult(res)
# res <- dbSendQuery(conn, "ALTER TABLE working.elk_flathead_planning_20210101 ADD PRIMARY KEY (misc_point_id)")
# dbClearResult(res)

# dbDisconnect(conn = conn)


##lets get the data that has been input in qgis and join it back into the dataframe so it can be reburned
query <- "SELECT a.stream_crossing_id, a.modelled_crossing_id, a.dam_id, a.my_text, a.my_priority, a.my_stream_name, a.my_road_name,
a.modelled_crossing_id_corr, a.geometry FROM working.elk_flathead_planning_20210109 a;" ##this could now be changed to 20210109

dat_after_review<- st_read(conn, query = query) %>%
  st_drop_geometry()

##join back your info and populate where we have completed the review
# cols_to_replace <- names(dat_after_review) %>%
#   purrr::discard(~ .x %in% c('aggregated_crossings_id', 'stream_crossing_id')) ##we join with this so keep

cols_to_replace <- names(dat_after_review) %>%
  purrr::discard(~ .x %in% c('modelled_crossing_id', 'stream_crossing_id', 'dam_id')) ##we join with this so keep

# 'modelled_crossing_id', 'stream_crossing_id', 'dam_id'

##remove the columns we already have data for
dat_mod2 <- dat_mod %>%
  select(-all_of(cols_to_replace))

##join with data already completed
dat_mod3 <- left_join(
  dat_mod2,
  dat_after_review,
  by = c('modelled_crossing_id', 'stream_crossing_id', "dam_id")
)

##declare the schema and table names
schema_name = "working"
table_name = "elk_flathead_planning_20210110"
crs = 26911
geom_type = "Point"

##burn back to the database and keep on working!!!
sf::st_write(obj = dat_mod3, dsn = conn, Id(schema= schema_name, table = table_name))

## sf doesn't automagically create a spatial index or a primary key
res <- dbSendQuery(conn, paste0("CREATE INDEX ON ", schema_name, ".", table_name, " USING GIST (geometry)"))
dbClearResult(res)
res <- dbSendQuery(conn, paste0("ALTER TABLE ", schema_name, ".", table_name, " ADD PRIMARY KEY (misc_point_id)"))
dbClearResult(res)
res <- dbSendQuery(conn,
                   paste0("ALTER TABLE ", schema_name, ".", table_name, " ALTER COLUMN geometry
           Type geometry(", geom_type, ", ", crs, ")
           USING ST_SetSRID(geometry, ", crs, ");"))
dbClearResult(res)

dbDisconnect(conn = conn)
