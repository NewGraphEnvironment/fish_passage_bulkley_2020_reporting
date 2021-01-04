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
           WHERE table_schema='whse_basemapping'")
# # # # #
# # # # # ##list column names in a table
dbGetQuery(conn,
           "SELECT column_name,data_type
           FROM information_schema.columns
           WHERE table_schema = 'whse_fish'
           and table_name='fiss_fish_obsrvtn_events_sp'")

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


#get a spatial version of the modelled info
query = "SELECT c.*, a.study_area
  FROM ali.misc a
INNER JOIN ali.crossings c ON (ST_Within(c.geom, a.geometry));"

dat_mod_elk <- st_read(conn, query = query)

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

##see who did assesssments in the flathead
unique(dat_pscis_flathead$consultant_name)
flat_vast <- dat_pscis_flathead %>%
  filter(consultant_name %ilike% 'VAST')  ##nothing large and nothing with high hab value - no mention in the report either

##burn the pscis info to a dataframe so that we can do a summary in the report
# dat_pscis %>% readr::write_csv(file = paste0(getwd(), '/data/raw_input/pscis_study_area.csv'))

#get a spatial version of the modelled info
query = "SELECT c.*, a.study_area
  FROM ali.misc a
INNER JOIN ali.crossings c ON (ST_Within(c.geom, a.geometry));"

dat_mod_flathead <- st_read(conn, query = query)

###lets join the two outputs together for each
dat_pscis <- bind_rows(
  dat_pscis_elk,
  dat_pscis_flathead
)

#burn the pscis info to a dataframe so that we can do a summary in the report
# dat_pscis %>% readr::write_csv(file = paste0(getwd(), '/data/raw_input/pscis_study_area.csv'))



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
         linear_feature_id_corr = NA_integer_,
         reviewer = 'ali',
         notes = NA_character_) %>%
  mutate(utm_easting = case_when(
    is.na(utm_easting) ~ easting,
    T ~ utm_easting),
    utm_northing = case_when(
      is.na(utm_northing) ~ northing,
      T ~ utm_northing)) %>%
  st_as_sf(coords = c("utm_easting", "utm_northing")) %>%
  select(-easting, -northing) %>%
  mutate(observedspp_dnstr2 = as.character(observedspp_dnstr), ##we are having issues with indexed columns that we are dropping but want to keep these
         observedspp_upstr2 = as.character(observedspp_upstr))

  # replace(., is.na(.), TRUE)
  # filter(!is.na(stream_crossing_id)) %>% ##heres the clincher! diff is 600 crossings
  # filter(study_area == 'flathead')
# filter(wct_network_km >= 1)

# add a unique id - we could just use the reference number
dat_mod$misc_point_id <- seq.int(nrow(dat))


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

##see the columns in a table
# dbGetQuery(conn,
#            "SELECT column_name,data_type
#            FROM information_schema.columns
#            WHERE table_schema = 'working'
#            and table_name='my_pscis_20180315'")

remove <- c('wscode_ltree','localcode_ltree','dnstr_crossings','dnstr_barriers_anthropogenic','observedspp_dnstr','observedspp_upstr')
dat <- dat_mod %>%
  select(-all_of(remove))

sf::st_write(obj = dat, dsn = conn, Id(schema= "working", table = "elk_flathead_planning_20210101", change the name!!))

## sf doesn't automagically create a spatial index or a primary key
res <- dbSendQuery(conn, "CREATE INDEX ON working.elk_flathead_planning_20210101 USING GIST (geometry)")
dbClearResult(res)
res <- dbSendQuery(conn, "ALTER TABLE working.elk_flathead_planning_20210101 ADD PRIMARY KEY (misc_point_id)")
dbClearResult(res)

dbDisconnect(conn = conn)

test <- dat %>%
  filter(modelled_crossing_id == 4605870)
