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
           WHERE table_schema='bcfishpass'")
# # # # #
# # # # # ##list column names in a table
dbGetQuery(conn,
           "SELECT column_name,data_type
           FROM information_schema.columns
           WHERE table_schema = 'bcfishpass'
           and table_name='modelled_stream_crossings'")

##get the watersheds of interest
##here is the elk
wshd_study_elk <- fwapgr::fwa_watershed_at_measure(blue_line_key = 356570562, downstream_route_measure = 22910, epsg = 3005)

##here is the flathead
wshd_study_flathead <- st_read(conn,
                               query = "SELECT * FROM whse_basemapping.fwa_named_watersheds_poly a
                               WHERE a.named_watershed_id = '4600'"
                               )

##map it to have a look
# ggplot2::ggplot() +
#   ggplot2::geom_sf(data = wshd_study_elk, lwd = 0.15, fill = 'steelblue', alpha = 0.5) +
#   ggplot2::geom_sf(data = wshd_study_flathead, lwd = 0.15, fill = 'red', alpha = 0.5)


##join the two polygons together
wshd_study_area <- sf::st_union(wshd_study_elk, wshd_study_flathead)

# ##plot it to be sure
# ggplot2::ggplot() +
#   ggplot2::geom_sf(data = wshd_study_area, lwd = 0.15, fill = 'steelblue', alpha = 0.5)


##burn the study area into the db and pull out the bcfishpass info
##make it called dat to keep things flexy
dat <- wshd_study_area

# add a unique id - we could just use the reference number
dat$misc_point_id <- seq.int(nrow(dat))

# dbSendQuery(conn, paste0("CREATE SCHEMA IF NOT EXISTS ", "test_hack",";"))
# load to database
sf::st_write(obj = dat, dsn = conn, Id(schema= "ali", table = "misc"))

# sf doesn't automagically create a spatial index or a primary key
res <- dbSendQuery(conn, "CREATE INDEX ON ali.misc USING GIST (geometry)")
dbClearResult(res)
res <- dbSendQuery(conn, "ALTER TABLE ali.misc ADD PRIMARY KEY (misc_point_id)")
dbClearResult(res)


##find the pscis points https://gis.stackexchange.com/questions/210387/querying-points-inside-and-outside-of-polygons
query <- "SELECT p.*
  FROM ali.misc a
JOIN whse_fish.pscis_assessment_svw p ON (ST_Within(p.geom, a.geometry));"

dat_pscis <- st_read(conn, query = query)

##burn the pscis info to a dataframe so that we can do a summary in the report
# dat_pscis %>% readr::write_csv(file = paste0(getwd(), '/data/raw_input/pscis_study_area.csv'))

#get a spatial version of the modelled info
query = "SELECT c.*
  FROM ali.misc a
JOIN ali.crossings c ON (ST_Within(c.geom, a.geometry));"

dat_mod <- st_read(conn, query = query)


##add the pscis info that we need for our analysis
dat_mod2 <- left_join(
  dat_mod,
  select(dat_pscis %>%
           st_drop_geometry(),
         stream_crossing_id, stream_name, road_name, downstream_channel_width, image_view_url),
  by = 'stream_crossing_id'
)


##get rid of all the salmon and steelhead info
dat_mod3 <- dat_mod2 %>%
  select(-dplyr::contains(c('salmon', 'steelhead'))) %>% ##get rid of all the salmon and steelhead info
  filter(!crossing_type %in% c('FORD', "BRIDGE", "OBS", "DAM - OTHER", "DAM - HYDRO")) %>%
  filter(barrier_status != 'PASSABLE') %>%
  filter(
    !is.na(observedspp_upstr) |
    wct_network_km >= 2 |
      stream_order >= 3 |
      wct_lakereservoir_ha >= 5 |
      wct_wetland_ha >= 5 |
      downstream_channel_width > 2
           ) %>%
  filter(!is.na(stream_crossing_id)) ##heres the clincher! diff is 600 crossings
  # filter(wct_network_km >= 1)

