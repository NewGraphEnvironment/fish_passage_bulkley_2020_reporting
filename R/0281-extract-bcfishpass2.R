##all the data is in the bcfishpass now so we will redo this and use it for analysis going forward

source('R/packages.R')
source('R/functions.R')
source('R/private_info.R')
source('R/0255-load-pscis.R')


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
# # # ##list tables in a schema
dbGetQuery(conn,
           "SELECT table_name
           FROM information_schema.tables
           WHERE table_schema='bcfishpass'")
# # # # #
# # # # # ##list column names in a table
dbGetQuery(conn,
           "SELECT column_name,data_type
           FROM information_schema.columns
           WHERE table_name='crossings'")


#first thing we want to do is match up our pha

dat <- pscis_all %>%
  sf::st_as_sf(coords = c("easting", "northing"),
               crs = 26909, remove = F) %>% ##don't forget to put it in the right crs buds
  sf::st_transform(crs = 3005) ##not sure why tis ws wsg84...

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

dat_info <- dbGetQuery(conn, "SELECT
  a.misc_point_id,
  b.*,
  ST_Distance(ST_Transform(a.geometry,3005), b.geom) AS distance
FROM
  ali.misc AS a
CROSS JOIN LATERAL
  (SELECT *
   FROM bcfishpass.crossings
   ORDER BY
     a.geometry <-> geom
   LIMIT 1) AS b")

##get all the data and save it as an sqlite database as a snapshot of what is happening.  we can always hopefully update it
query <- "SELECT *
   FROM bcfishpass.crossings
   WHERE watershed_group_code IN ('BULK','MORR')"

##import and grab the coordinates
bcfishpass_morr_bulk <- st_read(conn, query =  query) %>%
  st_transform(crs = 26909) %>%
  mutate(utm_zone = 9,
         northing = sf::st_coordinates(.)[,1],
         easting = sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry()



dbDisconnect(conn = conn)

##join the modelled road data to our pscis submission info

dat_joined <- left_join(
  dat,
  dat_info,
  # select(dat_info,misc_point_id:fcode_label, distance, crossing_id), ##geom keep only the road info and the distance to nearest point from here
  by = "misc_point_id"
)

##make a dataframe with our crossings that need a match
match_this <- dat_joined %>%
  st_drop_geometry() %>%
  select(pscis_crossing_id, stream_crossing_id, modelled_crossing_id, linear_feature_id, watershed_group_code) %>%
  mutate(reviewer = 'AI',
         notes = "Matched to closest stream model (not sure why it didn't happen automatically") %>%
  filter(!is.na(pscis_crossing_id) &
           is.na(stream_crossing_id))

match_this_to_join <- match_this %>%
  select(-stream_crossing_id) %>%
  mutate(linear_feature_id = NA_integer_) %>%
  rename(stream_crossing_id = pscis_crossing_id)


##test to see if the match_this hits are already assigned in crossings
bcfishpass_morr_bulk %>%
  filter(stream_crossing_id %in% (match_this %>% pull(pscis_crossing_id)))

##nope

##get the crossing data from bcfishpass
pscis_modelledcrossings_streams_xref <- read_csv("C:/scripts/bcfishpass/01_prep/02_pscis/data/pscis_modelledcrossings_streams_xref.csv")


##check to make sure your match_this crossings aren't already assigned somehow
pscis_modelledcrossings_streams_xref %>%
  filter(stream_crossing_id %in% (match_this %>% pull(pscis_crossing_id)))


# mydb <- DBI::dbConnect(RSQLite::SQLite(), "data/bcfishpass.sqlite")
conn <- rws_connect("data/bcfishpass.sqlite")
rws_list_tables(conn)
rws_drop_table("bcfishpass_morr_bulk", conn = conn)
rws_write(bcfishpass_morr_bulk, exists = F, delete = TRUE,
          conn = conn, x_name = "bcfishpass_morr_bulk")
# rws_write(epa_traits_dtbl, exists = FALSE, delete = TRUE,
#           conn = conn, x_name = "invertebrates_ffg_tolerance_epa_freshbiotraits")
rws_list_tables(conn)
rws_disconnect(conn)

# ##burn it all to a file we can use later
# dat_joined %>% readr::write_csv(file = paste0(getwd(), '/data/extracted_inputs/bcfishpass2.csv'))


