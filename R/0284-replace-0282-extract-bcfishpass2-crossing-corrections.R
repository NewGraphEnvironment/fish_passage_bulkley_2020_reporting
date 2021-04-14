##all the data is in bcfishpass.crossings now so we will redo this and use it for analysis going forward
##in here we also have workflows to match our crossings to modelled crossings in case they are not already.
##We are going to test pulling from our local db.

source('R/packages.R')
# source('R/functions.R')
source('R/private_info.R')
# source('R/0255-load-pscis.R')

##this is from the old file
# conn <- DBI::dbConnect(
#   RPostgres::Postgres(),
#   dbname = dbname,
#   host = host,
#   port = port,
#   user = user,
#   password = password
# )

conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = dbname_wsl,
  host = host_wsl,
  port = port_wsl,
  user = user_wsl,
  password = password_wsl
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
           WHERE table_schema='whse_fish'")
# # # # #
# # # # # ##list column names in a table
dbGetQuery(conn,
           "SELECT column_name,data_type
           FROM information_schema.columns
           WHERE table_name='fiss_fish_obsrvtn_pnt_sp'")

dbGetQuery(conn,
           "SELECT a.total_lakereservoir_ha
           FROM bcfishpass.crossings a
           WHERE stream_crossing_id IN (58159,58161,123446)")

dbGetQuery(conn,
           "SELECT o.observation_date, o.point_type_code  FROM whse_fish.fiss_fish_obsrvtn_pnt_sp o;") %>%
  filter(observation_date > '1900-01-01' &
         observation_date < '2021-02-01') %>%
  group_by(point_type_code) %>%
  summarise(min = min(observation_date, na.rm = T),
            max = max(observation_date, na.rm = T))


# #first thing we want to do is match up our pha
#
# dat <- pscis_all %>%
#   sf::st_as_sf(coords = c("easting", "northing"),
#                crs = 26909, remove = F) %>% ##don't forget to put it in the right crs buds
#   sf::st_transform(crs = 3005) ##get the crs same as the layers we want to hit up
#
# # add a unique id - we could just use the reference number
# dat$misc_point_id <- seq.int(nrow(dat))
#
# # dbSendQuery(conn, paste0("CREATE SCHEMA IF NOT EXISTS ", "test_hack",";"))
# # load to database
# sf::st_write(obj = dat, dsn = conn, Id(schema= "ali", table = "misc"))
#
#
# # sf doesn't automagically create a spatial index or a primary key
# res <- dbSendQuery(conn, "CREATE INDEX ON ali.misc USING GIST (geometry)")
# dbClearResult(res)
# res <- dbSendQuery(conn, "ALTER TABLE ali.misc ADD PRIMARY KEY (misc_point_id)")
# dbClearResult(res)
#
# dat_info <- dbGetQuery(conn, "SELECT
#   a.misc_point_id,
#   b.*,
#   ST_Distance(ST_Transform(a.geometry,3005), b.geom) AS distance
# FROM
#   ali.misc AS a
# CROSS JOIN LATERAL
#   (SELECT *
#    FROM bcfishpass.crossings
#    ORDER BY
#      a.geometry <-> geom
#    LIMIT 1) AS b")
#
# ##get all the data and save it as an sqlite database as a snapshot of what is happening.  we can always hopefully update it
query <- "SELECT *
   FROM bcfishpass.crossings
   WHERE watershed_group_code IN ('BULK','MORR')"

##import and grab the coordinates - this is already done
bcfishpass_morr_bulk <- st_read(conn, query =  query) %>%
  # st_transform(crs = 26909) %>%  ##simon does this now on his end.
  # mutate(utm_zone = 9,
  #        easting = sf::st_coordinates(.)[,1],
  #        northing = sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry()



# test <- dbGetQuery(conn, "SELECT *
#    FROM bcfishpass.mean_annual_precip
#    WHERE watershed_group_code IN ('BULK','MORR')")
#
# test2 <- dbGetQuery(conn, "SELECT *
#    FROM bcfishpass.streams
#    WHERE watershed_group_code IN ('BULK','MORR')")

# porphyryr <- st_read(conn, query =
# "SELECT * FROM bcfishpass.crossings
#    WHERE stream_crossing_id = '124487'")

dbDisconnect(conn = conn)


##join the modelled road data to our pscis submission info

dat_joined <- left_join(
  dat,
  dat_info,
  # select(dat_info,misc_point_id:fcode_label, distance, crossing_id), ##geom keep only the road info and the distance to nearest point from here
  by = "misc_point_id"
)

##lets simiplify dat_joined to have a look up
my_pscis_modelledcrossings_streams_xref <- dat_joined %>%
  select(pscis_crossing_id, stream_crossing_id, modelled_crossing_id, source) %>%
  st_drop_geometry()

##this is how we update our local db.
##my time format format(Sys.time(), "%Y%m%d-%H%M%S")
# mydb <- DBI::dbConnect(RSQLite::SQLite(), "data/bcfishpass.sqlite")
conn <- rws_connect("data/bcfishpass.sqlite")
rws_list_tables(conn)
##archive the last version for now
bcfishpass_archive <- readwritesqlite::rws_read_table("bcfishpass_morr_bulk", conn = conn)
# rws_drop_table("bcfishpass_archive", conn = conn) ##if it exists get rid of it - might be able to just change exists to T in next line
rws_write(bcfishpass_archive, exists = F, delete = TRUE,
          conn = conn, x_name = paste0("bcfishpass_morr_bulk_archive", "_", format(Sys.time(), "%Y-%m-%d-%H%m")))
rws_drop_table("bcfishpass_morr_bulk", conn = conn) ##now drop the table so you can replace it
rws_write(bcfishpass_morr_bulk, exists = F, delete = TRUE,
          conn = conn, x_name = "bcfishpass_morr_bulk")
# rws_drop_table("my_pscis_modelledcrossings_streams_xref", conn = conn)
# rws_write(my_pscis_modelledcrossings_streams_xref, exists = FALSE, delete = TRUE,
#           conn = conn, x_name = "my_pscis_modelledcrossings_streams_xref")
rws_list_tables(conn)
rws_disconnect(conn)

##make a dataframe with our crossings that need a match
match_this <- dat_joined %>%
  st_drop_geometry() %>%
  select(pscis_crossing_id, stream_crossing_id, modelled_crossing_id, linear_feature_id, watershed_group_code) %>%
  mutate(reviewer = 'AI',
         notes = "Matched to closest stream model") %>%
  filter(!is.na(pscis_crossing_id) &
           is.na(stream_crossing_id))

match_this_to_join <- match_this %>%
  select(-stream_crossing_id) %>%
  mutate(linear_feature_id = NA_integer_) %>%
  rename(stream_crossing_id = pscis_crossing_id) %>%
  mutate(across(c(stream_crossing_id:linear_feature_id), as.numeric))


##test to see if the match_this hits are already assigned in crossings
bcfishpass_morr_bulk %>%
  filter(stream_crossing_id %in% (match_this %>% pull(pscis_crossing_id)))

##need to learn to move from the other fork for now rename and grab from there
file.copy(from = "C:/scripts/bcfishpass/01_prep/02_pscis/data/pscis_modelledcrossings_streams_xref.csv",
            to = "C:/scripts/pscis_modelledcrossings_streams_xref.csv",
          overwrite = T)

##get the crossing data from bcfishpass
pscis_modelledcrossings_streams_xref <- readr::read_csv("C:/scripts/pscis_modelledcrossings_streams_xref.csv")


##check to make sure your match_this crossings aren't already assigned somehow
pscis_modelledcrossings_streams_xref %>%
  filter(stream_crossing_id %in% (match_this %>% pull(pscis_crossing_id)))

##because the crossings are already there we will need to pull them out and then sub them back in
pscis_modelledcrossings_streams_xref_to_join <- pscis_modelledcrossings_streams_xref %>%
  filter(!stream_crossing_id %in% (match_this %>% pull(pscis_crossing_id)))

pscis_modelledcrossings_streams_xref_joined <- bind_rows(
  pscis_modelledcrossings_streams_xref_to_join,
  match_this_to_join) %>%
  # mutate(stream_crossing_id = as.integer(stream_crossing_id)) %>% ##i can't figure out why this needs to be an integer. it should sort as is (numeric)
  dplyr::arrange(stream_crossing_id)

##now burn it back to bcfishpass ready for a pull request
readr::write_csv(pscis_modelledcrossings_streams_xref_joined, "C:/scripts/bcfishpass/01_prep/02_pscis/data/pscis_modelledcrossings_streams_xref.csv",
                 na = "")
