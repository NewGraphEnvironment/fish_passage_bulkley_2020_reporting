source('R/functions.R')
source('R/packages.R')
source('R/private_info.R')

##make a dataframe to pull info from the db
##we should probably break each row out and determine the crs by the utm_zone attribute
##lets do both phases at once to create a file for feeding back to bcfishpass


##this is weird but we know these will be dups because we check at the end of this script.
##lets pull these out of these files at the start

# dups <- c(4600183, 4600069, 4600367, 4605732, 4600070)

dat1 <- import_pscis(workbook_name = 'pscis_phase1.xlsm')
  # filter(!my_crossing_reference %in% dups)

dat2 <- import_pscis(workbook_name = 'pscis_phase2.xlsm')

dat3 <- import_pscis(workbook_name = 'pscis_reassessments.xlsm')

dat <- bind_rows(
  dat1,
  dat2,
  dat3
) %>%
  distinct(.keep_all = T) %>%
  sf::st_as_sf(coords = c("easting", "northing"),
               crs = 26909, remove = F) %>% ##don't forget to put it in the right crs buds
  sf::st_transform(crs = 3005) ##convert to match the bcfishpass format


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
           WHERE table_name='modelled_stream_crossings'")


# test <- dbGetQuery(conn, "SELECT * FROM bcfishpass.waterfalls")

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
   FROM fish_passage.modelled_crossings_closed_bottom
   ORDER BY
     a.geometry <-> geom
   LIMIT 1) AS b")



##join the modelled road data to our pscis submission info

dat_joined <- left_join(
  dat,
  select(dat_info,misc_point_id:utm_northing, distance, crossing_id, geom), ##keep only the road info and the distance to nearest point from here
  by = "misc_point_id"
)

dbDisconnect(conn = conn)


##we also need to know if the culverts are within a municipality so we should check
##get the road info from our database
conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "postgis",
  host = "localhost",
  port = "5432",
  user = "postgres",
  password = "postgres"
)


##list tables in a schema
# dbGetQuery(conn,
#            "SELECT table_name
#            FROM information_schema.tables
#            WHERE table_schema='ali'")

# dbGetQuery(conn,
#            "SELECT column_name,data_type
#            FROM information_schema.columns
#            WHERE table_name='dbm_mof_50k_grid'")


# load to database
sf::st_write(obj = dat, dsn = conn, Id(schema= "working", table = "misc"))


dat_info <- dbGetQuery(conn,
                       "

                                  SELECT a.misc_point_id, b.admin_area_abbreviation, c.map_tile_display_name
                                  FROM working.misc a
                                  INNER JOIN
                                  whse_basemapping.dbm_mof_50k_grid c
                                  ON ST_Intersects(c.geom, ST_Transform(a.geometry,3005))
                                  LEFT OUTER JOIN
                                  whse_legal_admin_boundaries.abms_municipalities_sp b
                                  ON ST_Intersects(b.geom, ST_Transform(a.geometry,3005))
                       ")

dbDisconnect(conn = conn)

##add the municipality info
dat_joined2 <- left_join(
  dat_joined,
  dat_info,
  by = "misc_point_id"
)


##we already did this but can do it again I guess.  you cut and paste the result into kable then back
##into here using addin for datapasta
# tab_rd_tenure_xref <- unique(dat_joined2$client_name) %>%
#   as_tibble() %>%
#   purrr::set_names(nm = 'client_name') %>%
#   mutate(client_name_abb = NA)

tab_rd_tenure_xref <- tibble::tribble(
                                           ~client_name,        ~client_name_abb,
                        "DISTRICT MANAGER NADINA (DND)",           "FLNR Nadina",
                        "CANADIAN FOREST PRODUCTS LTD.",                "Canfor",
                            "WET’SUWET’EN VENTURES LTD", "Wet’suwet’en Ventures"
                        )



##add that to your dat file for later
dat_joined3 <- left_join(
  dat_joined2,
  tab_rd_tenure_xref,
  by = 'client_name'
)

##make a dat to make it easier to see so we can summarize the road info we might want to use
# dat_rd_sum <- dat_joined3 %>%
#   select(pscis_crossing_id, my_crossing_reference, crossing_id, distance, road_name_full,
#          road_class, road_name_full, road_surface, file_type_description, forest_file_id,
#          client_name, client_name_abb, map_label, owner_name, admin_area_abbreviation)


##make a dat to make it easier to see so we can summarize the road info we might want to use
dat_joined4 <- dat_joined3 %>%
  mutate(admin_area_abbreviation = case_when(
    is.na(admin_area_abbreviation) ~ 'MoTi',
    T ~ admin_area_abbreviation),
    my_road_tenure =
      case_when(!is.na(client_name_abb) ~ paste0(client_name_abb, ' ', forest_file_id),
                !is.na(road_class) ~ paste0(admin_area_abbreviation, ' ', road_class),
                !is.na(owner_name) ~ owner_name)) %>%
  mutate(my_road_tenure =
           case_when(distance > 100 ~ 'Unknown',  ##we need to get rid of the info for the ones that are far away
                     T ~ my_road_tenure)) %>%
  rename(geom_modelled_crossing = geom)

##build tables to populate the pscis spreadsheets
pscis1_rd_tenure <- left_join(
  select(dat1, my_crossing_reference),
  select(dat_joined4, my_crossing_reference, my_road_tenure),
  by = 'my_crossing_reference'
) %>%
  distinct(my_crossing_reference, my_road_tenure) %>%
  tibble::rownames_to_column() %>%
  mutate(rowname = as.integer(rowname)) %>%
  mutate(rowname = rowname + 4)


##burn it all to a file we can input to pscis submission spreadsheet
pscis1_rd_tenure %>% readr::write_csv(file = paste0(getwd(), '/data/extracted_inputs/pscis1_rd_tenure.csv'))

pscis_reassessmeents_rd_tenure <- left_join(
  select(dat3, pscis_crossing_id),
  select(dat_joined4, pscis_crossing_id, my_road_tenure),
  by = 'pscis_crossing_id'
) %>%
  distinct(pscis_crossing_id, my_road_tenure) %>%
  tibble::rownames_to_column() %>%
  mutate(rowname = as.integer(rowname)) %>%
  mutate(rowname = rowname + 4)

##burn it all to a file we can input to pscis submission spreadsheet
pscis_reassessmeents_rd_tenure %>% readr::write_csv(file = paste0(getwd(), '/data/extracted_inputs/pscis_reassessmeents_rd_tenure.csv'))

pscis_phase2_rd_tenure <- left_join(
  select(dat2, pscis_crossing_id),
  select(dat_joined4, pscis_crossing_id, my_road_tenure),
  by = 'pscis_crossing_id'
) %>%
  distinct(pscis_crossing_id, my_road_tenure) %>%
  tibble::rownames_to_column() %>%
  mutate(rowname = as.integer(rowname)) %>%
  mutate(rowname = rowname + 4)

##burn it all to a file we can input to pscis submission spreadsheet
pscis_phase2_rd_tenure %>% readr::write_csv(file = paste0(getwd(), '/data/extracted_inputs/pscis_phase2_rd_tenure.csv'))


##we need to qa which are our modelled crossings at least for our phase 2 crossings

# pscis_modelledcrossings_streams_xref <- dat_joined4 %>%
#   select(stream_crossing_id = pscis_crossing_id,
#          crossing_id,
#          linear_feature_id) %>%
#   filter(!is.na(stream_crossing_id)) %>%
#   mutate(modelled_crossing_id = case_when(
#     stream_crossing_id == 50159 |
#     stream_crossing_id == 62425 |
#     stream_crossing_id == 62426
#     ~ NA_integer_,
#     T ~ crossing_id
#   ),
#   linear_feature_id = case_when(
#     is.na(modelled_crossing_id) ~ NA_integer64_,
#     T ~ linear_feature_id
#   ))

##we need to add our pscis info
# dat_joined6 <- dat_joined5 %>%
#   mutate(stream_crossing_id = as.numeric(stream_crossing_id)) %>%
#   mutate(pscis_crossing_id = case_when(is.na(pscis_crossing_id) ~ stream_crossing_id,
#                                        T ~ pscis_crossing_id))

#######################################################################################################


#######-----------------add the update real bcfishpass information-----------
##if we run into issues we can come back and rebiuld from here
##this sucks and is super hacky but we are going to grab all the info from bcfishpass and add it

##connect again
##get the new bcfishpass info from the database
conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = dbname,
  host = host,
  port = port,
  user = user,
  password = password
)


##maybe easiest to just burn in our ids and join based on the modelled crosing id

dat <- dat_joined4 %>%
  select(crossing_id)

# load to database
sf::st_write(obj = dat, dsn = conn, Id(schema= "ali", table = "misc"))

dat_info <- dbGetQuery(conn,
"SELECT a.crossing_id, b.*
FROM ali.misc a
LEFT OUTER JOIN
bcfishpass.crossings b
ON a.crossing_id = b.modelled_crossing_id")

##here we find identical columns because we will want to remove them from one of the lists.
columns_to_remove <- intersect(names(dat_joined4), names(dat_info))
columns_to_keep <- "crossing_id"
columns_to_remove <- setdiff(columns_to_remove, columns_to_keep)  ##make sure to keep the joining column


##Lets keep the newest ones and remove from the old dataframe

dat_joined5 <- dat_joined4 %>%
  select(-all_of(columns_to_remove))

##join together the old and the new
dat_joined6 <- left_join(
  dat_joined5,
  dat_info,
  by = 'crossing_id'
) %>%
  distinct(pscis_crossing_id, my_crossing_reference, source, .keep_all = T)

dups <- dat_joined6 %>% group_by(pscis_crossing_id) %>%
  mutate(duplicated = n()>1) %>%
  filter(duplicated == T & !is.na(pscis_crossing_id))   ##this is not connected bc its an error with the geometry when its empty - feeds the top input though!!!
  # distinct(pscis_crossing_id, .keep_all = T) %>%
  # pull(my_crossing_reference)

##burn it all to a file we can use later
dat_joined6 %>% readr::write_csv(file = paste0(getwd(), '/data/extracted_inputs/bcfishpass.csv'))


