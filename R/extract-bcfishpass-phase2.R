source('R/functions.R')
source('R/packages.R')
source('R/private_info.R')

##make a dataframe to pull info from the db
##we should probably break each row out and determine the crs by the utm_zone attribute
##lets do both phases at once to create a file for feeding back to bcfishpass


##this is weird but we know these will be dups because we check at the end of this script.
##lets pull these out of these files at the start

dups <- c(4600183, 4600069, 4600367, 4605732, 4600070)

dat1 <- import_pscis(workbook_name = 'pscis_phase1.xlsm') %>%
  filter(!my_crossing_reference %in% dups)

dat1b <- import_pscis(workbook_name = 'pscis_phase1b.xlsm')

dat2 <- import_pscis(workbook_name = 'pscis_phase2.xlsm')

dat3 <- import_pscis(workbook_name = 'pscis_reassessments.xlsm')

dat <- bind_rows(
  dat1,
  dat1b,
  dat2,
  dat3
) %>%
  distinct(.keep_all = T) %>%
  sf::st_as_sf(coords = c("easting", "northing"),
               crs = 26911, remove = F) %>%
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
# dbGetQuery(conn,
#            "SELECT table_name
#            FROM information_schema.tables
#            WHERE table_schema='ali'")
# # # # #
# # # # # ##list column names in a table
# dbGetQuery(conn,
#            "SELECT column_name,data_type
#            FROM information_schema.columns
#            WHERE table_name='crossings'")


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


##swapped out fish_passage.modelled_crossings_closed_bottom for bcfishpass.barriers_anthropogenic

##join the modelling data to our pscis submission info
dat_joined <- left_join(
  select(dat, misc_point_id, pscis_crossing_id,my_crossing_reference, source), ##traded pscis_crossing_id for my_crossing_reference
  dat_info,
  by = "misc_point_id"
) %>%
  mutate(downstream_route_measure = as.integer(downstream_route_measure))


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



# dat_info <- dbGetQuery(conn,
#                        "
#                                   SELECT a.misc_point_id, b.admin_area_abbreviation
#                                   FROM working.misc a
#                                   INNER JOIN
#                                   whse_legal_admin_boundaries.abms_municipalities_sp b
#                                   ON ST_Intersects(b.geom, ST_Transform(a.geometry,3005))
#                        ")

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

# ##burn it all to a csv so you can use it however you want
# df_joined2 %>% readr::write_csv(file = paste0(getwd(), '/data/bcfishpass-phase2.csv'))
#
# ##clean up the workspace
rm(dat, dat1, dat1b, dat2, dat3, dat_info, dat_joined, res)
#
# ##now we pull the data out whenever we want to make tables for the report
# dat <- readr::read_csv(file = paste0(getwd(), '/data/bcfishpass-phase2.csv'))

##this no longer works because we were using the fish_passage.modelled_crossings_closed_bottom and now we don't have the rd info
##make a tibble of the client names so you can summarize in the report
##we do not need to repeat this step but this is how we make a dat to paste into a kable in rmarkdown then paste tibble as a rstudio addin so we can
##populate the client_name_abb...

##we already did this but can do it again I guess.  you cut and paste the result into kable then back
##into here using addin for datapasta
# tab_rd_tenure_xref <- unique(dat$client_name) %>%
#   as_tibble() %>%
#   purrr::set_names(nm = 'client_name') %>%
#   mutate(client_name_abb = NA)

tab_rd_tenure_xref <- tibble::tribble(
                                                   ~client_name, ~client_name_abb,
                        "DISTRICT MANAGER ROCKY MOUNTAIN (DRM)",          "FLNR",
                                "CANADIAN FOREST PRODUCTS LTD.",        "Canfor",
                                                             NA,              NA
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

##we need to qa which are our modelled crossings at least for our phase 2 crossings
##I used this to populate the phase 2 spreadsheet I believe
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


##this is going to bring in Pscis data so we can see the pscis id's
get_this <- bcdc_tidy_resources('pscis-assessments') %>%
  filter(bcdata_available == T)  %>%
  pull(package_id)

dat <- bcdata::bcdc_get_data(get_this)



####-------raw data preserve---------------------------------------
##burn pscis as is as a record
# dat %>% readr::write_csv(file = paste0(getwd(), '/data/raw_input/pscis_bcdata.csv'))

##now if we want to skip this step we just load the csv like so
# dat <-



xref_pscis_my_crossing_modelled <- dat %>%
  purrr::set_names(nm = tolower(names(.))) %>%
  dplyr::filter(funding_project_number == "BCFP-003_phase1") %>%
  select(external_crossing_reference, stream_crossing_id) %>%
  mutate(external_crossing_reference = as.integer(external_crossing_reference)) %>%
  sf::st_drop_geometry()

dat_joined5 <- left_join(
  dat_joined4,
  xref_pscis_my_crossing_modelled,
  by = c('my_crossing_reference' = 'external_crossing_reference')
)

##we need to add our pscis info
dat_joined6 <- dat_joined5 %>%
  mutate(stream_crossing_id = as.numeric(stream_crossing_id)) %>%
  mutate(pscis_crossing_id = case_when(is.na(pscis_crossing_id) ~ stream_crossing_id,
                                       T ~ pscis_crossing_id))


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

dat <- dat_joined6 %>%
  select(crossing_id)

# load to database
sf::st_write(obj = dat, dsn = conn, Id(schema= "ali", table = "misc"))

dat_info <- dbGetQuery(conn,
"SELECT a.crossing_id, b.*
FROM ali.misc a
LEFT OUTER JOIN
ali.crossings b
ON a.crossing_id = b.modelled_crossing_id")

##here we find identical columns because we will want to remove them from one of the lists.
columns_to_remove <- intersect(names(dat_joined6), names(dat_info))
columns_to_keep <- "crossing_id"
columns_to_remove <- setdiff(columns_to_remove, columns_to_keep)  ##make sure to keep the joining column


##Lets keep the newest ones and remove from the old dataframe
dat_joined7 <- dat_joined6 %>%
  select(-all_of(columns_to_remove))

##join together the old and the new
dat_joined8 <- left_join(
  dat_joined7,
  dat_info,
  by = 'crossing_id'
) %>%
  distinct(pscis_crossing_id, my_crossing_reference, source, .keep_all = T)

dups <- dat_joined8 %>% group_by(pscis_crossing_id) %>%
  mutate(duplicated = n()>1) %>%
  filter(duplicated == T & !is.na(pscis_crossing_id))   ##this is not connected bc its an error with the geometry when its empty - feeds the top input though!!!
  # distinct(pscis_crossing_id, .keep_all = T) %>%
  # pull(my_crossing_reference)

##burn it all to a file we can use later
dat_joined8 %>% readr::write_csv(file = paste0(getwd(), '/data/bcfishpass-phase2.csv'))


