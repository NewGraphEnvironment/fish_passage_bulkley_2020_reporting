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

# dat2 <- import_pscis(workbook_name = 'pscis_phase2.xlsm')

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


##swapped out fish_passage.modelled_crossings_closed_bottom for bcfishpass.barriers_anthropogenic

##join the modelling data to our pscis submission info
dat_joined <- left_join(
  select(dat, misc_point_id, pscis_crossing_id, my_crossing_reference, source), ##traded pscis_crossing_id for my_crossing_reference
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
rm(dat_info, dat_joined, res)
#
# ##now we pull the data out whenever we want to make tables for the report
# dat <- readr::read_csv(file = paste0(getwd(), '/data/bcfishpass-phase2.csv'))

##this no longer works because we were using the fish_passage.modelled_crossings_closed_bottom and now we don't have the rd info
##make a tibble of the client names so you can summarize in the report
##we do not need to repeat this step but this is how we make a dat to paste into a kable in rmarkdown then paste tibble as a rstudio addin so we can
##populate the client_name_abb...

##we already did this but can do it again I guess.  you cut and paste the result into kable then back
##into here using addin for datapasta
tab_rd_tenure_xref <- unique(dat_joined2$client_name) %>%
  as_tibble() %>%
  purrr::set_names(nm = 'client_name') %>%
  mutate(client_name_abb = NA)

tab_rd_tenure_xref <- tibble::tribble(
                                           ~client_name,        ~client_name_abb,
                                                     NA,                      NA,
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
##we need to qa which are our modelled crossings at least for our phase 2 crossings



