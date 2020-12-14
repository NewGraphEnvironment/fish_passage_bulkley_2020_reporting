##i want to make a table that looks just like the field data collection form
source('R/functions.R')
source('R/packages.R')
source('R/private_info.R')

##make a dataframe to pull info from the db
##we should probably break each row out and determine the crs by the utm_zone attribute
df <- import_pscis(workbook_name = 'pscis_phase1.xlsm') %>%
  sf::st_as_sf(coords = c("easting", "northing"),
               crs = 26911, remove = F) %>%
  sf::st_transform(crs = 3005) ##convert to match the bcbarriers format


##get the road info from the database
conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = dbname,
  host = host,
  port = port,
  user = user,
  password = password
)


##list tables in a schema
# dbGetQuery(conn,
#            "SELECT table_name
#            FROM information_schema.tables
#            WHERE table_schema='ali'")
# dbSendQuery(conn, paste0("DROP TABLE IF EXISTS ", "ali.test",";"))

# add a unique id - we could just use the reference number
df$misc_point_id <- seq.int(nrow(df))

# dbSendQuery(conn, paste0("CREATE SCHEMA IF NOT EXISTS ", "test_hack",";"))
# load to database
sf::st_write(obj = df, dsn = conn, Id(schema= "ali", table = "misc"))



# sf doesn't automagically create a spatial index or a primary key
res <- dbSendQuery(conn, "CREATE INDEX ON ali.misc USING GIST (geometry)")
dbClearResult(res)
res <- dbSendQuery(conn, "ALTER TABLE ali.misc ADD PRIMARY KEY (misc_point_id)")
dbClearResult(res)

df_info <- dbGetQuery(conn, "SELECT
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
df_joined <- left_join(
  select(df, misc_point_id, my_crossing_reference),
  df_info,
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

# load to database
sf::st_write(obj = df, dsn = conn, Id(schema= "working", table = "misc"))



dat_info <- dbGetQuery(conn,
                       "
                                  SELECT a.misc_point_id, b.admin_area_abbreviation
                                  FROM working.misc a
                                  INNER JOIN
                                  whse_legal_admin_boundaries.abms_municipalities_sp b
                                  ON ST_Intersects(b.geom, ST_Transform(a.geometry,3005))
                       ")

dbDisconnect(conn = conn)

##add the municipality info
df_joined2 <- left_join(
  df_joined,
  dat_info,
  by = "misc_point_id"
)

# ##burn it all to a csv so you can use it however you want
# df_joined2 %>% readr::write_csv(file = paste0(getwd(), '/data/bcfishpass.csv'))
#
# ##clean up the workspace
# rm(dat, dat_info, df_joined, res)
#
# ##now we pull the data out whenever we want to make tables for the report
# dat <- readr::read_csv(file = paste0(getwd(), '/data/bcfishpass.csv'))


##make a tibble of the client names so you can summarize in the report
##we do not need to repeat this step but this is how we make a dat to paste into a kable in rmarkdown then paste tibble as a rstudio addin so we can
##populate the client_name_abb...
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
df_joined3 <- left_join(
  df_joined2,
  tab_rd_tenure_xref,
  by = 'client_name'
)

##make a dat to make it easier to see so we can summarize the road info we might want to use
dat_rd_sum <- df_joined3 %>%
  select(my_crossing_reference, crossing_id, distance, road_name_full,
         road_class, road_name_full, road_surface, file_type_description, forest_file_id,
         client_name, client_name_abb, map_label, owner_name, admin_area_abbreviation)


##make a dat to make it easier to see so we can summarize the road info we might want to use
df_joined4 <- df_joined3 %>%
  mutate(admin_area_abbreviation = case_when(
    is.na(admin_area_abbreviation) ~ 'MoTi',
    T ~ admin_area_abbreviation),
    my_road_tenure =
      case_when(!is.na(client_name_abb) ~ paste0(client_name_abb, ' ', forest_file_id),
                !is.na(road_class) ~ paste0(admin_area_abbreviation, ' ', road_class),
                !is.na(owner_name) ~ owner_name)) %>%
  mutate(my_road_tenure =
           case_when(distance > 100 ~ 'Unknown',  ##we need to get rid of the info for the ones that are far away
                     T ~ my_road_tenure))

##burn it all to a file we can use later
df_joined4 %>% readr::write_csv(file = paste0(getwd(), '/data/bcfishpass.csv'))
