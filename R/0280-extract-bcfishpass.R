source('R/packages.R')
source('R/functions.R')
source('R/private_info.R')
source('R/0255-load-pscis.R')

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
           WHERE table_name='barriers_anthropogenic'")


# test <- dbGetQuery(conn, "SELECT * FROM bcfishpass.waterfalls")

dat <- pscis_all

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
  # dat_info,
  select(dat_info,misc_point_id:fcode_label, distance, crossing_id), ##geom keep only the road info and the distance to nearest point from here
  by = "misc_point_id"
) %>%
  sf::st_drop_geometry() ##distinct will pick up geometries!!!!!!


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
                     T ~ my_road_tenure))
  # rename(geom_modelled_crossing = geom)

# test <- dat_joined4 %>% filter(pscis_crossing_id == 197656)

##build tables to populate the pscis spreadsheets
pscis1_rd_tenure <- left_join(
  select(pscis_phase1, my_crossing_reference),
  select(dat_joined4, my_crossing_reference, my_road_tenure),
  by = 'my_crossing_reference'
) %>%
  distinct(my_crossing_reference, my_road_tenure) %>%
  tibble::rownames_to_column() %>%
  mutate(rowname = as.integer(rowname)) %>%
  mutate(rowname = rowname + 4) %>%
  readr::write_csv(file = paste0(getwd(), '/data/extracted_inputs/pscis1_rd_tenure.csv'))

pscis_reassessmeents_rd_tenure <- left_join(
  select(pscis_reassessments, pscis_crossing_id),
  select(dat_joined4, pscis_crossing_id, my_road_tenure),
  by = 'pscis_crossing_id'
) %>%
  distinct(pscis_crossing_id, my_road_tenure) %>%
  tibble::rownames_to_column() %>%
  mutate(rowname = as.integer(rowname)) %>%
  mutate(rowname = rowname + 4) %>%
  readr::write_csv(file = paste0(getwd(), '/data/extracted_inputs/pscis_reassessmeents_rd_tenure.csv'))

pscis_phase2_rd_tenure <- left_join(
  select(pscis_all %>% filter(source %like% 'phase2') %>% st_drop_geometry(), pscis_crossing_id),
  select(dat_joined4, pscis_crossing_id, my_road_tenure),
  by = 'pscis_crossing_id'
) %>%
  distinct(pscis_crossing_id, my_road_tenure) %>%
  tibble::rownames_to_column() %>%
  mutate(rowname = as.integer(rowname)) %>%
  mutate(rowname = rowname + 4) %>%
  readr::write_csv(file = paste0(getwd(), '/data/extracted_inputs/pscis_phase2_rd_tenure.csv'))

pscis_rd <- dat_joined4 %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(my_road_class = case_when(is.na(road_class) & !is.na(file_type_description) ~
                                            file_type_description,
                                          T ~ road_class)) %>%
  dplyr::mutate(my_road_class = case_when(is.na(my_road_class) & !is.na(owner_name) ~
                                            'rail',
                                          T ~ my_road_class)) %>%
  dplyr::mutate(my_road_surface = case_when(is.na(road_surface) & !is.na(file_type_description) ~
                                              'loose',
                                            T ~ road_surface)) %>%
  dplyr::mutate(my_road_surface = case_when(is.na(my_road_surface) & !is.na(owner_name) ~
                                              'rail',
                                            T ~ my_road_surface)) %>%
  readr::write_csv(file = paste0(getwd(), '/data/extracted_inputs/pscis_rd.csv'))


####----tab cost multipliers for road surface-----
tab_cost_rd_mult <- pscis_rd %>%
  select(my_road_class, my_road_surface) %>%
  # mutate(road_surface_mult = NA_real_, road_class_mult = NA_real_) %>%
  mutate(road_class_mult = case_when(my_road_class == 'local' ~ 4,
                                     my_road_class == 'collector' ~ 4,
                                     my_road_class == 'arterial' ~ 10,
                                     my_road_class == 'highway' ~ 10,
                                     my_road_class == 'rail' ~ 5,
                                     T ~ 1))  %>%
  mutate(road_surface_mult = case_when(my_road_surface == 'loose' |
                                         my_road_surface == 'rough' ~
                                         1,
                                       T ~ 2)) %>%
  # mutate(road_type_mult = road_class_mult * road_surface_mult) %>%
  mutate(cost_m_1000s_bridge = road_surface_mult * road_class_mult * 12.5,
         cost_embed_cv = road_surface_mult * road_class_mult * 25) %>%
  # mutate(cost_1000s_for_10m_bridge = 10 * cost_m_1000s_bridge) %>%
  distinct( .keep_all = T) %>%
  tidyr::drop_na() %>%
  arrange(cost_m_1000s_bridge, my_road_class) %>%
  readr::write_csv(file = paste0(getwd(), '/data/extracted_inputs/tab_cost_rd_mult.csv'))

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

# dat_unique <- dat %>% distinct(pscis_crossing_id, .keep_all = T)

# load to database
sf::st_write(obj = dat, dsn = conn, Id(schema= "ali", table = "misc"))


##this is the way we did it before but seems brittle as bcfishpass is new than fish_passage schema
##this time we will pull bcfishpass raw and try to join based on the id and see if they are all the same
# dat_info <- dbGetQuery(conn,
# "SELECT a.crossing_id, b.*
# FROM ali.misc a
# LEFT OUTER JOIN
# bcfishpass.crossings b
# ON a.crossing_id = b.modelled_crossing_id")

dat_info <- dbGetQuery(conn, "SELECT
  a.misc_point_id, a. pscis_crossing_id,
  b.*,
  ST_Distance(ST_Transform(a.geometry,3005), b.geom) AS distance
FROM
  ali.misc AS a
CROSS JOIN LATERAL
  (SELECT *
   FROM bcfishpass.barriers_anthropogenic
   ORDER BY
     a.geometry <-> geom
   LIMIT 1) AS b")

##test the join to see if we get the same matched points each time
test <- left_join(
  select(dat_joined4, misc_point_id, crossing_id, distance),
  select(dat_info, misc_point_id, modelled_crossing_id, distance2 = distance),
  by = c('misc_point_id', 'crossing_id' = 'modelled_crossing_id')
) %>%
  mutate(distance_diff = distance - distance2) %>%
  filter(distance_diff > 10)

##this looks good with all the same match and only 1 crossing more than 10m away but it is a good match regardless

##here we find identical columns because we will want to remove them from one of the lists.
columns_to_remove <- intersect(names(dat_joined4), names(dat_info))
columns_to_keep <- c("misc_point_id", "pscis_crossing_id") #crossing_id
columns_to_remove <- setdiff(columns_to_remove, columns_to_keep)  ##make sure to keep the joining column


##Lets keep the newest ones and remove from the old dataframe

dat_joined5 <- dat_joined4 %>%
  # sf::st_drop_geometry() %>% ##the distinct was picking up the geometry!!!!!!!
  # distinct(pscis_crossing_id, .keep_all = T) %>% ##need to get rid of duplicates
  select(-all_of(columns_to_remove)) %>%
  select(misc_point_id, everything())

# identical(dat_joined5[21,3],dat_joined5[22,3])

##join together the old and the new
dat_joined6 <- left_join(
  dat_joined5,
  select(dat_info, -pscis_crossing_id),
  by = 'misc_point_id' #c('crossing_id' = 'modelled_crossing_id')
)
  # distinct(pscis_crossing_id, my_crossing_reference, source, .keep_all = T)

dups <- dat_joined6 %>% group_by(pscis_crossing_id) %>%
  mutate(duplicated = n()>1) %>%
  filter(duplicated == T & !is.na(pscis_crossing_id))   ##this is not connected bc its an error with the geometry when its empty - feeds the top input though!!!
  # distinct(pscis_crossing_id, .keep_all = T) %>%
  # pull(my_crossing_reference)


##burn it all to a file we can use later
dat_joined6 %>% readr::write_csv(file = paste0(getwd(), '/data/extracted_inputs/bcfishpass.csv'))


