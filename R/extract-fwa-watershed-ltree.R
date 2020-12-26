##try to get the watershed codes with a simple ltree query

source('R/packages.R')
source('R/functions.R')
source('R/tables-phase2.R')

wshed_input <- bcfishpass_phase2 %>%
  filter(source %like% 'phase2' &
           !is.na(modelled_crossing_id)) %>%
  select(-geom, -geometry) %>%
  mutate(downstream_route_measure_chk = case_when(
    downstream_route_measure < 200 ~ 20,
    T ~ NA_real_
  ),
  downstream_route_measure = case_when(
    !is.na(downstream_route_measure_chk) ~ downstream_route_measure_chk,
    T ~ downstream_route_measure
  ))


dat <- wshed_input %>%
  filter(!is.na(modelled_crossing_id)) %>%
  # mutate(ltree_in = paste0(localcode_ltree, '.*{1}'))  ##this one is maybe a bit better
  mutate(ltree_in = paste0(localcode_ltree, '.*')) ##this is hab_wshds_ltree


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



# dat_info <- dbGetQuery(conn,
#                        "
#                                   SELECT a.*
#                                   FROM whse_basemapping.fwa_watersheds_poly a
#                                   WHERE a.localcode_ltree ~ '300.625474.584724.233916.539602.*{1}'
#                        ")
#
# q <- "
#                                   SELECT a.*
#                                   FROM whse_basemapping.fwa_watersheds_poly a
#                                   WHERE a.localcode_ltree ~ '300.625474.584724.233916.539602.*{1}';
                       # "
##trying
ltrees <-  dat %>%
  pull(ltree_in)
  # unique() %>%
  # as_vector() %>%
  # na.omit()

# localcode_ltree <- dat %>%
#   pull(localcode_ltree)

##trying
ids <-  dat %>%
  pull(pscis_crossing_id)
  # unique() %>%
  # as_vector() %>%
  # na.omit()

query_list <- paste0("SELECT * FROM whse_basemapping.fwa_watersheds_poly WHERE localcode_ltree ~ '", ltrees, "';")
# query_list <- paste0("SELECT * FROM whse_basemapping.fwa_watersheds_poly WHERE localcode_ltree >= '", localcode_ltree, "';")

# try <- query_list[1]
#
#
# test <- st_read(conn, query = try) %>%
#   st_union() %>%
#   sf::st_as_sf() %>%
#   rename(geom = x)


#funciton to generate wsheds
get_wshds <- function(q){
  st_read(conn, query = q) %>%
  st_union() %>%
  sf::st_as_sf() %>%
  rename(geom = x)
}


# wshds <- lapply(query_list, get_wshds)
wshds <- query_list %>%
  map(get_wshds) %>%
  purrr::set_names(nm = ids) %>%
  discard(function(x) nrow(x) == 0)  ##remove zero row tibbles with https://stackoverflow.com/questions/49696392/remove-list-elements-that-are-zero-row-tibbles


make_tibs <- function(dat){
  as_tibble(dat)
}

wshds2 <- wshds %>%
  map_df(make_tibs)

wshds3 <- bind_cols(
  as_tibble(names(wshds)),
  wshds2
) %>%
  st_as_sf() %>%
  rename(pscis_crossing_id = value) %>%
  mutate(area_km = round(sf::st_area(geom)/1000000,1),
         area_km = units::drop_units(area_km))
  # st_transform(crs = 3005)

##visualize your outputs
ggplot2::ggplot() +
  # ggplot2::geom_sf(data = wshed_simple, lwd = 0.15, fill = 'red', alpha = 0.1) +
  ggplot2::geom_sf(data = wshds3 %>% filter(pscis_crossing_id == 50181), lwd = 0.15, fill = 'steelblue', alpha = 0.5)
  # ggplot2::geom_sf(data = yakoun, lwd = 0.15)

##add to the geopackage
wshds3 %>%
  # sf::st_write(paste0("./data/", 'fishpass_mapping', ".gpkg"), 'hab_wshds_ltree_up1', append = F) %>% ##might want to f the append....
  sf::st_write(paste0("./data/", 'fishpass_mapping', ".gpkg"), 'hab_wshds_ltree', append = F) ##might want to f the append....

##burned to a kml so we can easily add elevation info
st_write(wshds3, append = TRUE, driver = 'kml', dsn = "data/raw_input/hab_wshds_ltree.kml")
# st_write(wshds3, append = TRUE, driver = 'kml', dsn = "data/raw_input/wsheds_up1.kml")

dbDisconnect(conn = conn)


