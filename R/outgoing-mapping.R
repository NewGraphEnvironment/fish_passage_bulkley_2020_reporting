# source('R/packages.R')
# source('R/functions.R')
source('R/0320-tables-phase2.R')
# source('R/tables.R')

##make your geopackage for mapping
make_geopackage <- function(dat, gpkg_name = 'fishpass_mapping', utm_zone = 9){
  nm <-deparse(substitute(dat))
  dat %>%
    sf::st_as_sf(coords = c("utm_easting", "utm_northing"), crs = 26900 + utm_zone, remove = F) %>%
    st_transform(crs = 3005) %>%
    sf::st_write(paste0("./data/", gpkg_name, ".gpkg"), nm, delete_layer = TRUE)
}


make_geopackage(dat = hab_fish_collect)
make_geopackage(dat = hab_features)
make_geopackage(dat = hab_site_priorities)
# make_geopackage(dat = phase1_priorities2)

##we do this manually since the
phase1_priorities %>%
  st_transform(crs = 3005) %>%
  sf::st_write(paste0("./data/", 'fishpass_mapping', ".gpkg"), 'phase1_priorities', delete_layer = TRUE)



##add the tracks
sf::read_sf("./data/habitat_confirmation_tracks.gpx", layer = "tracks") %>%
  sf::st_write(paste0("./data/", 'fishpass_mapping', ".gpkg"), 'hab_tracks', append = TRUE)

##study area watersheds
conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "postgis",
  host = "localhost",
  port = "5432",
  user = "postgres",
  password = "postgres"
)

dbGetQuery(conn,
           "SELECT column_name,data_type
           FROM information_schema.columns
           WHERE table_name='fwa_watershed_groups_poly'")

##here is the flathead
wshd_study_areas <- st_read(conn,
                           query = "SELECT * FROM whse_basemapping.fwa_watershed_groups_poly a
                               WHERE a.watershed_group_code  = 'BULK'
                           OR a.watershed_group_code  = 'MORR'"
)

wshd_study_areas %>%
  # select(-wscode_ltree) %>%
  st_cast("POLYGON") %>%
  sf::st_write(paste0("./data/", 'fishpass_mapping', ".gpkg"), 'wshd_study_areas', append = F)

dbDisconnect(conn = conn)

##going to grab the watershed for the riddeck ck crossing


wshd_reddick <- fwapgr::fwa_watershed_at_measure(blue_line_key = 360878896, downstream_route_measure = 1313.57027395708)

wshd_reddick %>%
  sf::st_write(paste0("./data/", 'fishpass_mapping', ".gpkg"), 'wshd_reddick', append = F)

st_write(wshd_reddick, append = TRUE, driver = 'kml', dsn = "data/extracted_inputs/wshd_reddick.kml")

# ggplot2::ggplot() +
#   ggplot2::geom_sf(data = wshd_reddick, lwd = 0.15, fill = 'steelblue', alpha = 0.5)

####------------add the watersheds-------------------------

##having the watersheds derived is nice so lets try
##make a function to retrieve the watershed info
# get_watershed <- function(dat){
#   mapply(fwapgr::fwa_watershed_at_measure,
#          blue_line_key = dat$blue_line_key,
#          downstream_route_measure = dat$downstream_route_measure,
#          SIMPLIFY = F) %>%
#     purrr::set_names(nm = dat$pscis_crossing_id) %>%
#     discard(function(x) nrow(x) == 0) %>% ##remove zero row tibbles with https://stackoverflow.com/questions/49696392/remove-list-elements-that-are-zero-row-tibbles
#     data.table::rbindlist(idcol="pscis_crossing_id") %>%
#     distinct(pscis_crossing_id, .keep_all = T) %>% ##in case there are duplicates we should get rid of
#     st_as_sf()
# }


# ##for each site grab a blueline key and downstream route measure
# hab_site_priorities2 <- hab_site_priorities %>%
#   mutate(srid = as.integer(26911))
#
# hab_site_fwa_index <- mapply(fwapgr::fwa_index_point,
#                              x = hab_site_priorities2$utm_easting,
#                              y = hab_site_priorities2$utm_northing,
#                              srid = hab_site_priorities2$srid,
#                              SIMPLIFY = F) %>%
#   purrr::set_names(nm = hab_site_priorities2$alias_local_name) %>%
#   data.table::rbindlist(idcol="alias_local_name") %>%
#   st_as_sf()

# ##now lets get our watersheds
# hab_site_fwa_wshds <- get_watershed(dat = hab_site_fwa_index)

##filter only phase 2 sites that were qa'd to a modelled crossing thorugh
# wshed_input <- bcfishpass_phase2 %>%
#   filter(source %like% 'phase2' &
#            !is.na(modelled_crossing_id)) %>%
#   select(-geom, -geometry) %>%
#   mutate(downstream_route_measure_chk = case_when(
#     downstream_route_measure < 200 ~ 20,
#     T ~ NA_real_
#   ),
#   downstream_route_measure = case_when(
#     !is.na(downstream_route_measure_chk) ~ downstream_route_measure_chk,
#     T ~ downstream_route_measure
#   ))
#
#
# ##now lets get our watersheds
# hab_site_fwa_wshds <- left_join(
#   get_watershed(dat = wshed_input) %>% mutate(pscis_crossing_id = as.integer(pscis_crossing_id)),
#   select(wshed_input, -localcode_ltree, -wscode_ltree),
#   by = 'pscis_crossing_id'
# )
#
# ##add to the geopackage
# hab_site_fwa_wshds %>%
#   sf::st_write(paste0("./data/", 'fishpass_mapping', ".gpkg"), 'hab_wshds', append = F) ##might want to f the append....





