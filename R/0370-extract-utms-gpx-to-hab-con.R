# source('R/packages.R')
source('R/0320-tables-phase2.R')


##extract the site utms by matching the times of the gpx tracks with the field
##notes yo


# sf::read_sf("./data/habitat_electrofishing_tracks.gpx", layer = "track_points")

##bring in the tracks ##time_cor = time + lubridate::hours(1)) to add an hour
utm_zone = 9
gps1<- sf::read_sf(paste0(getwd(), "/data/inputs_raw/field_2020_kyle.gpx"), layer = "track_points") %>%
  st_transform(crs = 26900 + utm_zone) %>%
  mutate(gps_id = 1, utm_zone = utm_zone)



gps2<- sf::read_sf(paste0(getwd(), "/data/inputs_raw/field_2020_al.gpx"), layer = "track_points") %>%
  st_transform(crs = 26900 + utm_zone) %>%
  mutate(gps_id = 2, utm_zone = utm_zone)



hab_gps <- bind_rows(
  gps1,
  gps2
) %>%
  mutate(easting = st_coordinates(geometry)[,1],
         northing = st_coordinates(geometry)[,2]) %>%
  select(gps_id, track_seg_point_id:time, utm_zone, easting, northing, geometry) %>%
  tibble::rownames_to_column()



##lets extract out site top/bottom from the ef_time sheet
####--------------bring in the habitat and fish data------------------
path = (path = "./data/inputs_raw/hab_con_ef_time.xlsx")

hab_con_gps <-  readxl::excel_sheets(path = path) %>%
  purrr::set_names() %>%
  purrr::map(read_excel,
             path = path,
             .name_repair = janitor::make_clean_names) %>%
  purrr::set_names(janitor::make_clean_names(names(.)))
  # purrr::map(at_trim_xlsheet2) %>% #moved to functions from https://github.com/NewGraphEnvironment/altools to reduce dependencies
  # purrr::map(plyr::colwise(type.convert))

hab_gps_xref <- left_join(
  hab_con_gps %>%
    pluck('xref_times'),
  hab_con_gps %>%
    pluck('xref_gps'),
  by = 'gps_id'
) %>%
  mutate(time = format(lubridate::ymd_hms(time), "%H:%M:%S"),
         # date = lubridate::as_date(date),
         date_time = as.POSIXct(paste(date, time), format="%Y-%m-%d %H:%M:%S")) %>%
  tibble::rownames_to_column() %>%
  mutate(date_time = date_time + as.difftime(gps_correction_s, units="secs") ##fix the times using your gps_correction_s column
         )



hab_con_gps_idx <- hab_gps %>%
  select(rowname, time, gps_id) %>%
  filter(gps_id == 1) %>%
  select(-gps_id) %>%
  data.table()

hab_gps_xref_idx <- hab_gps_xref %>%
  select(rowname, date_time, gps_id) %>%
  filter(gps_id == 1) %>%
  select(-gps_id) %>%
  data.table()

setkey( hab_con_gps_idx, time )
setkey( hab_gps_xref_idx, date_time )

combined <- hab_con_gps_idx[ hab_gps_xref_idx, roll = "nearest" ]

hab_gps_matched1 <- left_join(
  select(combined, i.rowname, rowname),
  select(hab_gps, rowname, time_gps = time, utm_zone:northing),
  by = 'rowname'
)


##now lets change the gps id
hab_con_gps_idx <- hab_gps %>%
  select(rowname, time, gps_id) %>%
  filter(gps_id == 2) %>%
  select(-gps_id) %>%
  data.table()

hab_gps_xref_idx <- hab_gps_xref %>%
  select(rowname, date_time, gps_id) %>%
  filter(gps_id == 2) %>%
  select(-gps_id) %>%
  data.table()

setkey( hab_con_gps_idx, time )
setkey( hab_gps_xref_idx, date_time )

combined <- hab_con_gps_idx[ hab_gps_xref_idx, roll = "nearest" ]

hab_gps_matched2 <- left_join(
  select(combined, i.rowname, rowname),
  select(hab_gps, rowname, time_gps = time, utm_zone:northing),
  by = 'rowname'
)

##join the two gps units
hab_gps_matched_prep <- bind_rows(
  hab_gps_matched1,
  hab_gps_matched2
) %>%
  select(-rowname) %>%
  sf::st_as_sf() %>%
  st_drop_geometry()

##join back to the original data frame
hab_gps_matched <- left_join(
  hab_gps_xref,
  hab_gps_matched_prep,
  by = c('rowname' = 'i.rowname')
) %>%
  mutate(qa_east_check = easting - qa_utm_easting,
         qa_north_check = northing - qa_utm_northing)


##clean up the workspace
rm(combined, gps1, gps2, hab_con_gps_idx, hab_gps_matched1, hab_gps_matched2,
   hab_gps_matched_prep, hab_gps_xref_idx)

##make a csv to copy paste our
site_csv <- hab_gps_matched %>%
  filter(loc == 'start') %>%
  select(site, utm_zone:northing) %>%
  mutate(easting = round(easting, 0),
         northing = round(northing, 0)) %>%
  readr::write_csv(file = paste0(getwd(), '/data/inputs_raw/hab_con_site_utms.csv'))


##############################################this is how we do it with the xref_waypoints_utm sheet of the inputs

##waypoints
gps1_wp<- sf::read_sf(paste0(getwd(), "/data/inputs_raw/field_2020_kyle.gpx"), layer = "waypoints") %>%
  st_transform(crs = 26900 + utm_zone) %>%
  mutate(gps_id = 1, utm_zone = utm_zone,
         name = as.numeric(name))


gps2_wp<- sf::read_sf(paste0(getwd(), "/data/inputs_raw/field_2020_al.gpx"), layer = "waypoints") %>%
  st_transform(crs = 26900 + utm_zone) %>%
  mutate(gps_id = 2, utm_zone = utm_zone,
         name = as.numeric(name))

gps_wp <- bind_rows(
  gps1_wp,
  gps2_wp
) %>%
  mutate(gps_easting = st_coordinates(geometry)[,1],
         gps_northing = st_coordinates(geometry)[,2]) %>%
  select(name, utm_zone, gps_easting, gps_northing) %>%
  sf::st_drop_geometry()

##get the data from the sheet
hab_con_wp_xref <- left_join(
  hab_con_gps %>%
    pluck('xref_waypoints_utms'),
  hab_con_gps %>%
    pluck('xref_gps'),
  by = 'gps_id'
) %>%
  # mutate(time = format(lubridate::ymd_hms(time), "%H:%M:%S"),
  #        # date = lubridate::as_date(date),
  #        date_time = as.POSIXct(paste(date, time), format="%Y-%m-%d %H:%M:%S")) %>%
  tibble::rownames_to_column()
##find nearest utm to each of the input times based on best match of time
##https://stackoverflow.com/questions/39282749/r-how-to-join-two-data-frames-by-nearest-time-date

##join the data from gps to get hte waypoints
gps_wp_xref <- left_join(
  hab_con_wp_xref,
  gps_wp,
  by = c('wp' = 'name')
)

##join to our original input sheet
hab_con_wp_xref <- left_join(
  hab_loc %>% mutate(utm_easting = as.numeric(utm_easting), utm_northing = as.numeric(utm_northing)),
  gps_wp_xref,
  by = c('alias_local_name' = 'local_name')
) %>%
  mutate(utm_easting = case_when(
    is.na(utm_easting) ~ easting,
    T ~ utm_easting),
    utm_northing = case_when(
      is.na(utm_northing) ~ northing,
      T ~ utm_northing),
    utm_easting = case_when(
      is.na(utm_easting) ~ gps_easting,
      T ~ utm_easting),
    utm_northing = case_when(
      is.na(utm_northing) ~ gps_northing,
      T ~ utm_northing)) %>%
  mutate(utm_easting = round(utm_easting,0),
         utm_northing = round(utm_northing,0)) %>%
  select(reference_number:alias_local_name, utm_easting, utm_northing, easting, northing, gps_easting, gps_northing) %>%
  readr::write_csv(file = paste0(getwd(), '/data/extracted_inputs/utm_inputs_wp.csv'), na = "")

