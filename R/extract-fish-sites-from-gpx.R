source('R/packages.R')


##extract the site utms by matching the times of the gpx tracks with the field
##notes yo


# sf::read_sf("./data/habitat_electrofishing_tracks.gpx", layer = "track_points")

##bring in the tracks ##time_cor = time + lubridate::hours(1)) to add an hour
utm_zone = 11
gps1<- sf::read_sf(paste0(getwd(), "/data/raw_input/elk_field_2020_kyle.gpx"), layer = "track_points") %>%
  st_transform(crs = 26900 + utm_zone) %>%
  mutate(gps_id = 1, utm_zone = utm_zone)



gps2<- sf::read_sf(paste0(getwd(), "/data/raw_input/elk_field_2020_al.gpx"), layer = "track_points") %>%
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
path = (path = "./data/habitat_confirmations_ef_time.xlsx")

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


##find nearest utm to each of the input times based on best match of time
##https://stackoverflow.com/questions/39282749/r-how-to-join-two-data-frames-by-nearest-time-date

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
  readr::write_csv(file = paste0(getwd(), '/data/raw_input/habitat_confirmation_site_utms.csv'))

