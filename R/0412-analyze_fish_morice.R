source('R/functions.R')
source('R/packages.R')
source('R/private_info.R')

##will just run again for the Morice
#!!!!!!!!!all the width data is gone so we need to use our archived version!!!!!!!!!!!!Scroll down to the hashed out section
##get the road info from the database
conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = dbname,
  host = host,
  port = port,
  user = user,
  password = password
)

# # ##list tables in a schema
dbGetQuery(conn,
           "SELECT table_name
           FROM information_schema.tables
           WHERE table_schema='bcfishpass'")
# # # # #
# # # # # ##list column names in a table
dbGetQuery(conn,
           "SELECT column_name,data_type
           FROM information_schema.columns
           WHERE table_schema = 'bcfishpass'
           and table_name='streams'")

dbGetQuery(conn,
           "SELECT column_name,data_type
           FROM information_schema.columns
           WHERE table_name='fiss_fish_obsrvtn_events_sp'")

##have a look at the channel width measured
query = 'SELECT * FROM bcfishpass.channel_width_measured'

channel_width_measured <- st_read(conn, query = query)

##try the calculation from davies_etal2007ModelingStream
channel_width_measured2 <- channel_width_measured %>%
  mutate(cw_daviesetal2007 = 0.042*(upstream_area_ha/100)^0.48*(map/10)*0.74) %>%
  mutate(diff = cw_daviesetal2007 - channel_width_measured)

##get some stats for
query = "SELECT
  fish_observation_point_id,
  s.gradient,
  s.stream_order,
  s.stream_magnitude,
  s.upstream_area_ha,
  s.channel_width,
  e.species_code,
  round((ST_Z((ST_Dump(ST_LocateAlong(s.geom, e.downstream_route_measure))).geom))::numeric) as elevation
FROM whse_fish.fiss_fish_obsrvtn_events_sp e
INNER JOIN bcfishpass.streams s
ON e.linear_feature_id = s.linear_feature_id
WHERE e.watershed_group_code = 'MORR';"

##e.species_code = 'WCT' AND
# unique(fiss_sum$species_code)
species_of_interest <- c('BT', 'CH', 'CM', 'CO', 'CT', 'DV', 'PK', 'RB','SK', 'ST')


fiss_sum <- st_read(conn, query = query) %>%
  filter(species_code %in% species_of_interest)


#!!!!!!!!!all the width data is gone so we need to use our archived version!!!!!!!!!!!!


##burn it all to a file we can use later
# fiss_sum %>% readr::write_csv(file = paste0(getwd(), '/data/extracted_inputs/fiss_sum.csv'))
# fiss_sum <- readr::read_csv(file = paste0(getwd(), '/data/extracted_inputs/fiss_sum.csv'))
#
# ##lets put it in the sqlite for safekeeping
# conn <- rws_connect("data/bcfishpass.sqlite")
# rws_list_tables(conn)
# rws_write(fiss_sum, exists = F, delete = TRUE,
#           conn = conn, x_name = "fiss_sum")
# rws_list_tables(conn)
# rws_disconnect(conn)


######################################################################################################################
######################################################################################################################
#########################################START HERE#########################################################
######################################################################################################################
######################################################################################################################

fiss_sum_grad_prep1 <- fiss_sum %>%
  mutate(Gradient = case_when(
    gradient < .03 ~ '0 - 3 %',
    gradient >= .03 &  gradient < .05 ~ '03 - 5 %',
    gradient >= .05 &  gradient < .08 ~ '05 - 8 %',
    gradient >= .08 &  gradient < .15 ~ '08 - 15 %',
    gradient >= .15 &  gradient < .22 ~ '15 - 22 %',
    gradient >= .22  ~ '22+ %')) %>%
  mutate(gradient_id = case_when(
    gradient < .03 ~ 3,
    gradient >= .03 &  gradient < .05 ~ 5,
    gradient >= .05 &  gradient < .08 ~ 8,
    gradient >= .08 &  gradient < .15 ~ 15,
    gradient >= .15 &  gradient < .22 ~ 22,
    gradient >= .22  ~ 99))

fiss_sum_grad_prep2 <- fiss_sum_grad_prep1 %>%
  group_by(species_code) %>%
  summarise(total_spp = n())

fiss_sum_grad_prep3 <- fiss_sum_grad_prep1 %>%
  group_by(species_code, Gradient, gradient_id)  %>%
  summarise(Count = n())

fiss_sum_grad <- left_join(
  fiss_sum_grad_prep3,
  fiss_sum_grad_prep2,
  by = 'species_code'
) %>%
  mutate(Percent = round(Count/total_spp * 100, 0))

##save this for the report
##burn it all to a file we can use later
# fiss_sum_grad %>% readr::write_csv(file = paste0(getwd(), '/data/extracted_inputs/fiss_sum_grad.csv'))


plot_grad <- fiss_sum_grad %>%
  filter(gradient_id != 99) %>%
  ggplot(aes(x = Gradient, y = Percent)) +
  geom_bar(stat = "identity")+
  facet_wrap(~species_code, ncol = 2)+
  theme_bw(base_size = 11)+
  labs(x = "Average Stream Gradient", y = "Occurrences (%)")
plot_grad

#############################CHANNEL WIDTH######################################

fiss_sum_width_prep1 <- fiss_sum %>%
  mutate(Width = case_when(
    channel_width < 2 ~ '0 - 2m',
    channel_width >= 2 &  channel_width < 4 ~ '02 - 04m',
    channel_width >= 4 &  channel_width < 6 ~ '04 - 06m',
    channel_width >= 6 &  channel_width < 10 ~ '06 - 10m',
    channel_width >= 10 &  channel_width < 15 ~ '10 - 15m',
    # channel_width >= 15 &  channel_width < 20 ~ '15 - 20m',
    channel_width >= 15  ~ '15m+')) %>%
  mutate(width_id = case_when(
    channel_width < 2 ~ 2,
    channel_width >= 2 &  channel_width < 4 ~ 4,
    channel_width >= 4 &  channel_width < 6 ~ 6,
    channel_width >= 6 &  channel_width < 10 ~ 10,
    channel_width >= 10 &  channel_width < 15 ~ 15,
    # channel_width >= 15 &  channel_width < 20 ~ 20,
    channel_width >= 15  ~ 99))

fiss_sum_width_prep2 <- fiss_sum_width_prep1 %>%
  group_by(species_code) %>%
  summarise(total_spp = n())

fiss_sum_width_prep3 <- fiss_sum_width_prep1 %>%
  group_by(species_code, Width, width_id)  %>%
  summarise(Count = n())

fiss_sum_width <- left_join(
  fiss_sum_width_prep3,
  fiss_sum_width_prep2,
  by = 'species_code'
) %>%
  mutate(Percent = round(Count/total_spp * 100, 0))

##save this for the report
##burn it all to a file we can use later
# fiss_sum_width %>% readr::write_csv(file = paste0(getwd(), '/data/extracted_inputs/fiss_sum_width.csv'))



plot_width <- fiss_sum_width %>%
  filter(!is.na(width_id)) %>%
  ggplot(aes(x = Width, y = Percent)) +
  geom_bar(stat = "identity")+
  facet_wrap(~species_code, ncol = 2)+
  ggdark::dark_theme_bw(base_size = 11)+
  labs(x = "Channel Width", y = "Occurrences (%)")
plot_width


#############WATERSHED SIZE#################################################
# bin_1 <- min(fiss_sum$upstream_area_ha, na.rm = TRUE)
fiss_sum_wshed_filter <- fiss_sum %>%
  filter(upstream_area_ha < 10000)

max(fiss_sum_wshed_filter$upstream_area_ha, na.rm = TRUE)
bin_1 <- 0
# bin_1 <- floor(min(fiss_sum_wshed_filter$upstream_area_ha, na.rm = TRUE)/5)*5
bin_n <- ceiling(max(fiss_sum_wshed_filter$upstream_area_ha, na.rm = TRUE)/5)*5
bins <- seq(bin_1,bin_n, by = 1000)

plot_wshed_hist <- ggplot(fiss_sum_wshed_filter, aes(x=upstream_area_ha
                                           # fill=alias_local_name
                                           # color = alias_local_name
)) +
  geom_histogram(breaks = bins,
                 position="identity", size = 0.75)+
  labs(x = "Upstream Watershed Area (ha)", y = "Count Fish (#)") +
  facet_wrap(~species_code, ncol = 2)+
  # scale_color_grey() +
  # scale_fill_grey() +
  theme_bw(base_size = 11)+
  scale_x_continuous(breaks = bins[seq(1, length(bins), by = 2)])+
  # scale_color_manual(values=c("grey90", "grey60", "grey30", "grey0"))+
  # theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_histogram(aes(y=..density..), breaks = bins, alpha=0.5,
                 position="identity", size = 0.75)
plot_wshed_hist


# fiss_sum_wshed_prep1 <- fiss_sum %>%
#   mutate(Watershed = case_when(
#     upstream_area_ha < 5 ~ '0 - 5m',
#     upstream_area_ha >= 5 &  upstream_area_ha < 10 ~ '05 - 10m',
#     upstream_area_ha >= 10 &  upstream_area_ha < 15 ~ '10 - 15m',
#     upstream_area_ha >= 15 &  upstream_area_ha < 20 ~ '15 - 20m',
#     upstream_area_ha >= 20  ~ '20m+')) %>%
#   mutate(watershed_id = case_when(
#     upstream_area_ha < 5 ~ 5,
#     upstream_area_ha >= 5 &  upstream_area_ha < 10 ~ 10,
#     upstream_area_ha >= 10 &  upstream_area_ha < 15 ~ 15,
#     upstream_area_ha >= 15 &  upstream_area_ha < 20 ~ 20,
#     upstream_area_ha >= 20  ~ 99))
#
# fiss_sum_width_prep2 <- fiss_sum_width_prep1 %>%
#   group_by(species_code) %>%
#   summarise(total_spp = n())
#
# fiss_sum_width_prep3 <- fiss_sum_width_prep1 %>%
#   group_by(species_code, Width, width_id)  %>%
#   summarise(Count = n())
#
# fiss_sum_width <- left_join(
#   fiss_sum_width_prep3,
#   fiss_sum_width_prep2,
#   by = 'species_code'
# ) %>%
#   mutate(Percent = round(Count/total_spp * 100, 0))
#
# ##save this for the report
# ##burn it all to a file we can use later
# fiss_sum_width %>% readr::write_csv(file = paste0(getwd(), '/data/extracted_inputs/fiss_sum_width.csv'))
#
# plot_width <- fiss_sum_width %>%
#   filter(!is.na(width_id)) %>%
#   ggplot(aes(x = Width, y = Percent)) +
#   geom_bar(stat = "identity")+
#   facet_wrap(~species_code, ncol = 2)+
#   theme_bw(base_size = 11)+
#   labs(x = "Channel Width", y = "Occurrences (%)")
# plot_width
