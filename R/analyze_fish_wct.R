source('R/functions.R')
source('R/packages.R')
source('R/private_info.R')

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
           WHERE table_schema='whse_fish'")
# # # # #
# # # # # ##list column names in a table
dbGetQuery(conn,
           "SELECT column_name,data_type
           FROM information_schema.columns
           WHERE table_schema = 'bcfishpass'
           and table_name='streams'")

##get some stats for WCT
query = "SELECT
  fish_observation_point_id,
  s.gradient,
  s.stream_order,
  s.upstream_area_ha,
  round((ST_Z((ST_Dump(ST_LocateAlong(s.geom, e.downstream_route_measure))).geom))::numeric) as elevation
FROM whse_fish.fiss_fish_obsrvtn_events_sp e
INNER JOIN bcfishpass.streams s
ON e.linear_feature_id = s.linear_feature_id
WHERE e.species_code = 'WCT'
AND e.watershed_group_code = 'ELKR';"

wct_elkr <- st_read(conn, query = query)

wct_elkr_grad <- wct_elkr %>%
  mutate(slopeclass = case_when(
    gradient < .03 ~ 'sc00_03',
    gradient >= .03 &  gradient < .05 ~ 'sc03_05',
    gradient >= .05 &  gradient < .12 ~ 'sc05_12',
    gradient >= .12 &  gradient < .22 ~ 'sc12_22',
    gradient >= .22 &  gradient < .30 ~ 'sc22_30',
    gradient >= .30 ~ 'sc30_plus')) %>%
  group_by(slopeclass)  %>%
  summarise(num_occ = n())

wct_elkr_order <- wct_elkr %>%
  group_by(stream_order)  %>%
  summarise(num_occ = n())
