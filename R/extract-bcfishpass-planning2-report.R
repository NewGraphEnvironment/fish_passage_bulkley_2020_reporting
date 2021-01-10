##there is another script (extract-bcfishpass-planning.R) to build the file that we reviewed in Q


##to look at pscis crossings only we use the query builder in Q to say "stream_crossing_id" IS NOT NULL
##when it is time to look at the modelled crossings only we use "stream_crossing_id" IS NULL
source('R/functions.R')
source('R/packages.R')

conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "postgis",
  host = "localhost",
  port = "5432",
  user = "postgres",
  password = "postgres"
)

##declare the schema and table names
schema_name = "working"
table_name = "elk_flathead_planning_20210109"

##lets get the data that has been input in qgis and join it back into the dataframe so it can be reburned
query <- paste0("SELECT a.* FROM ", schema_name, ".", table_name, " a;")

###declare the columns you want to keep
col_to_keep <- c('aggregated_crossings_id', 'stream_crossing_id', 'priority' = 'my_priority', 'study_area', 'wct_network_km',
                 'stream_order', 'downstream_channel_width', 'wct_lakereservoir_ha', 'wct_wetland_ha',
                 'observedspp_upstr', 'habitat_value_code','image_view_url', 'comments' = 'my_text')

##pull out the data, filter and clean
dat_after_review<- st_read(conn, query = query) %>%
  filter(!is.na(my_priority)) %>%
  select(all_of(col_to_keep)) %>%
  st_transform(crs = 4326)

##we should fix this up but we will do it this way for now.

##burn to kml
st_write(dat_after_review, dsn = paste0(getwd(), '/data/planning_kmls/flathead_planning.kml'), driver = 'kml', append=FALSE)

dbDisconnect(conn = conn)


##we need to zip our kmls or they would be pulled correctly from github
files_to_zip <- paste0("data/planning_kmls/", list.files(path = "data/planning_kmls/", pattern = "\\.kml$"))  ##this used to includes the planning file which we don't want to do so watch out
zip::zipr("data/planning_kmls/elk_planning_kmls.zip", files = files_to_zip)  ##it does not work to zip to kmz!!
