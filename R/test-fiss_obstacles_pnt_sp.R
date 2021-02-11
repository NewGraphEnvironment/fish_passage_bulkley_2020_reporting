##this is to test if the fiss_obstacles_pnt_sp table FISH_OBSTACLE_POINT_ID is stable since the
##last time downloaded

conn <- dbConnect(dbDriver("PostgreSQL"),
                  dbname = "postgis",
                  host = "localhost",
                  port = "5432",
                  user = "postgres",
                  password = "postgres")

dbGetQuery(conn,
           "SELECT column_name,data_type
           FROM information_schema.columns
           WHERE table_name='fiss_obstacles_pnt_sp'")


d <- sf::st_read(conn,
            query = "SELECT a.fish_obstacle_point_id, a.utm_zone, a.utm_easting, a.utm_northing,
            a.obstacle_name, a.gazetted_name, a.last_updated
            FROM whse_fish.fiss_obstacles_pnt_sp a")

##pull file with 'date_modified' as 2017-01-30
d3 <- st_read("D:/temp/Fish_Obstacles.shp")


d4 <- left_join(
  select(d3, FSHBSTCLPN, UTM_ZONE, TMSTNG, TMNRTHNG, BSTCLNM, GZTTDNM),
  d,
  by = c('FSHBSTCLPN' = "fish_obstacle_point_id")
) %>%
  select(FSHBSTCLPN, obstacle_name, BSTCLNM, gazetted_name, GZTTDNM, everything()) %>%
  slice(1:1000) %>%
  readr::write_csv('data/extracted_inputs/fiss_obstacle_qa.csv')


dbDisconnect(conn = conn)
##looks like there hasn't been an update for jan 1 to feb 11.  Need to check back.
