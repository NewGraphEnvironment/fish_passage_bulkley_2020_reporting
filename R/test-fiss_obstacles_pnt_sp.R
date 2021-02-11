##this is to test if the fiss_obstacles_pnt_sp table FISH_OBSTACLE_POINT_ID is stable since the
##last time downloaded

conn <- dbConnect(dbDriver("PostgreSQL"),
                  dbname = "postgis_2018",
                  host = "localhost",
                  port = "5432",
                  user = "postgres",
                  password = "postgres")

dbGetQuery(conn,
           "SELECT column_name,data_type
           FROM information_schema.columns
           WHERE table_name='fiss_obstacles_pnt_sp'")


d <- sf::st_read(conn,
            query = "SELECT a.fish_obstacle_point_id, a.utm_zone, a.utm_easting, a.utm_northing, a.last_updated
            FROM whse_fish.fiss_obstacles_pnt_sp_old a")

d2 <- sf::st_read(conn,
                 query = "SELECT a.fish_obstacle_point_id, a.utm_zone, a.utm_easting, a.utm_northing, a.last_updated
            FROM whse_fish.fiss_obstacles_pnt_sp a")


d3 <- left_join(
  d,
  d2,
  by = "fish_obstacle_point_id"
)

dbDisconnect(conn = conn)
##looks like there hasn't been an update for jan 1 to feb 11.  Need to check back.
