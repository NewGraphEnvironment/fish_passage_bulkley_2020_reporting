##we need to make a cross-ref table for the feature names
source('R/functions.R')
source('R/packages.R')
source('R/tables-phase2.R')


conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "postgis",
  host = "localhost",
  port = "5432",
  user = "postgres",
  password = "postgres"
)

# # ##list tables in a schema
dbGetQuery(conn,
           "SELECT table_name
           FROM information_schema.tables
           WHERE table_schema='whse_basemapping'")

##list column names in a table
dbGetQuery(conn,
           "SELECT column_name,data_type
           FROM information_schema.columns
           WHERE table_schema = 'whse_fish'
           and table_name='fiss_obstacles_pnt_sp'")

##list distinct column names in a table
xref_fiss_obstacles <- dbGetQuery(conn, "SELECT distinct a.obstacle_name FROM whse_fish.fiss_obstacles_pnt_sp a") %>%
  as_tibble() %>% mutate(spreadsheet = NA_character_)

# xref_fwa_obstructions <- dbGetQuery(conn, "SELECT distinct a.obstruction_type FROM whse_basemapping.fwa_obstructions_sp a") %>%
#   as_tibble()

xref_spreadsheat <- unique(hab_features$feature_type)

##I have cut the resulting tibble into the 0310-tables.R file




