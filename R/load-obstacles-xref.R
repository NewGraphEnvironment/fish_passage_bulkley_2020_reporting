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


xref_obstacle_names <- tibble::tribble(
                                                    ~fiss_obstacle_name, ~spreadsheet_feature_type,
                                                          "Beaver Dams",                        NA,
                                                     "Velocity barrier",                        NA,
                                                                "Wedge",                        NA,
                                                               "Bridge",                        NA,
                                                            "Hydro Dam",                        NA,
                                         "Water Management Storage Dam",                        NA,
                                                        "Not Specified",                        NA,
                                                                 "Bars",                        NA,
                                                              "LWD Jam",                 "LWD Jam",
                                                                "OTHER",                        NA,
                                              "Irrigation District Dam",                        NA,
                                                 "Dam - Unknown Origin",                        NA,
                                                              "Cascade",                        NA,
                                                                 "Pump",                        NA,
                                                              "Log jam",                        NA,
                                                     "Cascade or Chute",                        NA,
                                                    "Persistent Debris",                        NA,
                                                            "Hydro dam",                        NA,
                                                                "Rocks",                        NA,
                         "Persistent debris; present for several years",                        NA,
                                                                 "Weir",                        NA,
                                                                "Falls",                        NA,
                                                                 "Logs",                        NA,
                                                              "Log Jam",                        NA,
                                                              "Culvert",                        NA,
                                                                 "Rock",                        NA,
                                                               "Canyon",                        NA,
                                                           "Beaver Dam",                        NA,
                                                "Regional District Dam",                        NA,
                                                          "Underground",                        NA,
                                                         "Woody Debris",                        NA,
                                                        "Cascade/Chute",        "cascade or Chute",
                                                          "Private Dam",                        NA,
                                                             "Gradient",                        NA,
                                             "Fisheries Management Dam",                        NA,
                                                           "BEAVER DAM",                        NA,
                                          "Landslide or bank sloughing",                        NA,
                                                     "Velocity Barrier",                        NA,
                                                                  "Dam",                        NA
                         )



