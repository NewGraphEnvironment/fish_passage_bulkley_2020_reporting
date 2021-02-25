##grab the bcfishpass spawning and rearing table and put in the database so it can be used to populate the methods and tie to the references table

urlfile="https://github.com/smnorris/bcfishpass/raw/main/02_model/data/model_spawning_rearing_habitat.csv"

bcfishpass_spawn_rear_model <- read_csv(url(urlfile))


##burn it to the database so we have a stable version for reporting
conn <- rws_connect("data/bcfishpass.sqlite")
rws_list_tables(conn)
# rws_drop_table("bcfishpass_spawn_rear_model", conn = conn)
rws_write(bcfishpass_spawn_rear_model, exists = F, delete = TRUE,
          conn = conn, x_name = "bcfishpass_spawn_rear_model")
rws_list_tables(conn)
rws_disconnect(conn)
