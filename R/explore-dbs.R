
dbGetQuery(conn2,
           "SELECT *
           FROM information_schema.tables")

dbGetQuery(conn,
           "SELECT table_name
           FROM information_schema.tables
           WHERE table_schema='geobc'")

# # # # # ##list column names in a table
DBI::dbGetQuery(conn,
           "SELECT column_name,data_type
           FROM information_schema.columns
           WHERE table_name='fwa_lakes_poly'")


test <- st_read(conn2,
                            query = "SELECT * FROM bcfishpass.model_spawning_rearing_habitat a
                                LIMIT 1000"
)

morr_streams <- st_read(conn2,
                query = "SELECT * FROM bcfishpass.streams a
                               WHERE a.watershed_group_code  = 'MORR'"
)

morr_streams_bcfp <- st_read(conn_bcfp,
                        query = "SELECT * FROM bcfishpass.streams a
                               WHERE a.watershed_group_code  = 'MORR'"
)

morr_cw <- st_read(conn2,
                query = "SELECT * FROM bcfishpass.channel_width_modelled a
                               WHERE a.watershed_group_code  = 'MORR'"
)

morr_cw_bcfp <- st_read(conn_bcfp,
                   query = "SELECT * FROM bcfishpass.channel_width_modelled a
                               WHERE a.watershed_group_code  = 'MORR'"
)

test2 <- test %>%
  filter(!is.na(channel_width))

test %>% st_zm() %>% distinct(upstream_area_ha)
ggplot2::ggplot() +
  ggplot2::geom_sf(data = test[990,], lwd = 0.15, fill = 'red', alpha = 1)


##trying to see why there are so few chan widths

morr_streams_unique <- morr_streams %>%
  sf::st_drop_geometry() %>%
  dplyr::distinct(wscode_ltree, localcode_ltree, linear_feature_id, .keep_all = T)

morr_streams_cw_na <- morr_streams %>%
  filter(is.na(channel_width) &
           !edge_type %in% c(1000,1100,2000,2300) &
           stream_order > 1)

morr_streams_modelled <- morr_streams %>%
  filter(!is.na(accessibility_model_steelhead))

morr_streams_modelled_bcfp <- morr_streams_bcfp %>%
  filter(!is.na(accessibility_model_steelhead))

morr_cw_unique <- morr_cw %>%
  dplyr::distinct(wscode_ltree, localcode_ltree, .keep_all = T)

##which stream s have a gradiet barrier of 15 downstream but not 20
morr_15 <- morr_streams %>%
  filter(!is.na(dnstr_barriers_gradient_15) & is.na(dnstr_barriers_gradient_20))


morr_15_bcfp <- morr_streams_bcfp %>%
  filter(!is.na(dnstr_barriers_gradient_15) & is.na(dnstr_barriers_gradient_20))

test <- setdiff(
  select(morr_15, segmented_stream_id, dnstr_barriers_gradient_15,dnstr_barriers_gradient_20),
  select(morr_15_bcfp, segmented_stream_id, dnstr_barriers_gradient_15, dnstr_barriers_gradient_20)
)

morr_15_diff <- morr_15_bcfp %>%
  filter(segmented_stream_id %in% (test %>% pull(segmented_stream_id))) %>%
  select(segmented_stream_id, dnstr_barriers_gradient_15, dnstr_barriers_gradient_20)

##find your gradient barriers in the MORR
grad_barr <- st_read(conn2,
                        query = "SELECT * FROM bcfishpass.gradient_barriers a
                               WHERE a.watershed_group_code  = 'MORR'"
)

grad_barr_bcfp <- st_read(conn_bcfp,
                     query = "SELECT * FROM bcfishpass.gradient_barriers a
                               WHERE a.watershed_group_code  = 'MORR'"
)

##find out which ecoregions fall within the BuLK and MORR watershed groups then find out which watershed groups touch those ecoregions
##conn is our main connection to wsl now
eco <- st_read(conn,
               query = "
                                  SELECT ws.watershed_group_code, eco.ecoregion_code, eco.geom
                                  FROM whse_basemapping.fwa_watershed_groups_poly ws
                                  INNER JOIN
                                  whse_terrestrial_ecology.erc_ecoregions_sp eco
                                  ON ST_Intersects(ws.geom,eco.geom)
                                  WHERE ws.watershed_group_code IN
                                  ('BULK',
                                  'MORR')")


##see which watershed groups fall into the skeena
wsg_skeena <- st_read(conn,
        query = "SELECT ws.watershed_group_code, b.basin
        FROM whse_basemapping.fwa_watershed_groups_poly ws
        INNER JOIN
        geobc.bcbasins b
        ON ST_Intersects(ws.geom,b.geom)
        WHERE b.basin IN ('SKEENA')")

##make a list of the skeena watershed groups

## BULK, MORR, LSKE, KISP, KLUM, LKEL, ZYMO, BABL, BABR, MSKE, SUST, USKE
