##make a file with all the modelled crossings in the watershed along with the amount of habitat upstream
##we will need to add the road information later


test <- dbGetQuery(conn,
                   "SELECT
  a.*,
  ROUND((SUM(ST_Length(b.geom)) FILTER (WHERE b.gradient >= 0 AND b.gradient < .035) / 1000)::numeric, 2) as upstr_slopeclass_1_km,
  ROUND((SUM(ST_Length(b.geom)) FILTER (WHERE b.gradient >= .035 AND b.gradient < .055) / 1000)::numeric, 2) as upstr_slopeclass_2_km,
  ROUND((SUM(ST_Length(b.geom)) FILTER (WHERE b.gradient >= .055 AND b.gradient < .135) / 1000)::numeric, 2) as upstr_slopeclass_3_km,
  ROUND((SUM(ST_Length(b.geom)) FILTER (WHERE b.gradient >= .135 AND b.gradient < .205) / 1000)::numeric, 2) as upstr_slopeclass_4_km,
  ROUND((SUM(ST_Length(b.geom)) FILTER (WHERE b.gradient >= .205 AND b.gradient < .305) / 1000)::numeric, 2) as upstr_slopeclass_5_km,
  ROUND((SUM(ST_Length(b.geom)) FILTER (WHERE b.gradient >= .305) / 1000)::numeric, 2) as upstr_slopeclass_6_km,
  b.dnstr_barriers_gradient_15,
  b.dnstr_barriers_gradient_20,
  b.dnstr_barriers_gradient_30
FROM bcfishpass.modelled_stream_crossings a
LEFT OUTER JOIN bcfishpass.streams b
ON FWA_Upstream(
    a.blue_line_key,
    a.downstream_route_measure,
    a.wscode_ltree,
    a.localcode_ltree,
    b.blue_line_key,
    b.downstream_route_measure,
    b.wscode_ltree,
    b.localcode_ltree,
    True,
    .02
   )
WHERE
    a.watershed_group_code = 'ELKR'
GROUP BY a.modelled_crossing_id,
b.dnstr_barriers_gradient_15,
b.dnstr_barriers_gradient_20,
b.dnstr_barriers_gradient_30;")


##this should be called in the function
test2 <- test %>% distinct(linear_feature_id, downstream_route_measure, .keep_all = T)
