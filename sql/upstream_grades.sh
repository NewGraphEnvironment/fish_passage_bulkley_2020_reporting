psql2csv "SELECT
  a.barriers_anthropogenic_id,
  stream_crossing_id,
  modelled_crossing_id,
  dam_id,
  ROUND((SUM(ST_Length(b.geom)) FILTER (WHERE b.gradient >= 0 AND b.gradient < .035) / 1000)::numeric, 2) as upstr_slopeclass_1_km,
  ROUND((SUM(ST_Length(b.geom)) FILTER (WHERE b.gradient >= .035 AND b.gradient < .055) / 1000)::numeric, 2) as upstr_slopeclass_2_km,
  ROUND((SUM(ST_Length(b.geom)) FILTER (WHERE b.gradient >= .055 AND b.gradient < .135) / 1000)::numeric, 2) as upstr_slopeclass_3_km,
  ROUND((SUM(ST_Length(b.geom)) FILTER (WHERE b.gradient >= .135 AND b.gradient < .205) / 1000)::numeric, 2) as upstr_slopeclass_4_km,
  ROUND((SUM(ST_Length(b.geom)) FILTER (WHERE b.gradient >= .205 AND b.gradient < .305) / 1000)::numeric, 2) as upstr_slopeclass_5_km,
  ROUND((SUM(ST_Length(b.geom)) FILTER (WHERE b.gradient >= .305) / 1000)::numeric, 2) as upstr_slopeclass_6_km
FROM bcfishpass.barriers_anthropogenic a
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
    a.watershed_group_code = 'ELKR' AND
    b.dnstr_barriers_gradient_15 IS NULL AND
    b.dnstr_barriers_gradient_20 IS NULL AND
    b.dnstr_barriers_gradient_30 IS NULL AND
    b.dnstr_barriers_falls IS NULL AND
    b.dnstr_barriers_majordams IS NULL AND
    b.dnstr_barriers_subsurfaceflow IS NULL AND
    b.dnstr_barriers_ditchflow IS NULL
GROUP BY a.barriers_anthropogenic_id,
stream_crossing_id,
modelled_crossing_id,
dam_id;" > upstream_gradients_15pct_elkr.csv


psql2csv "SELECT
  a.barriers_anthropogenic_id,
  stream_crossing_id,
  modelled_crossing_id,
  dam_id,
  ROUND((SUM(ST_Length(b.geom)) FILTER (WHERE b.gradient >= 0 AND b.gradient < .035) / 1000)::numeric, 2) as upstr_slopeclass_1_km,
  ROUND((SUM(ST_Length(b.geom)) FILTER (WHERE b.gradient >= .035 AND b.gradient < .055) / 1000)::numeric, 2) as upstr_slopeclass_2_km,
  ROUND((SUM(ST_Length(b.geom)) FILTER (WHERE b.gradient >= .055 AND b.gradient < .135) / 1000)::numeric, 2) as upstr_slopeclass_3_km,
  ROUND((SUM(ST_Length(b.geom)) FILTER (WHERE b.gradient >= .135 AND b.gradient < .205) / 1000)::numeric, 2) as upstr_slopeclass_4_km,
  ROUND((SUM(ST_Length(b.geom)) FILTER (WHERE b.gradient >= .205 AND b.gradient < .305) / 1000)::numeric, 2) as upstr_slopeclass_5_km,
  ROUND((SUM(ST_Length(b.geom)) FILTER (WHERE b.gradient >= .305) / 1000)::numeric, 2) as upstr_slopeclass_6_km
FROM bcfishpass.barriers_anthropogenic a
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
    a.watershed_group_code = 'ELKR' AND
    b.dnstr_barriers_gradient_20 IS NULL AND
    b.dnstr_barriers_gradient_30 IS NULL AND
    b.dnstr_barriers_falls IS NULL AND
    b.dnstr_barriers_majordams IS NULL AND
    b.dnstr_barriers_subsurfaceflow IS NULL AND
    b.dnstr_barriers_ditchflow IS NULL
GROUP BY a.barriers_anthropogenic_id,
stream_crossing_id,
modelled_crossing_id,
dam_id;" > upstream_gradients_20pct_elkr.csv

psql2csv "SELECT
  a.barriers_anthropogenic_id,
  stream_crossing_id,
  modelled_crossing_id,
  dam_id,
  ROUND((SUM(ST_Length(b.geom)) FILTER (WHERE b.gradient >= 0 AND b.gradient < .035) / 1000)::numeric, 2) as upstr_slopeclass_1_km,
  ROUND((SUM(ST_Length(b.geom)) FILTER (WHERE b.gradient >= .035 AND b.gradient < .055) / 1000)::numeric, 2) as upstr_slopeclass_2_km,
  ROUND((SUM(ST_Length(b.geom)) FILTER (WHERE b.gradient >= .055 AND b.gradient < .135) / 1000)::numeric, 2) as upstr_slopeclass_3_km,
  ROUND((SUM(ST_Length(b.geom)) FILTER (WHERE b.gradient >= .135 AND b.gradient < .205) / 1000)::numeric, 2) as upstr_slopeclass_4_km,
  ROUND((SUM(ST_Length(b.geom)) FILTER (WHERE b.gradient >= .205 AND b.gradient < .305) / 1000)::numeric, 2) as upstr_slopeclass_5_km,
  ROUND((SUM(ST_Length(b.geom)) FILTER (WHERE b.gradient >= .305) / 1000)::numeric, 2) as upstr_slopeclass_6_km
FROM bcfishpass.barriers_anthropogenic a
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
    a.watershed_group_code = 'ELKR' AND
    b.dnstr_barriers_gradient_30 IS NULL AND
    b.dnstr_barriers_falls IS NULL AND
    b.dnstr_barriers_majordams IS NULL AND
    b.dnstr_barriers_subsurfaceflow IS NULL AND
    b.dnstr_barriers_ditchflow IS NULL
GROUP BY a.barriers_anthropogenic_id,
stream_crossing_id,
modelled_crossing_id,
dam_id;" > upstream_gradients_30pct_elkr.csv