source('R/private_info.R')
source('R/packages.R')
pacman::p_load(olsrr,
               caret,
               MLmetrics)
##svDialogs

# Load latest channel width measurement points (FISS and PSCIS)
# channel_width = fread(input = dlg_open(default = "./data/*",
#                                        title = "Please select the measured channel width file",
#                                        gui = .GUI)$res)


conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = dbname_wsl,
  host = host_wsl,
  port = port_wsl,
  user = user_wsl,
  password = password_wsl
)

dbGetQuery(conn,
           "SELECT table_name
           FROM information_schema.tables
           WHERE table_schema='bcfishpass'")

##count rows in a table
DBI::dbGetQuery(conn,
                "SELECT reltuples as approximate_row_count
                FROM pg_class WHERE relname = 'fwa_watersheds_poly';")


# DBI::dbGetQuery(conn,
#                         "SELECT ''''||watershed_group_code||''''
#     FROM whse_basemapping.fwa_watershed_groups_poly
#     ORDER BY watershed_group_code;")
#
# # # # # # ##list column names in a table
DBI::dbGetQuery(conn,
                "SELECT column_name,data_type
           FROM information_schema.columns
           WHERE table_name='crossings'")

##bring in the new width file
cw <- sf::st_read(conn,
                  query = "SELECT *
              FROM bcfishpass.channel_width_modelled")  # filter(!is.na(channel_width_fiss) & !is.na(channel_width_pscis)) %>%
  # filter(cw_diff < 30)
# filter(cw_diff != Inf)
# summarize(m = mean(cw_diff, na.rm = T))


cw_mod_porter <- sf::st_read(conn,
                  query = "SELECT *
              FROM bcfishpass.channel_width_modelled")

dbDisconnect(conn = conn)

##add some columns for flagging error sites
error_stream_sample_site_ids <- c(44813,44815,10997,8518,37509,37510,53526,15603,98,8644,117,8627,142,8486,8609,15609,10356)
error_stream_crossing_ids <- c(57592,123894,57408,124137)

cw <- cw %>%
  mutate(
    error_flag = case_when(
    (stream_sample_site_id %in% error_stream_sample_site_ids |
      stream_crossing_id %in% error_stream_crossing_ids) ~ T
  ),
  error_comment = case_when(
    error_flag == T ~ 'errors in measurement and/or linking to streams'
  ),
  error_reviewer = case_when(
    error_flag == T ~ 'CWF'
    )
  )

## burn a copy to file
cw %>%
  readr::write_csv(file = paste0(getwd(), '/data/width_modelling/channel_width_measured_analyze.csv'))


cw2 <- cw %>%
  mutate(
    cw_diff = round(abs(channel_width_fiss - channel_width_pscis)/((channel_width_fiss + channel_width_pscis)/2) *100),1,
    cw_ave = (channel_width_fiss + channel_width_pscis)/2,  ##should mutate all at once but not taking the time figure out the syntax now
    upstream_area_ha_log = log10(upstream_area_ha),
    map_upstream_log = log10(map_upstream),
    channel_width_measured_log = log10(channel_width_measured),
    cw_ave_log = log(cw_ave),
    stream_order_log = log10(stream_order),
    stream_magnitude_log = log10(stream_magnitude),
    gradient = case_when(gradient < 0.0001 ~ 0.0001, T ~ gradient),
    gradient_log = log10(abs(gradient))
  ) %>%  ##we want the average of our collaborating channel widths
  filter(
    channel_width_measured <=15 &  ##we are mostly interested in streams that would have culverts so lets try cw of 15m or less
      channel_width_measured >=1 &  ##seems like a lot of our error is from small channel widths
      upstream_area_ha <= 20000 &  ##lots of error from enormous watersheds.  20000ha is quite a bit bigger than Richfield our largest potential culvert site...
      upstream_area_ha > 0 &
      !is.na(map_upstream)
      # is.finite(map_upstream_log) &
      # is.finite(upstream_area_ha_log) &
      # is.finite(channel_width_measured_log)
    # cw_diff <=25  ##we can use only the channel widths that have agreeing values for fiss and pscis but then our dataset is tiny with mostly very small widths represented.
  ) %>%
  arrange(desc(upstream_area_ha))


##lets visualize our data and see what is going on
## watershed area looks like a poisson distribution....
ggplot(cw2, aes(x=gradient)) +
  geom_histogram(
    position="identity", size = 0.75)+
  labs(x = "Variable", y = "Sites (#)") +
  theme_bw(base_size = 11)+
  scale_x_continuous()+
  geom_histogram(aes(y=..density..),  alpha=0.5,
                 position="identity", size = 0.75)



##here is what happens using porters equation and the resulting errors
cw_porter <- cw2 %>%
  mutate(cw_mod = round(0.042 * (upstream_area_ha/100)^0.48 * (map_upstream/10)^0.74 ,2)) %>%  ##km2 and mm - this is Porter et al 2008 equations
  mutate(cw_qa_perc = abs(round((cw_mod - channel_width_measured)/channel_width_measured * 100, 0))) %>% ##see how close the porter model gets.... Not bad actually.
  arrange(desc(cw_qa_perc))
  # summarize(rmse = mean(cw_qa_perc), median_error = median(cw_qa_perc))

ggplot(cw_porter, aes(x=upstream_area_ha, y = cw_qa_perc)) +
  geom_point()+
  labs(x = "Variable", y = "Error (%)") +
  theme_bw(base_size = 11)

cw_m1 <- lm(channel_width_measured_log  ~ upstream_area_ha_log +
              map_upstream_log + stream_magnitude_log + gradient_log,
            data = cw2)

cw_subset <-  olsrr::ols_step_best_subset(cw_m1)  ##we don't gain much from the stream_order or gradient it seems


summary(cw_m1)




##lets see how well it works on all the data
cw_predict <- cw2 %>%
  mutate(cw_predicted_log = predict(cw_m1),
         cw_predicted = 10^cw_predicted_log,
         cw_qa_perc = round(abs((channel_width_measured - cw_predicted)/channel_width_measured * 100),0)) %>%
  arrange(desc(cw_qa_perc))
  # summarize(rmse = mean(cw_qa_perc), median_error = median(cw_qa_perc))  ##interestingly I get almost the same error as Porter....

ggplot(cw_predict, aes(x=upstream_area_ha, y = cw_qa_perc)) +
  geom_point()+
  labs(x = "Variable", y = "Error (%)") +
  theme_bw(base_size = 11)


ggplot(data=cw2, aes(cw_m1$residuals)) +
  geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Model Residuals")

ggplot(data = cw2, aes(x = channel_width_measured_log, y = map_upstream_log)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data")


#split data into model training and testing sets
set.seed(123)
train <-  cw2$channel_width_measured %>%
  createDataPartition(p=0.8,list = FALSE)
mod_trainer = cw2[train,]
mod_tester = cw2[-train,]


#use model results to predict channel width for test data, generate some performance metrics
cw_modelled <- lm(channel_width_measured_log  ~ upstream_area_ha_log +
              map_upstream_log, data = mod_trainer)
summary(cw_modelled)



tester$cw_predictions <- cw_modelled %>%
  predict(mod_tester) ##add the predictions



predictions = cw_modelled %>% predict(mod_tester)
caret::RMSE(predictions,tester$channel_width_measured)
caret::R2(predictions,tester$channel_width_measured)



################predicted values after bayesian analysis##################################
## this section was just used to explore how to translate equation from our channel-width-21 repo set up by Joe into postgresql
## in R we have eWidth[i] = exp(b0 + bArea * log(area[i])  + bPrecipitation * log(precipitation[i]))


streams2 <- DBI::dbGetQuery(
  conn,
  "With streams AS
  (
  SELECT
  s.wscode_ltree,
  s.localcode_ltree,
  s.watershed_group_code,
  max(s.stream_order) as stream_order,
  max(s.stream_magnitude) as stream_magnitude,
  max(COALESCE(s.upstream_area_ha, 0)) + 1 as upstream_area_ha,
  max(COALESCE(s.upstream_lake_ha, 0)) + 1 as upstream_lake_ha,
  max(COALESCE(s.upstream_wetland_ha, 0)) + 1 as upstream_wetland_ha
FROM whse_basemapping.fwa_stream_networks_sp s
LEFT OUTER JOIN whse_basemapping.fwa_waterbodies wb
ON s.waterbody_key = wb.waterbody_key
WHERE s.watershed_group_code IN ('BULK','MORR','ELKR','HORS')
-- we only want widths of streams/rivers
AND (wb.waterbody_type = 'R' OR (wb.waterbody_type IS NULL AND s.edge_type IN (1000,1100,2000,2300)))
AND s.localcode_ltree IS NOT NULL
GROUP BY wscode_ltree, localcode_ltree, watershed_group_code)

  SELECT
  s.wscode_ltree,
  s.localcode_ltree,
  s.watershed_group_code,
  s.upstream_area_ha,
  p.map_upstream,
  -- The formula for predicting channel width based on Thorley and Irvine 2021
  -- eWidth[i] = exp(b0 + bArea * log(area[i]) + bArea * log(area[i]) + bPrecipitation * log(precipitation[i])
  -- the OLD equation we used that is referenced in porter translated to postgresql was
  --OLD -- 0.042 * power((s.upstream_area_ha/100),0.48) * power((p.map_upstream/10),0.74) -- OLD do not use
  round(
      (EXP(-2.2383120 + 0.3121556 * ln(s.upstream_area_ha/100) + 0.6546995 * ln(p.map_upstream/10))
    )::numeric, 2
  )
  as channel_width_modelled
FROM streams s
INNER JOIN bcfishpass.mean_annual_precip p
ON s.wscode_ltree = p.wscode_ltree
AND s.localcode_ltree = p.localcode_ltree
WHERE s.upstream_area_ha IS NOT NULL;"
)

streams_cw <- streams %>%
  mutate(
    channel_width_modelled =
      exp(-2.2383120 + 0.3121556  * log(upstream_area_ha/100) + 0.6546995 * log(map_upstream/10))
  )

cw <- sf::st_read(conn,
                  query = "SELECT *
              FROM bcfishpass.channel_width_modelled")






######################################################################################################
#########################Extras - Al#################################################################
##this is how we got the list of watershed groups to use to fit in memory for one precip run.
# wshds_find <- cw %>%
#   group_by(watershed_group_code) %>%
#   summarise(n = n()) %>%
#   filter(n > 9) %>%
#   pull(watershed_group_code)

##we can make a list of these watershed groups so that we can paste into our scipt with the right syntax.
# cat(paste(shQuote(wshds_find, type="sh"), collapse=", "))


# ##here is a list of the stream_sample_site_ids that have been flagged by CWF to exclude.  WE can look at them if we want
# fiss_exclude <- c(44813,44815,10997,8518,37509,37510,53526,15603,98,8644,117,8627,142,8486,8609,15609,10356)
# ##here is a list of pscis stream_crossing_ids to exclude flagged by CWF
# pscis_exclude <- c(57592,123894,57408,124137)

##if we wanted to split up the cw dat to have seperate columns for each of the ids then remove the {} we would do the folowing.
# this is not necessary b/c now we have pulled out the issue sites beforehand.
# cw_split <- splitstackshape::cSplit(cw, 'stream_sample_site_ids', ',', drop = F) %>%
#   splitstackshape::cSplit(., 'stream_crossing_ids', ',', drop = F) %>%
#   mutate(across(starts_with('stream_sample_site_ids_'), ~ stringr::str_replace_all(., '[{}]', ''))) %>%
#   mutate(across(starts_with('stream_crossing_ids_'), ~ stringr::str_replace_all(., '[{}]', '')))





