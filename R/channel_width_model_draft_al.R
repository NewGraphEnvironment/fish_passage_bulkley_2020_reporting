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
# DBI::dbGetQuery(conn,
#                 "SELECT column_name,data_type
#            FROM information_schema.columns
#            WHERE table_name='fwa_watersheds_poly'")



map <- sf::st_read(conn,
                   query = "SELECT *
              FROM bcfishpass.mean_annual_precip;")

cw <- sf::st_read(conn,
                  query = "SELECT *
              FROM bcfishpass.channel_width_measured"
) %>%
  mutate(cw_diff = round(abs(channel_width_fiss - channel_width_pscis)/((channel_width_fiss + channel_width_pscis)/2) *100),1)
  # filter(!is.na(channel_width_fiss) & !is.na(channel_width_pscis)) %>%
  # filter(cw_diff < 30)
# filter(cw_diff != Inf)
# summarize(m = mean(cw_diff, na.rm = T))





dbDisconnect(conn = conn)


##add the map_upstream values and filter down
cw_map_upstream <- left_join(
  cw,
  select(map, wscode_ltree, localcode_ltree, map_upstream),
  by = c('wscode_ltree', 'localcode_ltree'))
  # select(-`1`, -stream_sample_site_ids, -stream_crossing_ids) %>%
  # mutate(across(everything(), ~stringr::str_replace_all(., '[{}]', '')))


##lets burn a copy of the cw to send to joe incase we mess something up
cw_map_upstream %>% readr::write_csv(file = paste0(getwd(), '/data/channel_width_measured_20210331.csv'))


cw2 <- cw_map_upstream %>%
  mutate(
    cw_ave = (channel_width_fiss + channel_width_pscis)/2,
    upstream_area_ha_log = log10(upstream_area_ha),
    map_upstream_log = log10(map_upstream),
    channel_width_measured_log = log10(channel_width_measured),
    # channel_width_measured_sqrt = sqrt(channel_width_measured),
    cw_ave_log = log(cw_ave),
    # upstream_area_ha_sqrt = sqrt(upstream_area_ha),
    # map_upstream_sqrt = sqrt(map_upstream),
    # cw_ave_sqrt = sqrt(cw_ave),
  ) %>%  ##we want the average of our collaborating channel widths

  filter(
    channel_width_measured <=15 &  ##we are mostly interested in streams that would have culverts so lets try cw of 15m or less
      upstream_area_ha <= 100000 &  ##same thing for enormous watersheds.  20000ha seems reasonable as only one of our qualifying sites is in a whsd bigger than this
      upstream_area_ha > 0 &
      !is.na(map_upstream) &
      is.finite(map_upstream_log) &
      is.finite(upstream_area_ha_log) &
      is.finite(channel_width_measured_log)

    # cw_diff <=25 & ##we only use the channel widths that have agreeing values for fiss and pscis.
    # cw_ave <=1.5  ##seems like a lot of our error is from small channel widths
  ) %>%
  arrange(desc(upstream_area_ha))


##lets visualize our data and see what is going on
## watershed area looks like a poisson distribution....
ggplot(cw2, aes(x=channel_width_measured_log)) +
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
  mutate(cw_qa = abs(round((cw_mod - channel_width_measured)/channel_width_measured * 100, 0))) %>% ##see how close the porter model gets.... Not bad actually.
  summarize(rmse = mean(cw_qa), median_error = median(cw_qa))


cw_m1 <- lm(channel_width_measured_log  ~ upstream_area_ha_log +
              map_upstream_log,
            data = cw2)

##so cw_log_predicted = -7.56499 + 0.36919 * upstream_area_ha_log + 0.93301 * map_upstream_log
# summary(cw_m1)
summary(cw_m1)

# pchisq(200.3, 272, lower.tail = FALSE)



##lets see how well it works on all the data
cw_predict <- cw2

cw_predict$cw_predicted_log <- predict(cw_m1)


cw_predict <- cw_predict %>%
  mutate(cw_predicted = 10^cw_predicted_log,
         error = round(abs((channel_width_measured - cw_predicted)/channel_width_measured * 100),0))  %>%
  # filter(channel_width_measured < 6) %>%
  # arrange(desc(error))
  summarise(error_mean = mean(error), error_median = median(error))


# Checking that the residuals are normally distributed
resid <- resid(cw_m2) # Extracting the residuals
fitted <- fitted(cw_m2)
predictions <- predict(cw_m2)
coef <- coef(cw_m2)
deviance <- deviance(cw_m2)






ggplot(data=cw2, aes(cw_m2$residuals)) +
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
# cw_subset <-  olsrr::ols_step_best_subset(cw_m1)

#split data into model training and testing sets
set.seed(123)
training.samples_watershed = cw2$channel_width_measured %>%
  createDataPartition(p=0.8,list = FALSE)
train.data_watershed = cw2[training.samples_watershed,]
test.data_watershed = cw2[-training.samples_watershed,]


#use model results to predict channel width for test data, generate some performance metrics

test.data_watershed$cw_predictions <- cw_m1 %>%
  predict(test.data_watershed) ##add the predictions

test.data_watershed <- test.data_watershed %>%
  mutate(cw_qa = abs(cw_predictions - channel_width_fiss)/channel_width_fiss *100) %>%
  summarise(qa_median = median(cw_qa))


predictions_watershed = cw_m1 %>% predict(test.data_watershed)
caret::RMSE(predictions_watershed,test.data_watershed$channel_width_measured)
caret::R2(predictions_watershed,test.data_watershed$channel_width_measured)







#https://ourcodingclub.github.io/tutorials/modelling/
cw2 %>%
  filter(channel_width_measured_log > 0) %>%
  ggplot(aes(x = channel_width_measured_log, y = upstream_area_ha_log))+
  geom_point(aes(colour = map_log)) +
  labs(x = "channel_width_measured_log", y = "upstream_area_ha_log") +
  stat_smooth(method = 'lm')
  # scale_colour_manual(values = c("#FFC125", "#36648B")) +
  # scale_fill_manual(values = c("#FFC125", "#36648B")) +
  # theme.clean()






######################################################################################################
#########################Extras - Al#################################################################
##this is how we got the list of watershed groups to use.
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

######################################################################################################
#########################Extras - NICK#################################################################

#adjust parameters after the '~' based on results of 'channel_width_best_subset'
model_final = lm(channel_width_measured ~ stream_order * log_stream_magnitude * log_map * log_upstream_area_ha, data = train.data_watershed)
summary(model_final)

#use model results to predict channel width for test data, generate some performance metrics
predictions_watershed = model_final %>% predict(test.data_watershed)
RMSE(predictions_watershed,test.data_watershed$channel_width_measured)
R2(predictions_watershed,test.data_watershed$channel_width_measured)

#further model performance metrics

predictions_watershed = as.data.frame(predictions_watershed)
predictions_watershed$r_id = row.names(predictions_watershed)

channel_width_watershed$r_id = row.names(channel_width_watershed)

prediction_eval_watershed = inner_join(channel_width_watershed,predictions_watershed,by="r_id")

prediction_eval_watershed$predictions_watershed = abs(prediction_eval_watershed$predictions_watershed)

prediction_eval_watershed = subset(prediction_eval_watershed, select = c(r_id,channel_width_measured,predictions_watershed))
View(prediction_eval_watershed)


MAPE(prediction_eval_watershed$predictions_watershed,prediction_eval_watershed$channel_width_measured)

prediction_eval_watershed$difference = abs((prediction_eval_watershed$channel_width_measured - prediction_eval_watershed$predictions_watershed))
prediction_eval_watershed$percent_diff = ifelse(prediction_eval_watershed$channel_width_measured > prediction_eval_watershed$predictions_watershed,1-(prediction_eval_watershed$predictions_watershed/prediction_eval_watershed$channel_width_measured),1-(prediction_eval_watershed$channel_width_measured/prediction_eval_watershed$predictions_watershed))
View(prediction_eval_watershed)
mean(prediction_eval_watershed$percent_diff)

#assess prediction binning success
prediction_eval_watershed$measured_bin = NA
prediction_eval_watershed$predicted_bin = NA

#CH and ST spawn; can test other species/habitat types by changing cut thresholds
cuts = c(-Inf,3.7,50,Inf)
bins = c(0,1,0)
prediction_eval_watershed$measured_bin = bins[findInterval(prediction_eval_watershed$channel_width,cuts)]
prediction_eval_watershed$predicted_bin = bins[findInterval(prediction_eval_watershed$predictions_watershed,cuts)]

View(prediction_eval_watershed)

prediction_eval_watershed$prediction_correct = ifelse(prediction_eval_watershed$measured_bin == prediction_eval_watershed$predicted_bin,1,0)
sum(prediction_eval_watershed$prediction_correct)/length(prediction_eval_watershed$prediction_correct)




