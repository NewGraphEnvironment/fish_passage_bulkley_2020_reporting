source('R/packages.R')

##08NK002 is elk river at Fernie  - 08NK016 is near sparwood

library(tidyhydat)
library(fasstr)
library(hydatr)

# download_hydat()

##C:\Users\allan\AppData\Local\tidyhydat\tidyhydat

hydatr::hydat_load(source = "C://Users//allan//AppData//Local//tidyhydat//tidyhydat") # loads the database (you'll need to call this one each time you load the package)


hydrograph <- fasstr::plot_daily_stats(station_number = "08NK002",
                                       start_year = 1970,
                                       end_year = 9999,
                                       log_discharge = TRUE,
                                       ignore_missing = TRUE)



hydrograph_print <- hydrograph[["Daily_Statistics"]]

ggsave(plot = hydrograph_print, file="./fig/hydrology1.png",
       h=3.4, w=5.11, units="in", dpi=300)

tidyhat_info <- search_stn_number("08NK002")
hydatr_info <- as.data.frame(hydat_station_info(tidyhat_info$STATION_NUMBER))
hydatr_info <- mutate(hydatr_info, title = paste0(stringr::str_to_title(STATION_NAME),
                                                  " (Station #",STATION_NUMBER," - Lat " ,round(LATITUDE,6),
                                                  " Lon ",round(LONGITUDE,6), "). Available daily discharge data from 1970",
                                                  # FIRST_YEAR, ##removed the default here
                                                  " to ",LAST_YEAR, "."))
hydrograph1_caption <- hydatr_info$title

##fasstr::plot_data_screening2 is a custom version of plot_data_screening
hydrograph_stats_print <- fasstr::plot_data_screening3(station_number = "08NK002", start_year = 1970)[["Data_Screening"]]

ggsave(plot = hydrograph_stats_print, file="./fig/hydrology_stats1.png",
       h=3.4, w=5.11, units="in", dpi=300)

##another way to make the graph
flow_raw <- tidyhydat::hy_daily_flows("08NK002", start_date = '1970-01-01')

flow <- flow_raw %>%
  mutate(day_of_year = yday(Date)) %>%
  group_by(day_of_year) %>%
  summarise(daily_ave = mean(Value, na.rm=TRUE),
            daily_sd = sd(Value, na.rm = TRUE),
            max = max(Value, na.rm = TRUE),
            min = min(Value, na.rm = TRUE),
            q025 = quantile(Value, probs = 0.025),
            q975 = quantile(Value, probs = 0.975)) %>%
  mutate(Date = as.Date(day_of_year, origin = "2015-12-31"))

plot <- ggplot()+
  geom_ribbon(data = flow, aes(x = Date, ymax = max,
                               ymin = min),
              alpha = 0.3, linetype = 1)+
  # geom_ribbon(data = flow_UDR, aes(x = Date, ymax = daily_ave + 2 * daily_sd,
  #                                  ymin = daily_ave - 2 * daily_sd),
  #             alpha = 0.4, linetype = 1)+
  # geom_ribbon(data = flow_UDR, aes(x = Date, ymax = q975,
  #                                  ymin = q025),
  #             alpha = 0.3, linetype = 1)+

  scale_x_date(date_labels = "%b", date_breaks = "2 month") +
  labs(x = NULL, y = expression(paste("Mean Daily Discharge (", m^3, "/s)", sep="")))+
  theme_bw() +
  # ylim(0,600) +
  # theme(axis.text.y=element_blank())+
  # scale_y_continuous() +
  geom_line(data = flow, aes(x = Date, y = daily_ave),
            linetype = 1, size = 0.7) +
  scale_colour_manual(values = c("grey10", "red")) +
  coord_cartesian(ylim = c(0, 600))
plot

ggsave(plot = plot, file="./fig/hydrology2.png",
       h=3.4, w=5.11, units="in", dpi=300)
