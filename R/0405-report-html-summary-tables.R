##summary table
print_tab_summary_png <- function(site){
  make_tab_summary(df = pscis_all %>% filter(pscis_crossing_id == site)) %>% ##%>% sf::st_drop_geometry()
    kable(booktabs = T) %>%    #
    kableExtra::add_footnote(label = paste0('Comments: ', pscis_all %>% filter(pscis_crossing_id == site) %>%
                                              distinct(pscis_crossing_id, .keep_all = T) %>% ##might be my_crossing_refe
                                              pull(assessment_comment)), notation = 'none') %>% #this grabs the comments out
    # kableExtra::add_footnote(label = paste0('Photos: From top left clockwise: Road/Site Card, Barrel, Outlet, Downstream, Upstream, Inlet.',
    #                                         paste0('![](data/photos/', site, '/crossing_all.JPG)')), notation = 'none', escape = F) %>%
    kableExtra::kable_styling(c("condensed"), full_width = T, font_size = 11) %>%
    kableExtra::save_kable(paste0("fig/sum/", site, ".png"))
  # kableExtra::scroll_box(width = "100%", height = "500px") ##not scrolling to simplify our pagedown output
}

# print_tab_summary_phase1_png <- function(site){
#   make_tab_summary(df = pscis_phase1_reassessments %>% filter(pscis_crossing_id == site)) %>%
#     kable(booktabs = T) %>%    #
#     kableExtra::add_footnote(label = paste0('Comments: ', pscis_phase1_reassessments %>% filter(pscis_crossing_id == site) %>% ##might be my_crossing_refe
#                                               pull(assessment_comment)), notation = 'none') %>% #this grabs the comments out
#     # kableExtra::add_footnote(label = paste0('Photos: From top left clockwise: Road/Site Card, Barrel, Outlet, Downstream, Upstream, Inlet.',
#     #                                         paste0('![](data/photos/', site, '/crossing_all.JPG)')), notation = 'none', escape = F) %>%
#     kableExtra::kable_styling(c("condensed"), full_width = T, font_size = 11) %>%
#     kableExtra::save_kable(paste0("fig/sum/", site, ".png"))
#   # kableExtra::scroll_box(width = "100%", height = "500px") ##not scrolling to simplify our pagedown output
# }
#
#
#
# ##summary table
# print_tab_summary_phase2_png <- function(site){
#   make_tab_summary(df = pscis_phase2 %>% filter(pscis_crossing_id == site)) %>%
#     kable(booktabs = T) %>%    #
#     kableExtra::add_footnote(label = paste0('Comments: ', pscis_phase2 %>% filter(pscis_crossing_id == site) %>%
#                                               pull(assessment_comment)), notation = 'none') %>% #this grabs the comments out
#     # kableExtra::add_footnote(label = paste0('Photos: From top left clockwise: Road/Site Card, Barrel, Outlet, Downstream, Upstream, Inlet.',
#     #                                         paste0('![](data/photos/', site, '/crossing_all.JPG)')), notation = 'none', escape = F) %>%
#     kableExtra::kable_styling(c("condensed"), full_width = T, font_size = 11) %>%
#     kableExtra::save_kable(paste0("fig/sum/", site, ".png"))
#   # kableExtra::scroll_box(width = "100%", height = "500px") ##not scrolling to simplify our pagedown output
# }


# print_tab_summary_phase1_png(site = '4600005')
sites <- pscis_all %>% pull(pscis_crossing_id)

sites %>%
  map(print_tab_summary_png)


# sites_phase1 <- pscis_phase1_reassessments %>% pull(amalgamated_crossing_id)
#
# sites_phase1 %>%
#   map(print_tab_summary_phase1_png)
#
# sites_phase2 <- pscis_phase2 %>% pull(pscis_crossing_id)
#
# sites_phase2 %>%
#   map(print_tab_summary_phase2_png)
