##summary table
print_tab_summary_phase1_png <- function(site){
  make_tab_summary(df = pscis %>% filter(my_crossing_reference == site)) %>%
    kable(booktabs = T) %>%    #
    kableExtra::add_footnote(label = paste0('Comments: ', pscis %>% filter(my_crossing_reference == site) %>%
                                              pull(assessment_comment)), notation = 'none') %>% #this grabs the comments out
    # kableExtra::add_footnote(label = paste0('Photos: From top left clockwise: Road/Site Card, Barrel, Outlet, Downstream, Upstream, Inlet.',
    #                                         paste0('![](data/photos/', site, '/crossing_all.JPG)')), notation = 'none', escape = F) %>%
    kableExtra::kable_styling(c("condensed"), full_width = T, font_size = 11) %>%
    kableExtra::save_kable(paste0("fig/sum/", site, ".png"))
  # kableExtra::scroll_box(width = "100%", height = "500px") ##not scrolling to simplify our pagedown output
}



##summary table
print_tab_summary_phase2_png <- function(site){
  make_tab_summary(df = pscis2 %>% filter(pscis_crossing_id == site)) %>%
    kable(booktabs = T) %>%    #
    kableExtra::add_footnote(label = paste0('Comments: ', pscis2 %>% filter(pscis_crossing_id == site) %>%
                                              pull(assessment_comment)), notation = 'none') %>% #this grabs the comments out
    # kableExtra::add_footnote(label = paste0('Photos: From top left clockwise: Road/Site Card, Barrel, Outlet, Downstream, Upstream, Inlet.',
    #                                         paste0('![](data/photos/', site, '/crossing_all.JPG)')), notation = 'none', escape = F) %>%
    kableExtra::kable_styling(c("condensed"), full_width = T, font_size = 11) %>%
    kableExtra::save_kable(paste0("fig/sum/", site, ".png"))
  # kableExtra::scroll_box(width = "100%", height = "500px") ##not scrolling to simplify our pagedown output
}


print_tab_summary_phase1_png(site = '4600005')

sites_phase1 <- pscis %>% pull(my_crossing_reference)

sites_phase1 %>%
  map(print_tab_summary_phase1_png)

sites_phase2 <- pscis2 %>% pull(pscis_crossing_id)

sites_phase2 %>%
  map(print_tab_summary_phase2_png)
