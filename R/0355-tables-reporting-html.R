##summary table
# print_tab_summary_png <- function(site){
#   make_tab_summary(df = pscis_all %>% filter(pscis_crossing_id == site)) %>% ##%>% sf::st_drop_geometry()
#     kable(booktabs = T) %>%    #
#     kableExtra::add_footnote(label = paste0('Comments: ', pscis_all %>% filter(pscis_crossing_id == site) %>%
#                                               distinct(pscis_crossing_id, .keep_all = T) %>% ##might be my_crossing_refe
#                                               pull(assessment_comment)), notation = 'none') %>% #this grabs the comments out
#     # kableExtra::add_footnote(label = paste0('Photos: From top left clockwise: Road/Site Card, Barrel, Outlet, Downstream, Upstream, Inlet.',
#     #                                         paste0('![](data/photos/', site, '/crossing_all.JPG)')), notation = 'none', escape = F) %>%
#     kableExtra::kable_styling(c("condensed"), full_width = T, font_size = 11) %>%
#     kableExtra::save_kable(paste0("fig/sum/", site, ".png"))
#   # kableExtra::scroll_box(width = "100%", height = "500px") ##not scrolling to simplify our pagedown output
# }

print_tab_summary_html <- function(site){
  make_tab_summary(df = pscis_all %>% filter(pscis_crossing_id == site)) %>% ##%>% sf::st_drop_geometry()
    kable(booktabs = T) %>%    #
    kableExtra::add_footnote(label = paste0('Comments: ', pscis_all %>% filter(pscis_crossing_id == site) %>%
                                              distinct(pscis_crossing_id, .keep_all = T) %>% ##might be my_crossing_refe
                                              pull(assessment_comment)), notation = 'none') %>% #this grabs the comments out
    # kableExtra::add_footnote(label = paste0('Photos: From top left clockwise: Road/Site Card, Barrel, Outlet, Downstream, Upstream, Inlet.',
    #                                         paste0('![](data/photos/', site, '/crossing_all.JPG)')),
    #                          notation = 'none',
    #                          escape = T) %>%
    kableExtra::kable_styling(c("condensed"), full_width = T, font_size = 18, html_font = 'helvetica') %>%
    # cat(., file = paste0("docs/sum/", site, ".html")) %>%
    readr::write_file(., file = paste0("docs/sum/", site, ".html"))
  # kableExtra::scroll_box(width = "100%", height = "500px") ##not scrolling to simplify our pagedown output
}



# print_tab_summary_phase1_png(site = '4600005')
sites <- pscis_all %>%
  # slice(1:2) %>%
  pull(pscis_crossing_id)

# sites %>%
#   map(print_tab_summary_png)


##we are going to use this one now
sites %>%
  map(print_tab_summary_html)

# sites_phase1 <- pscis_phase1_reassessments %>% pull(amalgamated_crossing_id)
#
# sites_phase1 %>%
#   map(print_tab_summary_phase1_png)
#
# sites_phase2 <- pscis_phase2 %>% pull(pscis_crossing_id)
#
# sites_phase2 %>%
#   map(print_tab_summary_phase2_png)

#print off the tables for the modelling for phase2 sites.
##we need to have the correct modelled crossing ids for all of our phase 1s to pull this off for those. I think we do though actually...do it later!
print_tab_summary_bcfp_html <- function(sites){
  make_tab_summary_bcfp(site = sites) %>%
  kable(caption = paste0('Summary of fish habitat modelling for PSCIS crossing ', sites, '.'), booktabs = T) %>%    #
    kableExtra::add_footnote('Model data is preliminary and subject to adjustments including incorperation of area based estimates.', notation = 'symbol') %>%
    kableExtra::kable_styling(c("condensed"), full_width = T, font_size = 18) %>%
    readr::write_file(., file = paste0("docs/sum/bcfp/", sites, ".html"))
}

print_tab_summary_bcfp_html_planning <- function(site){
  make_tab_summary_bcfp_planning(siteid = site) %>% ##%>% sf::st_drop_geometry()
    kable(booktabs = T) %>%    #
    kableExtra::kable_styling(c("condensed"), full_width = T, font_size = 18, html_font = 'helvetica') %>%
    # cat(., file = paste0("docs/sum/", site, ".html")) %>%
    readr::write_file(., file = paste0("docs/sum/bcfp/", site, ".html"))
  # kableExtra::scroll_box(width = "100%", height = "500px") ##not scrolling to simplify our pagedown output
}

##for now we just print out the phase 2 tables
pscis_all %>%
  filter(source %ilike% 'phase2') %>%
  pull(pscis_crossing_id) %>%
  map(print_tab_summary_bcfp_html)

##print the modelled crossing tables
tab_plan_raw %>%
  filter(!is.na(my_text)) %>%
  pull(aggregated_crossings_id) %>%
  map(print_tab_summary_html_planning)

##should do phase 1 as well

