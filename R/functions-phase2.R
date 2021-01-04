

my_overview_info <- function(site = my_site){
  pscis2 %>% filter(pscis_crossing_id == site)
}

##transpose the data so you can get ranges and filter
my_habitat_info <- function(dat = hab_site, sit = my_site){
  left_join(
  hab_site %>%
    filter(site == sit & location == 'us') %>%
    select(site, everything()) %>%
    t() %>%
    as.data.frame() %>%  # as_tibble() %>%
    tibble::rownames_to_column() %>%
    rename(us = V1),

  hab_site %>%
    filter(site == sit & location == 'ds') %>%
    select(site, everything()) %>%
    t() %>%
    as.data.frame() %>%  # as_tibble() %>%
    tibble::rownames_to_column() %>%
    rename(ds = V1),
  by = 'rowname'
) %>%
  mutate(rowname = stringr::str_replace_all(rowname, '_', ' '))
}

##transpose the data so you can get ranges and filter
my_habitat_info2 <- function(dat = hab_site, sit = my_site,
                             loc = 'us'){
  dat %>%
    filter(site == sit & location == loc) %>%
    select(site, everything()) %>%
    t() %>%
    as.data.frame() %>%  # as_tibble() %>%
    tibble::rownames_to_column() %>%
    rename(v = V1) %>%
    mutate(rowname = stringr::str_replace_all(rowname, '_', ' '))
    # filter(column == row) %>%
    # pull(v)
}

##transpose the data so you can get ranges and filter
my_habitat_info3 <- function(dat = hab_site, sit = my_site,
                             loc = 'us', row = 'site'){
  dat %>%
    filter(site == sit & location == loc) %>%
    select(site, everything()) %>%
    t() %>%
    as.data.frame() %>%  # as_tibble() %>%
    tibble::rownames_to_column() %>%
    # rename(v = V1) %>%
    mutate(rowname = stringr::str_replace_all(rowname, '_', ' ')) %>%
    filter(rowname == row) %>%
    pull(V1)
}

my_pscis_info <- function(dat = pscis2, site = my_site){
  dat %>%
    filter(pscis_crossing_id == site) %>%
    mutate(stream_name = stringr::str_replace_all(stream_name, 'Tributary', 'tributary'))
}


my_bcfishpass <- function(dat = bcfishpass_phase2, site = my_site){
  dat %>%
    mutate(across(where(is.numeric), round, 0)) %>%
    filter(pscis_crossing_id == site) %>%
    distinct(pscis_crossing_id, .keep_all = T)
}

my_watershed_area <- function(dat = wsheds, site = my_site){
  dat %>%
    filter(pscis_crossing_id == my_site) %>%
    pull(area_km)
}

##we needed to back off this b/c maps not ready
# my_mapsheet <- function(){
#   paste0('https://hillcrestgeo.ca/outgoing/fishpassage/projects/elk/FishPassage_', my_bcfishpass() %>%
#            pull(map_tile_display_name), '.pdf')
# }

my_mapsheet <- function(){
  paste0('https://hillcrestgeo.ca/outgoing/fishpassage/projects/elk/confirmations')
}

my_priority_info <- function(dat = habitat_confirmations_priorities, sit = my_site, loc = 'us'){
  dat %>%
    filter(site == sit & location == loc)
}




my_cost_estimate <- function(dat = tab_cost_est_phase2, site = my_site){
  dat %>%
    filter(pscis_crossing_id == site) %>%
    distinct(pscis_crossing_id, .keep_all = T)
}

