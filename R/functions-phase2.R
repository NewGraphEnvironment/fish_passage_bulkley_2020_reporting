

my_overview_info <- function(dat = pscis_phase2, site = my_site){
  dat %>% filter(pscis_crossing_id == site)
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

my_pscis_info <- function(dat = pscis_phase2, site = my_site){
  dat %>%
    filter(pscis_crossing_id == site) %>%
    mutate(stream_name = stringr::str_replace_all(stream_name, 'Tributary', 'tributary'))
}


my_bcfishpass <- function(dat = bcfishpass_phase2, site = my_site){
  dat %>%
    mutate(across(where(is.numeric), round, 0)) %>%
    filter(stream_crossing_id == site) %>%
    distinct(stream_crossing_id, .keep_all = T)
}

# my_bcfishpass <- function(dat = bcfishpass_phase2, site = my_site){
#   dat %>%
#     mutate(across(where(is.numeric), round, 0)) %>%
#     filter(pscis_crossing_id == site) %>%
#     distinct(pscis_crossing_id, .keep_all = T)
# }

my_watershed_area <- function(dat = wsheds, site = my_site){
  dat %>%
    filter(pscis_crossing_id == my_site) %>%
    pull(area_km)
}

##we needed to back off this b/c maps not ready
my_mapsheet <- function(){
  paste0('https://hillcrestgeo.ca/outgoing/fishpassage/projects/bulkley/FishPassage_', my_bcfishpass() %>%
           pull(dbm_mof_50k_grid), '.pdf')
}

# my_mapsheet <- function(){
#   paste0('https://hillcrestgeo.ca/outgoing/fishpassage/projects/elk/confirmations')
# }

my_priority_info <- function(dat = habitat_confirmations_priorities, sit = my_site, loc = 'us'){
  dat %>%
    filter(site == sit & location == loc)
}




my_cost_estimate <- function(dat = tab_cost_est_phase2, site = my_site){
  dat %>%
    filter(pscis_crossing_id == site) %>%
    distinct(pscis_crossing_id, .keep_all = T)
}

##this will pull out fish species names from our fish species codes
my_fish_sp <- function(sit = my_site, col_to_pull = quo(observedspp_upstr)){
  str_to_pull <- stringr::str_replace_all((my_bcfishpass(site = sit) %>% pull(!!col_to_pull)), c("\\{" = "","\\}" = "")) %>%
    strsplit(., ",") %>% unlist()
  fishbc::freshwaterfish %>% filter(Code %in% str_to_pull) %>% pull(CommonName) %>% stringr::str_to_lower() %>%
    knitr::combine_words()
}


