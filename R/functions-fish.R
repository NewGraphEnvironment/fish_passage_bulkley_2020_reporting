##functions for dealing with fish data

##lets us hab_fish_dens to make tables for the report
# tab_fish_site <- hab_fish_dens %>%
#   filter(
#     site  == '50155'
#     # &
#     # species_code == 'WCT'
#   ) %>%
#   mutate(effort = round(ef_seconds/area, 1)) %>%
#   select(location,site_number, ef_length_m:area, effort) %>%
#   distinct(.keep_all = T) %>%
#   select(Site = site_number,
#          Location = location,
#          `Width (m)` = ef_width_m,
#          `Length (m)` = ef_length_m,
#          `Area (m2)` = area,
#          `Effort (s)` = ef_seconds,
#          `Effort (s/m2)` = effort)

tab_fish_site <- function(dat = hab_fish_dens, sit = my_site){
  dat %>%
  filter(
    site == sit
    # &
    # species_code == 'WCT'
  ) %>%
  mutate(effort = round(ef_seconds/area, 1)) %>%
  select(location,site_number, ef_length_m:area, effort) %>%
  distinct(.keep_all = T) %>%
  select(Site = site_number,
         Location = location,
         `Width (m)` = ef_width_m,
         `Length (m)` = ef_length_m,
         `Area (m2)` = area,
         `Effort (s)` = ef_seconds,
         `Effort (s/m2)` = effort)
}


tab_fish_dens <- function(dat = hab_fish_dens, sit = my_site){ #species = 'RB'
  dat %>%
    # mutate(life_stage = factor(life_stage, levels = c('fry', 'parr', 'juvenile', 'adult'))) %>%
    filter(
      site  == sit
      # &
      #   species_code == species
    ) %>%
    select(location,site_number, species_code, life_stage, density_100m2) %>%
    distinct(.keep_all = T)  %>%
    pivot_wider(names_from = life_stage,
                values_from = density_100m2) %>%
    select(Site = site_number,
           Location = location,
           Species = species_code,
           everything()) %>% ##removed fry, parr
    purrr::set_names(nm = stringr::str_to_title(names(.))) %>%
    arrange(Species) %>%
    mutate_all(~replace_na(.,"0"))
}

# tab_fish_dens <- function(dat = hab_fish_dens, sit = my_site){ #species = 'RB'
#   dat %>%
#     # mutate(life_stage = factor(life_stage, levels = c('fry', 'parr', 'juvenile', 'adult'))) %>%
#     filter(
#       site  == sit
#       # &
#       #   species_code == species
#     ) %>%
#     select(location,site_number, life_stage, density_100m2) %>%
#     distinct(.keep_all = T)  %>%
#     pivot_wider(names_from = life_stage,
#                 values_from = density_100m2) %>%
#     select(Site = site_number,
#            Location = location,
#            everything()) %>% ##removed fry, parr
#     purrr::set_names(nm = stringr::str_to_title(names(.))) %>%
#     mutate_all(~replace_na(.,"0"))
# }


plot_fish_box <- function(dat = hab_fish_dens, sit = my_site){
  dat %>%
    filter(
      site  == sit
      # &
      #   species_code == species
    ) %>%
    ggplot(., aes(x = location, y =density_100m2)) +
    geom_boxplot()+
    facet_grid(species_code ~ life_stage, scales ="fixed",
               as.table = T)+
    theme_bw()+
    theme(legend.position = "none", axis.title.x=element_blank()) +
    geom_dotplot(binaxis='y', stackdir='center', dotsize=1)+
    ylab(expression(Density ~ (fish/100 ~  m^2)))
}

plot_fish_box_all <- function(dat = hab_fish_dens, sp = 'RB'){
  dat %>%
    filter(
      species_code  == sp
      # &
      #   species_code == species
    ) %>%
    ggplot(., aes(x = location, y =density_100m2)) +
    geom_boxplot()+
    facet_grid(site ~ life_stage, scales ="fixed",
               as.table = T)+
    theme_bw()+
    theme(legend.position = "none", axis.title.x=element_blank()) +
    geom_dotplot(binaxis='y', stackdir='center', dotsize=1)+
    ylab(expression(Density ~ (Fish/100 ~  m^2)))
}

tab_fish_mt <- function(sit = my_site){
  left_join(
  hab_fish_input %>%
    mutate(stage = factor(stage, levels = c('fry', 'parr', 'juvenile', 'adult'))),

  select(hab_fish_codes, common_name, species_code),

  by = 'common_name'
  )  %>%
    filter(site == sit & sampling_method == 'minnow trapping' & !is.na(stage)) %>%
    group_by(site, location, species_code, stage) %>%
    summarise(Number = sum(total_number)) %>%
    ungroup() %>%
    select(Location = location,
           Species = species_code,
           Stage = stage,
           Number) %>%
    pivot_wider(names_from = Stage,
                values_from = Number) %>%
    mutate(Location = case_when(Location == 'us' ~ 'Upstream',
                                T ~ 'Downstream')) %>%
    arrange(Species) %>%
    mutate_all(~replace_na(.,"0"))
}



