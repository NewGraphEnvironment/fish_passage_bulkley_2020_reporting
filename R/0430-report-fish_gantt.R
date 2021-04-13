##build a gantt chart for fish timing

gantt_raw <- read_csv("data/inputs_raw/fish_species_life_history_gantt.csv")


##start with just the morice to keep it simple
# ungroup()
##start with just the morice to keep it simple
gantt <- gantt_raw %>%
  select(Species,
         life_stage,
         morice_start2,
         morice_end2) %>%
  filter(
    life_stage != 'Rearing' &
      life_stage != 'Upstream fry migration' &
      !is.na(life_stage),
    !is.na(morice_start2)
  )%>%
  mutate(
    morice_start2 = lubridate::as_date(morice_start2),
    morice_end2 = lubridate::as_date(morice_end2),
    life_stage = factor(life_stage, levels =
                          c('Migration', 'Overwintering', 'Spawning', 'Incubation', 'Emergence', 'Outmigration')),
    life_stage = forcats::fct_rev(life_stage) ##last line was upside down!
  ) %>%
  filter(life_stage != 'Overwintering')


##make a plot
ggplot(gantt, aes(xmin = morice_start2,
                  xmax = morice_end2,
                  y = life_stage,
                  color = life_stage)) +
  geom_linerange(size = 2) +
  labs(x=NULL, y=NULL)+
  # theme_bw()+
  ggdark::dark_theme_bw(base_size = 11)+
  theme(legend.position = "none")+
  scale_x_date(date_labels = "%B")+
  facet_wrap(~Species, ncol = 1)

##another way to do it but is a pain to get multi lines!
# gantt2 <- gantt_raw %>%
#   select(Species,
#          life_stage,
#          morice_start2,
#          morice_end2) %>%
#   tidyr::pivot_longer(cols = c('morice_start2', 'morice_end2'),
#                       values_to = 'date') %>%
#   filter(!is.na(date),
#          life_stage != 'Rearing' &
#            life_stage != 'Upstream fry migration') %>%
#   mutate(
#     date = lubridate::as_date(date),
#     life_stage = factor(life_stage, levels =
#                           c('Migration', 'Spawning', 'Incubation', 'Emergence', 'Outmigration')),
#     life_stage = forcats::fct_rev(life_stage) ##last line was upside down!
#     # Species2 = paste0(Species, 1:nrow(.)),
#     # order = paste0(Species, '_', life_stage, '_', )
#   )
#
#
# ggplot(gantt2, aes(date, life_stage, color = life_stage)) +
#   geom_line(size = 2) +
#   labs(x=NULL, y=NULL)+
#   # theme_bw()+
#   ggdark::dark_theme_bw(base_size = 11)+
#   theme(legend.position = "none")+
#   scale_x_date(date_labels = "%B")+
#   facet_wrap(~Species, ncol = 1)
