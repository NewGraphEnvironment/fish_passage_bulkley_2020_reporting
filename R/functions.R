


##funciton ot find a string in your directory from https://stackoverflow.com/questions/45502010/is-there-an-r-version-of-rstudios-find-in-files

fif <- function(what, where=".", in_files="\\.[Rr]$", recursive = TRUE,
                ignore.case = TRUE) {

  fils <- list.files(path = where, pattern = in_files, recursive = recursive)

  found <- FALSE

  file_cmd <- Sys.which("file")

  for (fil in fils) {

    if (nchar(file_cmd) > 0) {
      ftype <- system2(file_cmd, fil, TRUE)
      if (!grepl("text", ftype)[1]) next
    }

    contents <- readLines(fil)

    res <- grepl(what, contents, ignore.case = ignore.case)
    res <- which(res)

    if (length(res) > 0) {

      found <-  TRUE

      cat(sprintf("%s\n", fil), sep="")
      cat(sprintf(" % 4s: %s\n", res, contents[res]), sep="")

    }

  }

  if (!found) message("(No results found)")

}




fit_to_page <- function(ft, pgwidth = 6.75){

  ft_out <- ft %>% flextable::autofit()

  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  return(ft_out)
}

fit_to_page_landscape <- function(ft, pgwidth = 12){

  ft_out <- ft %>% flextable::autofit()

  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  return(ft_out)
}


my_flextable <- function(df,  ...){ ##left_just_col = 2 was an option
  flextable::autofit(flextable::flextable(
    df,
    defaults = list(fontname = 'tahoma'))) %>%
    flextable::theme_booktabs(fontsize = 8) %>% ##changed from flextable::my_theme_booktabs(fontsize = 9) %>%
    fit_to_page()
}

##function to trim up sheet and get names (was previously source from altools package)
at_trim_xlsheet2 <- function(df, column_last = ncol(df)) {
  df %>%
    dplyr::select(1:column_last) %>% ##get rid of the extra columns.  should be more abstract
    janitor::row_to_names(which.max(complete.cases(.))) %>%
    janitor::clean_names() %>%
    janitor::remove_empty(., which = "rows")
}


##function to import pscis info
import_pscis <- function(workbook_name = 'pscis_phase1.xlsm'){ ##new template.  could change file back to .xls
  sig_fig0 <- c('length_or_width_meters')
  sig_fig1 <- c('culvert_slope_percent', 'stream_width_ratio')
  sig_fig2 <- c('outlet_drop_meters')
  readxl::read_excel(path = paste0(getwd(),"/data/", workbook_name),
                     sheet = 'PSCIS Assessment Worksheet') %>%
    # purrr::set_names(janitor::make_clean_names(names(.))) %>%
    at_trim_xlsheet2() %>% ##recently added function above and pulled the altools package as it was a week link
    rename(date = date_of_assessment_yyyy_mm_dd) %>%
    mutate(date = janitor::excel_numeric_to_date(as.numeric(date))) %>%
    filter(!is.na(date)) %>%
    readr::type_convert() %>%  ##guess the type!!
    mutate(source = workbook_name) %>%
    mutate(across(all_of(sig_fig0), round, 0)) %>%
    mutate(across(all_of(sig_fig1), round, 1)) %>%
    mutate(across(all_of(sig_fig2), round, 2))
}


###---------------summary for phase 1---------------------
####---------------make a table for the comments---------------
make_tab_summary_comments <- function(df){
  df %>%
    # sf::st_drop_geometry() %>%
    select(assessment_comment) %>%
    # slice(1) %>%
    set_names('Comment')
}

####---------------make the report table-----
##grab a df with the names of the left hand side of the table
make_tab_summary <- function(df){
  tab_results_left <- xref_names %>%
    filter(id_side == 1)
  ##get the data
  tab_pull_left <- df %>%
    select(pull(tab_results_left,spdsht)) %>%
    # slice(1) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column()

  left <- left_join(tab_pull_left, xref_names, by = c('rowname' = 'spdsht'))

  tab_results_right <- xref_names %>%
    filter(id_side == 2)

  ##get the data
  tab_pull_right<- df %>%
    select(pull(tab_results_right,spdsht)) %>%
    # slice(1) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column()

  right <- left_join(tab_pull_right, xref_names, by = c('rowname' = 'spdsht'))

  tab_joined <- left_join(
    select(left, report, V1, id_join),
    select(right, report, V1, id_join),
    by = 'id_join'
  ) %>%
    select(-id_join) %>%
    purrr::set_names(c('Location and Stream Data', '-', 'Crossing Characteristics', '--'))
  return(tab_joined)
}


##here we stack up and down then side to side for reporting - this works!
make_photo_comp_cv <- function(site_id){
  photos_images1 <- list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = T) %>%
    stringr::str_subset(., 'upstream|road|inlet') %>%
    as_tibble() %>%
    mutate(sort = case_when(
      value %ilike% 'road' ~ 1,
      value %ilike% 'inlet' ~ 2,
      value %ilike% 'upstream' ~ 3,
      value %ilike% 'barrel' ~ 4,
      value %ilike% 'outlet' ~ 5,
      value %ilike% 'downstream' ~ 6,
    )) %>%
    arrange(sort) %>%
    pull(value) %>%
    image_read()
  photos_images2 <- list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = T) %>%
    stringr::str_subset(., 'barrel|outlet|downstream') %>%
    as_tibble() %>%
    mutate(sort = case_when(
      value %ilike% 'road' ~ 1,
      value %ilike% 'inlet' ~ 2,
      value %ilike% 'upstream' ~ 3,
      value %ilike% 'barrel' ~ 4,
      value %ilike% 'outlet' ~ 5,
      value %ilike% 'downstream' ~ 6,
    )) %>%
    arrange(sort) %>%
    pull(value) %>%
    image_read()
  photos_stack1 <-image_append(image_scale(photos_images1, "x420"), stack = T) ##1/3 the width 373.33 and half the original height
  photos_stack2 <- image_append(image_scale(photos_images2, "x420"), stack = T)
  photos_stack <- c(photos_stack1, photos_stack2)
  photos_stacked <- image_append(image_scale(photos_stack), stack = F)
  image_write(photos_stacked, path = paste0(getwd(), '/data/photos/', site_id, '/crossing_all.JPG'), format = 'jpg')
}



####-------culvert details summary---------------
make_tab_cv <- function(dat = pscis){
tab_culvert_prep <- dat %>%
  select(pscis_crossing_id, continuous_embeddedment_yes_no,
         outlet_drop_meters, diameter_or_span_meters,
         stream_width_ratio, culvert_slope_percent,
         length_or_width_meters,
         final_score, barrier_result)

names_report <- left_join(
  as_tibble(names(tab_culvert_prep)),
  select(xref_names, spdsht, report),
  by = c('value' = 'spdsht')
) %>%
  pull(report)

tab_culvert <- tab_culvert_prep %>%
  purrr::set_names(nm = names_report)
}


####--------------phase1 summary tables--------------------------
print_tab_summary_all <- function(tab_sum, comments, photos){
  kable(tab_sum, booktabs = T) %>%
    kableExtra::kable_styling(c("condensed"), full_width = T, font_size = 11) %>%
    kableExtra::add_footnote(label = paste0('Comments: ', comments[[1]]), notation = 'none') %>% #this grabs the comments out
    kableExtra::add_footnote(label = paste0('Photos: PSCIS ID ', photos[[2]],
                                            '. From top left clockwise: Road/Site Card, Barrel, Outlet, Downstream, Upstream, Inlet.',
                                            photos[[1]]), notation = 'none')
    # kableExtra::add_footnote(label = '<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>', escape = F, notation = 'none')
}

####--------------phase1 summary tables pdf--------------------------
print_tab_summary_all_pdf <- function(tab_sum, comments, photos){
  kable(tab_sum, booktabs = T) %>%
    kableExtra::kable_styling(c("condensed"), full_width = T, font_size = 11) %>%
    kableExtra::add_footnote(label = paste0('Comments: ', comments[[1]]), notation = 'none') %>% #this grabs the comments out
    kableExtra::add_footnote(label = paste0('Photos: PSCIS ID ', photos[[2]],
                                            '. From top left clockwise: Road/Site Card, Barrel, Outlet, Downstream, Upstream, Inlet.',
                                            photos[[1]]), notation = 'none') %>%
  kableExtra::add_footnote(label = '<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>', escape = F, notation = 'none')
}

##summary table
print_tab_summary <- function(dat = pscis_phase2, site = my_site, site_photo_id = my_site, font = 11){
  make_tab_summary(df = dat %>% filter(pscis_crossing_id == site)) %>%
    kable(caption = paste0('Summary of fish passage assessment for PSCIS crossing ', site, '.'), booktabs = T) %>%    #
    kableExtra::add_footnote(label = paste0('Comments: ', dat %>% filter(pscis_crossing_id == site) %>%
                                              pull(assessment_comment)), notation = 'none') %>% #this grabs the comments out
    kableExtra::add_footnote(label = paste0('Photos: From top left clockwise: Road/Site Card, Barrel, Outlet, Downstream, Upstream, Inlet.',
                                            paste0('![](data/photos/', site_photo_id, '/crossing_all.JPG)')), notation = 'none') %>%
    kableExtra::kable_styling(c("condensed"), full_width = T, font_size = font)
  # kableExtra::scroll_box(width = "100%", height = "500px") ##not scrolling to simplify our pagedown output
}




####------my_kable-------------------------------
my_kable_scroll <- function(dat, caption_text = '', font = font_set){
  dat %>%
    kable(caption = caption_text, booktabs = T) %>%
    kableExtra::kable_styling(c("condensed", "responsive"),
                              full_width = T,
                              font_size = font) %>%
    kableExtra::scroll_box(width = "100%", height = "500px")
}

my_tab_overview <- function(dat, caption_text = '', font = font_set){
  dat %>%
    kable(caption = caption_text, booktabs = T) %>%
    kableExtra::kable_styling(c("condensed", "responsive"), full_width = T, font_size = font) %>%
    kableExtra::column_spec(column = c(9), width_min = '1.5in') %>%
    kableExtra::column_spec(column = c(5), width_min = '1.0in', width_max = '1.0in')
}

my_tab_overview_scroll <- function(dat, caption_text = '', font = font_set){
  dat %>%
    kable(caption = caption_text, booktabs = T) %>%
    kableExtra::kable_styling(c("condensed"),
                              full_width = T,
                              font_size = font) %>%
    kableExtra::column_spec(column = c(9), width_min = '1.5in') %>%
    kableExtra::column_spec(column = c(5), width_max = '1in') %>%
    kableExtra::scroll_box(width = "100%", height = "500px")
}


my_kable_scroll_no_height <- function(dat, caption_text = ''){
  dat %>%
    kable(caption = caption_text, booktabs = T) %>%
    kableExtra::kable_styling(c("condensed"), full_width = T, font_size = 11) %>%
    kableExtra::scroll_box(width = "100%")
}

# my_kable_scroll <- function(dat){
#   dat %>%
#     kable() %>%
#     kableExtra::kable_styling(c("condensed"), full_width = T, font_size = 11) %>%
#     kableExtra::scroll_box(width = "100%", height = "500px")
# }

my_kable <- function(dat, caption_text = '', font = font_set){
  dat %>%
    kable(caption = caption_text, booktabs = T) %>%
    kableExtra::kable_styling(c("condensed", "responsive"),
                              full_width = T,
                              font_size = font)
    # kableExtra::scroll_box(width = "100%", height = "500px")
}

# my_kable <- function(dat){
#   dat %>%
#     kable() %>%
#     kableExtra::kable_styling(c("condensed"), full_width = T, font_size = 11)
#   # kableExtra::scroll_box(width = "100%", height = "500px")
# }

get_img <- function(site = my_site, photo = my_photo){
  jpeg::readJPEG(paste0('data/photos/', site, '/', photo))
}

get_img_path_abs <- function(site = my_site, photo = my_photo){
  stub <- 'https://github.com/NewGraphEnvironment/fish_passage_elk_2020_reporting/blob/master/'
  paste0(stub, 'data/photos/', site, '/', photo)
}

get_img_path <- function(site = my_site, photo = my_photo){
  paste0('data/photos/', site, '/', photo)
}




print_tab_cost_mult <- function(dat = tab_cost_rd_mult_report, ...){
  tab_cost_rd_mult_report %>%
  my_kable()
}

##here is a shot at a function to pull a photo based on a string subset
pull_photo_by_str <- function(site_id = my_site, str_to_pull = 'barrel'){
  list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = T) %>%
    stringr::str_subset(., str_to_pull) %>%
    basename()
}

appendix_title <- function(site = my_site){
  paste0('# Appendix - ', site, ' - ', my_overview_info() %>% pull(stream_name), ' {-}')
}


##can't get this to work
# appendix_title <- function(site = my_site){
#   # site_id <- 'test' ##works when this is a string
#   knitr::asis_output(paste0('# Appendix - ', site, ' - ', my_overview_info() %>% pull(stream_name), ' {-#', site_id, '}'))
# }

##when we have 2 crosings
appendix_title2 <- function(site = my_site, site2 = my_site2){
  paste0('# Appendix - ', site, ' & ', site2, ' - ', my_overview_info() %>% pull(stream_name), ' {-}')
}

##when we have 3 crosings
appendix_title3 <- function(site = my_site, site2 = my_site2, site3 = my_site3){
  paste0('# Appendix - ', site, ' & ', site2, ' & ', site3, ' - ', my_overview_info() %>% pull(stream_name), ' {-}')
}


appendix_subtitle <- function(){
  paste0('**', my_overview_info() %>% pull(road_name), ' - ', my_overview_info() %>% pull(stream_name), '**')
}


##############this is for making kmls
make_kml_col <- function(df){
  df %>%
    mutate(`PSCIS ID` = as.integer(`PSCIS ID`),
           `Modelled ID` = as.integer(`Modelled ID`),
           color = case_when(Priority == 'high' ~ 'red',
                             Priority == 'no fix' ~ 'green',
                             Priority == 'moderate' ~ 'yellow',
                             T ~ 'grey'),
           # shape = case_when(Priority == 'high' ~ 'http://maps.google.com/mapfiles/kml/pushpin/red-pushpin.png',
           #                   Priority == 'no fix' ~ 'http://maps.google.com/mapfiles/kml/pushpin/grn-pushpin.png',
           #                   Priority == 'moderate' ~ 'http://maps.google.com/mapfiles/kml/pushpin/ylw-pushpin.png',
           #                   T ~ 'http://maps.google.com/mapfiles/kml/pushpin/wht-pushpin.png'),
           shape = case_when(Priority == 'high' ~ 'http://maps.google.com/mapfiles/kml/paddle/red-blank.png',
                             Priority == 'no fix' ~ 'http://maps.google.com/mapfiles/kml/paddle/grn-blank.png',
                             Priority == 'moderate' ~ 'http://maps.google.com/mapfiles/kml/paddle/ylw-blank.png',
                             T ~ 'http://maps.google.com/mapfiles/kml/paddle/wht-blank.png'),
           color = plotKML::col2kml(color),
           site_id = case_when(!is.na(`PSCIS ID`) ~ paste('PSCIS ', `PSCIS ID`),
                               is.na(`PSCIS ID`) ~ paste0('Modelled ', `Modelled ID`)),
           label = paste0(site_id, '-', Priority),
           `Image link` = case_when(!is.na(`Image link`) ~ cell_spec('crossing', "html", link = `Image link`),
                                    T ~ `Image link`)) %>%
    select(site_id, Priority, label, color, shape, everything())
  # mutate(across(where(is.numeric), round(.,2)))

}

## add a line to the function to make the comments column wide enough
make_html_tbl <- function(df) {
  # df2 <- df %>%
  #   dplyr::mutate(`Image link` = cell_spec('crossing', "html", link = `Image link`))
  df2 <- select(df, -shape, -color, -label) %>% janitor::remove_empty()
  df %>%
    mutate(html_tbl = knitr::kable(df2, 'html', escape = F) %>%
             kableExtra::row_spec(0:nrow(df2), extra_css = "border: 1px solid black;") %>% # All cells get a border
             kableExtra::row_spec(0, background = "yellow") %>%
             kableExtra::column_spec(column = ncol(df2) - 1, width_min = '0.5in') %>%
             kableExtra::column_spec(column = ncol(df2), width_min = '4in')
    )
}


openHTML <- function(x) browseURL(paste0('file://', file.path(getwd(), x)))


######modelling summary table
####---------------make the report table-----
##grab a df with the names of the left hand side of the table
make_tab_summary_bcfp <- function(dat = bcfishpass_all,
                                  xref_table = xref_bcfishpass_names,
                                  site = my_site){
  df <- dat %>%
    mutate(across(where(is.numeric), round, 1)) %>%
    filter(stream_crossing_id == site) %>%
    distinct(stream_crossing_id, .keep_all = T)
  tab_results_left <- xref_table %>%
    filter(id_side == 1) %>%
    arrange(id_join)
  ##get the data
  tab_pull_left <- df %>%
    select(pull(tab_results_left,bcfishpass)) %>%
    # slice(1) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column()

  left <- left_join(tab_pull_left, xref_table, by = c('rowname' = 'bcfishpass'))

  tab_results_right <- xref_table %>%
    filter(id_side == 2)

  ##get the data
  tab_pull_right<- df %>%
    select(pull(tab_results_right,bcfishpass)) %>%
    # slice(1) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column()

  right <- left_join(tab_pull_right, xref_table, by = c('rowname' = 'bcfishpass'))

  tab_joined <- left_join(
    select(left, report, V1, id_join),
    select(right, report, V1, id_join),
    by = 'id_join'
  ) %>%
    select(-id_join) %>%
    purrr::set_names(c('Habitat', 'Potential', 'remove', 'Remediation Gain')) %>%
    mutate(Potential = as.numeric(Potential),
           `Remediation Gain` = as.numeric(`Remediation Gain`)) %>%
    mutate(`Remediation Gain (%)` = round(`Remediation Gain`/Potential * 100,0),
           Habitat = stringr::str_replace_all(Habitat, 'Ha', '(ha)'),
           Habitat = stringr::str_replace_all(Habitat, 'Km', '(km)'),
           Habitat = stringr::str_replace_all(Habitat, 'Lakereservoir', 'Lake and Reservoir'),
           Habitat = stringr::str_replace_all(Habitat, 'Spawningrearing ', 'Spawning and Rearing ')) %>%
    select(-remove)
  return(tab_joined)
}

##this is in two places and should not be - see 0355-tables-reporting-html
print_tab_summary_bcfp <- function(site = my_site, font = 11, ...){
  make_tab_summary_bcfp(site = site) %>%
    kable(caption = paste0('Summary of fish habitat modelling for PSCIS crossing ', site, '.'), booktabs = T) %>%    #
    kableExtra::add_footnote('Model data is preliminary and subject to adjustments.', notation = 'symbol') %>%
    kableExtra::kable_styling(c("condensed"), full_width = T, font_size = font)
}

text_ref_tab_summary_bcfp <-  function(site = my_site){
  paste0('presents preliminary fish passage modelling data for crossing ', site,
         ' with spawning and rearing habitat estimated for chinook, coho, sockeye and steelhead. ',
         'Modelled estimates of the total length of salmon or steelhead habitat upstream of the crossing before potential barriers are ',
         my_bcfishpass(site = site, round_dig = 1) %>% pull(all_spawning_belowupstrbarriers_km), 'km of potential spawning habitat and ',
         my_bcfishpass(site = site, round_dig = 1) %>% pull(all_rearing_belowupstrbarriers_km), 'km of potential rearing habitat.')
}


#this is ugly but im out of time so going to copy above to make bcfp summaries for modelled crossings
##grab a df with the names of the left hand side of the table
make_tab_summary_bcfp_planning <- function(dat = bcfishpass_all,
                                  xref_table = xref_bcfishpass_names,
                                  siteid){
  df <- dat %>%
    mutate(across(where(is.numeric), round, 1)) %>%
    filter(aggregated_crossings_id == siteid)  ##this is all that changes.  need to script but it tidyeval and don't want to deal
    # distinct(stream_crossing_id, .keep_all = T)
  tab_results_left <- xref_table %>%
    filter(id_side == 1) %>%
    arrange(id_join)
  ##get the data
  tab_pull_left <- df %>%
    select(pull(tab_results_left,bcfishpass)) %>%
    # slice(1) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column()

  left <- left_join(tab_pull_left, xref_table, by = c('rowname' = 'bcfishpass'))

  tab_results_right <- xref_table %>%
    filter(id_side == 2)

  ##get the data
  tab_pull_right<- df %>%
    select(pull(tab_results_right,bcfishpass)) %>%
    # slice(1) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column()

  right <- left_join(tab_pull_right, xref_table, by = c('rowname' = 'bcfishpass'))

  tab_joined <- left_join(
    select(left, report, V1, id_join),
    select(right, report, V1, id_join),
    by = 'id_join'
  ) %>%
    select(-id_join) %>%
    purrr::set_names(c('Habitat', 'Potential', 'remove', 'Remediation Gain')) %>%
    mutate(Potential = as.numeric(Potential),
           `Remediation Gain` = as.numeric(`Remediation Gain`)) %>%
    mutate(`Remediation Gain (%)` = round(`Remediation Gain`/Potential * 100,0),
           Habitat = stringr::str_replace_all(Habitat, 'Ha', '(ha)'),
           Habitat = stringr::str_replace_all(Habitat, 'Km', '(km)'),
           Habitat = stringr::str_replace_all(Habitat, 'Lakereservoir', 'Lake and Reservoir'),
           Habitat = stringr::str_replace_all(Habitat, 'Spawningrearing ', 'Spawning and Rearing ')) %>%
    select(-remove)
  return(tab_joined)
}


