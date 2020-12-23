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
    stringr::str_subset(., 'barrel|outlet|upstream') %>%
    image_read()
  photos_images2 <- list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = T) %>%
    stringr::str_subset(., 'downstream|road|inlet') %>%
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
    kableExtra::add_footnote(label = paste0('Photos: ', photos[[1]], photos[[2]]), notation = 'none') %>%
    kableExtra::add_footnote(label = '<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>', escape = F, notation = 'none')
}

####------my_kable-------------------------------
my_kable_scroll <- function(dat, caption_text = ''){
  dat %>%
    kable(caption = caption_text, booktabs = T) %>%
    kableExtra::kable_styling(c("condensed"), full_width = T, font_size = 11) %>%
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

my_kable <- function(dat, caption_text = ''){
  dat %>%
    kable(caption = caption_text, booktabs = T) %>%
    kableExtra::kable_styling(c("condensed"), full_width = T, font_size = 11)
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

##summary table
print_tab_summary <- function(dat = pscis2, site = my_site, site_photo_id = my_site, font = 11){
  make_tab_summary(df = dat %>% filter(pscis_crossing_id == site)) %>%
    kable(caption = paste0('Summary of fish passage reassessment for PSCIS crossing ', site, '.'), booktabs = T) %>%    #
    # kableExtra::add_footnote(label = paste0('Comments: ', pscis2 %>% filter(pscis_crossing_id == my_site) %>%
    #                                           pull(assessment_comment)), notation = 'none') %>% #this grabs the comments out
    kableExtra::add_footnote(label = paste0('Photos: ',
                                            paste0('![](data/photos/', site_photo_id, '/crossing_all.JPG)')), notation = 'none') %>%
    kableExtra::kable_styling(c("condensed"), full_width = T, font_size = font)
    # kableExtra::scroll_box(width = "100%", height = "500px") ##not scrolling to simplify our pagedown output
}

print_tab_cost_mult <- function(dat = tab_cost_rd_mult_report, ...){
  tab_cost_rd_mult_report %>%
  my_kable()
}
