
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


# make_photo_comp_cv <- function(site_id){
#   photos_images1 <- list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = T) %>%
#     stringr::str_subset(., 'barrel|outlet|upstream') %>%
#     image_read()
#   photos_images2 <- list.files(path = paste0(getwd(), '/data/photos/', site_id), full.names = T) %>%
#     stringr::str_subset(., 'downstream|road|inlet') %>%
#     image_read()
#   photos_stack1 <-image_append(image_scale(photos_images1, "x420")) ##1/3 the width 373.33 and half the original height
#   photos_stack2 <- image_append(image_scale(photos_images2, "x420"))
#   photos_stack <- c(photos_stack1, photos_stack2)
#   photos_stacked <- image_append(image_scale(photos_stack), stack = T)
#   image_write(photos_stacked, path = paste0(getwd(), '/data/photos/', site_id, '/crossing_all.JPG'), format = 'jpg')
# }

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


####------my_kable-------------------------------
my_kable_scroll <- function(dat, caption_text = ''){
  dat %>%
    kable(caption = caption_text) %>%
    kableExtra::kable_styling(c("condensed"), full_width = T, font_size = 11) %>%
    kableExtra::scroll_box(width = "100%", height = "500px")
}

my_kable_scroll_no_height <- function(dat, caption_text = ''){
  dat %>%
    kable(caption = caption_text) %>%
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
    kable(caption = caption_text) %>%
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
