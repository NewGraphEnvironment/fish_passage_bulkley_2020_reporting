##move the photos and spreadsheet ready for submission to pscis

##we need to have photo folders for each site but some of our phase 2 sites now have pscis_crossing_id s and
##need their my_crossing_reference photo folders copied and renamed.

source('R/packages.R')
source('R/functions.R')
# source('R/tables.R')

# targetdir = paste0("D:/PSCIS_bulkley_2020_reassessments/")
# dir.create(targetdir)

##path to the photos
path <- paste0(getwd(), '/data/photos/')


##use the pscis spreadsheet to make the folders to copy the photos to
# d <- import_pscis(workbook_name = 'pscis_phase1.xlsm')
d <- import_pscis(workbook_name = 'pscis_phase2.xlsm') %>%
  filter(is.na(pscis_crossing_id))

##join to our new pscis_id
##this is made from load-crossings-xref.R
xref_pscis_my_crossing_modelled <- readr::read_csv(file = paste0(getwd(), '/data/extracted_inputs/xref_pscis_my_crossing_modelled.csv'))

d <- left_join(
  d,
  xref_pscis_my_crossing_modelled,
  by = c('my_crossing_reference' = 'external_crossing_reference')
)


# folderstocopy<- d$my_crossing_reference %>% as.character()
folderstocopy<- d$my_crossing_reference %>% as.character()

folders_new_names <- d$stream_crossing_id %>% as.character()

path_to_photos <- paste0(getwd(), '/data/photos/', folderstocopy)

folderstocreate<- paste0(path, folders_new_names)

##create the folders
lapply(folderstocreate, dir.create)


paths_to_copy <- function(target){
  list.files(path = target,
             pattern = ".JPG$",
             recursive = TRUE,
             full.names = T,
             include.dirs = T)
    # stringr::str_subset(., 'barrel|outlet|upstream|downstream|road|inlet')
}

photo_names_to_copy <- function(target){
  list.files(path = target,
             pattern = ".JPG$",
             recursive = TRUE,
             full.names = F,
             include.dirs = T)
    # stringr::str_subset(., 'barrel|outlet|upstream|downstream|road|inlet')
}


filestocopy_list <- path_to_photos %>%
  purrr::map(paths_to_copy)

change_file_names <- function(filestocopy, filename_before, filename_after){
  gsub(filestocopy, pattern = filename_before, replacement = filename_after)
}

# filestopaste_list <- filestocopy_list %>%
#   map(change_file_names)

filestopaste_list <- mapply(change_file_names, filestocopy_list, folderstocopy, folders_new_names)

copy_over_photos <- function(filescopy, filespaste){
  file.copy(from=filescopy, to=filespaste,
            overwrite = T,
            copy.mode = TRUE)
}

mapply(copy_over_photos, filescopy =  filestocopy_list,
       filespaste = filestopaste_list)




