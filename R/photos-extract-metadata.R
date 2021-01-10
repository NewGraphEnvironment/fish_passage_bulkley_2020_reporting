##here we need to pull all the metadata from all the marked photos so we can use it to have our photos show on the leaflet map
source('R/packages.R')


make_photo_metadata_list <- function(input_file){
  exifr::read_exif(input_file,recursive=T) %>%
    purrr::set_names(., nm = tolower(names(.))) %>%
    select(filename, sourcefile, gpslatitude, gpslongitude) %>%
    mutate(url  = paste0('https://github.com/NewGraphEnvironment/fish_passage_elk_2020_reporting_cwf/raw/master/',
                                                sourcefile),
                                  base = tools::file_path_sans_ext(filename))
}

files <- paste0('data/photos/',list.files('data/photos'))

photo_metadata_list <- files %>%
  map_df(make_photo_metadata_list)


##now we need to subset the ones that contain _k_ or crossings_all
photo_metadata_list2 <- photo_metadata_list %>%
  filter(filename %like% '_k')

##burn this to a csv so we can pull into the map
write.csv(photo_metadata_list2, file = 'data/photo_metadata.csv', row.names = F)
