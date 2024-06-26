##this is for as we work through
# preview_chapter('index.Rmd')



# if you have changed your bcfishpass model outputs by saving to sqlite with 0282-extract-bcfishpass2...
# you also need to make new html tables to link to in the leaflet map  use 0355-tables-reporting-html.R
# source('R/photos-extract-metadata.R') ##if you added new photos
# source('R/0355-tables-reporting-html.R')  #if you changed the modelling outputs

# 2023 fish sampling data - must have the 2023 Skeena repo cloned to your computer.
fs::file_copy("~/Projects/repo/fish_passage_skeena_2023_reporting/data/habitat_confirmations.xls",
              "data/2023/habitat_confirmations.xls", overwrite = TRUE)


#-----photos from 2023------------------------------------------------------------------------------------------
dir_2023_photos_stub = "~/Library/CloudStorage/OneDrive-Personal/Projects/2023_data/skeena/photos/"
dir_2023_photos <- c("124500", "197360")
dir_repo_photos_stub = "data/2023/photos/"
# copy the directories with purrr::map
purrr::map(dir_2023_photos,
           ~fs::dir_copy(paste0(dir_2023_photos_stub, .x),
                         paste0(dir_repo_photos_stub, .x),
                         overwrite = TRUE))

# Gitbook
library(magrittr)
{
source('R/functions.R')
news_to_appendix()

rmarkdown::render_site(output_format = 'bookdown::gitbook',
                       encoding = 'UTF-8')
}

# pdf
#################################################################################################
##go to the index.Rmd and change gitbook_on <- FALSE
#################################################################################################

##move the phase 1 appendix out of the main directory to a backup file
{
  file.rename('0600-appendix.Rmd', 'data/0600-appendix.Rmd')



  ##   then make our printable pdf
  rmarkdown::render_site(output_format = 'pagedown::html_paged',
                         encoding = 'UTF-8')
  ##  move it to the docs folder so that it can be seen by the download button
  file.rename('Bulkley.html', 'docs/Bulkley.html')

  ##now we need to print the docs/Elk.html file to Elk.pdf with chrome.  We should automate this step.  Do in browser for now
  openHTML('docs/Bulkley.html')

  ##move the phase 1 appendix back to main directory
  file.rename('data/0600-appendix.Rmd', '0600-appendix.Rmd')
}

#make Phase 1 appendix seperately
files_to_move <- list.files(pattern = ".Rmd$") %>%
  stringr::str_subset(., 'index|Bulkley|0600', negate = T)
files_destination <- paste0('hold/', files_to_move)

##move the files
mapply(file.rename, from = files_to_move, to = files_destination)



##this is hacky but hash out the following from the functions.R file print_tab_summary_all function
# kableExtra::add_footnote(label = '<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>', escape = F, notation = 'none')

##   then make our printable pdf
rmarkdown::render_site(output_format = 'pagedown::html_paged', encoding = 'UTF-8')

##  move it to the docs folder so that it can be in the same place as the report
file.rename('Bulkley.html', 'docs/Attachment_3_Phase_1_Data_and_Photos.html')

##move the files from the hold file back to the main file
mapply(file.rename, from = files_destination, to = files_to_move)

#print the attachment to pdf
openHTML('docs/Attachment_3_Phase_1_Data_and_Photos_prep.html')

##now get rid of the first 10 pages
length <- pdftools::pdf_length(paste0(getwd(), "/docs/Attachment_3_Phase_1_Data_and_Photos_prep.pdf"))

pdftools::pdf_subset(paste0(getwd(), "/docs/Attachment_3_Phase_1_Data_and_Photos_prep.pdf"),
           pages = 11:length, output = paste0(getwd(), "/docs/Attachment_3_Phase_1_Data_and_Photos.pdf"))

##clean out the old file
file.remove(paste0(getwd(), "/docs/Attachment_3_Phase_1_Data_and_Photos_prep.pdf"))
file.remove(paste0(getwd(), "/docs/Attachment_3_Phase_1_Data_and_Photos_prep.html"))

