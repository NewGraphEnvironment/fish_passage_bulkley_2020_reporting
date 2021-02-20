##this is for as we work through
# preview_chapter('index.Rmd')
# preview_chapter('0300-method.Rmd')
# preview_chapter('0400-results.Rmd')
# preview_chapter('0200-background.Rmd')
preview_chapter('0800-appendix-003139.Rmd') #mcquarrie
preview_chapter('0800-appendix-124500.Rmd') #helps
preview_chapter('0800-appendix-195290.Rmd')
preview_chapter('0800-appendix-197360.Rmd')
preview_chapter('0800-appendix-195290.Rmd')
preview_chapter('0800-appendix-197640.Rmd') #buck
preview_chapter('0800-appendix-197658.Rmd')
preview_chapter('0800-appendix-197663.Rmd')
preview_chapter('0800-appendix-197665.Rmd') #barren
preview_chapter('0800-appendix-197667.Rmd') #moan
preview_chapter('0800-appendix-197668.Rmd') #coffin
preview_chapter('0800-appendix-123445.Rmd')
preview_chapter('0200-background.Rmd')
preview_chapter('0400-results.Rmd')
preview_chapter('index.Rmd')
##this is how we clean up our bib file.  We need to find a way to add together the packages.bib file with the book.bib file first though.
# citr::tidy_bib_file(
#   rmd_file = "Elk-River-Fish-Passage-2020.Rmd",
#   messy_bibliography = 'book.bib',
#   file = 'book_tidy.bib')

##we also need to change all the date header to year in the bib file so that it can be run by our pdf maker
##i did this by hand last time but would be good to automate!!!



#######################################################################################
##to build both a paged html version and a gitbook follow the steps below


##change your VErsion #

##move the phase 1 appendix out of the main directory to a backup file
file.rename('0600-appendix.Rmd', 'data/0600-appendix.Rmd')

#################################################################################################
##go to the index.Rmd and change html_on <- FALSE
#################################################################################################


##   then make our printable pdf
rmarkdown::render_site(output_format = 'pagedown::html_paged',
                       encoding = 'UTF-8')


##  move it to the docs folder so that it can be seen by the download button
file.rename('Bulkley.html', 'docs/Bulkley.html')

##now we need to print the docs/Elk.html file to Elk.pdf with chrome.  We should automate this step.  Do in browser for now
openHTML('docs/Bulkley.html')

##move the phase 1 appendix back to main directory
file.rename('data/0600-appendix.Rmd', '0600-appendix.Rmd')

#################################################################################################
##go to the index.Rmd and change html_on <- TRUE
#################################################################################################


##  make the site
source('R/photos-extract-metadata.R')
rmarkdown::render_site(output_format = 'bookdown::gitbook', encoding = 'UTF-8')



##########################################make Phase 1 appendix seperately
#################################################################################################
##we need a workflow to print the Phase 1 attachment
files_to_move <- list.files(pattern = ".Rmd$") %>%
  stringr::str_subset(., 'index|Elk|0600', negate = T)
files_destination <- paste0('hold/', files_to_move)

mapply(file.rename, from = files_to_move, to = files_destination)

##this is hacky but hash out the following from the functions.R file print_tab_summary_all function
# kableExtra::add_footnote(label = '<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>', escape = F, notation = 'none')

##   then make our printable pdf
rmarkdown::render_site(output_format = 'pagedown::html_paged', encoding = 'UTF-8')

##  move it to the docs folder so that it can be in the same place as the report
file.rename('Bulkley.html', 'docs/Attachment_3_Phase_1_Data_and_Photos.html')

##move the files from the hold file back to the main file
mapply(file.rename, from = files_destination, to = files_to_move)

##go to the docs folder - print the attachment to pdf

##now get rid of the first 5 pages
length <- pdf_length(paste0(getwd(), "/docs/Attachment_3_Phase_1_Data_and_Photos_prep.pdf"))

pdf_subset(paste0(getwd(), "/docs/Attachment_3_Phase_1_Data_and_Photos_prep.pdf"),
           pages = 7:length, output = paste0(getwd(), "/docs/Attachment_3_Phase_1_Data_and_Photos.pdf"))

##clean out the old file
file.remove(paste0(getwd(), "/docs/Attachment_3_Phase_1_Data_and_Photos_prep.pdf"))
