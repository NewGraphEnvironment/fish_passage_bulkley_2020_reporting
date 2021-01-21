##this is for as we work through
# preview_chapter('index.Rmd')
# preview_chapter('0300-method.Rmd')
# preview_chapter('0400-results.Rmd')
# preview_chapter('0200-background.Rmd')
preview_chapter('0800-appendix-197360.Rmd')



##this is how we clean up our bib file.  We need to find a way to add together the packages.bib file with the book.bib file first though.
# citr::tidy_bib_file(
#   rmd_file = "Elk-River-Fish-Passage-2020.Rmd",
#   messy_bibliography = 'book.bib',
#   file = 'book_tidy.bib')

##we also need to change all the date header to year in the bib file so that it can be run by our pdf maker
##i did this by hand last time but would be good to automate!!!

#######################################################################################
##for a prod build


##change your VErsion #

##move the phase 1 appendix out of the main directory to a backup file
file.rename('0600-appendix.Rmd', 'data/0600-appendix.Rmd')

##go to the index.Rmd doc and turn
#html_on <- FALSE and change
#font_set <- 9
#photo_width <- "80%"

##   then make our printable pdf
rmarkdown::render_site(output_format = 'pagedown::html_paged', encoding = 'UTF-8')

##  move it to the docs folder so that it can be seen by the download button
file.rename('Elk.html', 'docs/Elk.html')

##move the phase 1 appendix back to main directory
file.rename('data/0600-appendix.Rmd', '0600-appendix.Rmd')

##go to the index.Rmd doc and turn
#html_on <- TRUE and change
#font_set <- 11
#photo_width <- "100%"

##  make the site
rmarkdown::render_site(output_format = 'bookdown::gitbook', encoding = 'UTF-8')


##sub in the title page
length <- pdf_length(paste0(getwd(), "/docs/Elk.pdf"))

pdf_subset(paste0(getwd(), "/docs/Elk.pdf"),
           pages = 2:length, output = paste0(getwd(), "/docs/Elk2.pdf"))

pdf_combine(c(paste0(getwd(), "/docs/title_page.pdf"),
  paste0(getwd(), "/docs/Elk2.pdf")),
            output = paste0(getwd(), "/docs/Elk3.pdf"))

file.rename(paste0(getwd(), "/docs/Elk3.pdf"), paste0(getwd(),"/docs/Elk.pdf"))

file.remove(paste0(getwd(), "/docs/Elk2.pdf"))
