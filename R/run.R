##this is for as we work through
# preview_chapter('index.Rmd')
# preview_chapter('0300-method.Rmd')
preview_chapter('0400-results.Rmd')
# preview_chapter('0200-background.Rmd')
# preview_chapter('0800-appendix-050155.Rmd')
# preview_chapter('0800-appendix-050181.Rmd') ##nupku
# preview_chapter('0800-appendix-050185.Rmd')
# preview_chapter('0800-appendix-062423.Rmd')
# preview_chapter('0800-appendix-062425.Rmd')
# preview_chapter('0800-appendix-062516.Rmd')
# preview_chapter('0800-appendix-197534.Rmd')
# preview_chapter('0800-appendix-197533.Rmd')
# preview_chapter('0800-appendix-197555.Rmd')



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
