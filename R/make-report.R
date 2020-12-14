##make the book and clean up the references

##biuld the file
rmarkdown::render_site(output_format = 'bookdown::gitbook', encoding = 'UTF-8')


##this is how we clean up our bib file.  We need to find a way to add together the packages.bib file with the book.bib file first though.
citr::tidy_bib_file(
  rmd_file = "Elk-River-Fish-Passage-2020.Rmd",
  messy_bibliography = 'book.bib',
  file = 'book_tidy.bib')

##we also need to change all the date header to year in the bib file so that it can be run by our pdf maker
##i did this by hand last time but would be good to automate!!!
