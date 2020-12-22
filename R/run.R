##this is for as we work through
preview_chapter('index.Rmd')
preview_chapter('0800-appendix-050155.Rmd')
preview_chapter('0800-appendix-050185.Rmd')
preview_chapter('0800-appendix-062423.Rmd')
preview_chapter('0800-appendix-062425.Rmd')
preview_chapter('0800-appendix-062516.Rmd')
preview_chapter('0800-appendix-197533.Rmd')
preview_chapter('0800-appendix-197555.Rmd')


##make our printable pdf
rmarkdown::render_site(output_format = 'pagedown::html_paged', encoding = 'UTF-8')

##copy it to the docs folder so that it can be downloaded
file.copy('Elk.html', 'docs/Elk.html')

##make the site
rmarkdown::render_site(output_format = 'bookdown::gitbook', encoding = 'UTF-8')
