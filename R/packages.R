# install.packages('pak')

pkgs_cran <- c(
  'tidyverse',
  'readwritesqlite',
  'sf',
  'readxl',
  'janitor',
  'leafem',
  'leaflet',
  # 'plotKML',
  'httr',
  'RPostgres',
  # 'RPostgreSQL',
  'DBI',
  'magick',
  'bcdata',
  'jpeg',
  'datapasta',
  'knitr',
  'data.table',
  'lubridate',
  'forcats',
  'bookdown',
  'fasstr',
  # flextable,
  'english',
  'leaflet.extras',
  'ggdark',
  'pdftools',
  'geojsonsf',
  'bit64', ##to make integer column type for pg
  "pagedown"
  # gert  ##to track git moves
  ##leafpop I think
  )

pkgs_gh <- c(
  ## 2024 update, this prevents black text in dark mode
  "haozhu233/kableExtra@a9c509a",
  'poissonconsulting/fwapgr',
  "newgraphenvironment/fpr"

)

pkgs_all <- c(pkgs_cran,
              pkgs_gh)

# install or upgrade all the packages with pak
if(params$update_packages){
  lapply(pkgs_all,
         pak::pkg_install,
         ask = FALSE)
}

pkgs_ld <- c(pkgs_cran,
             basename(pkgs_gh))

lapply(pkgs_ld,
       require,
       character.only = TRUE)

