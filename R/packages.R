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
  'poissonconsulting/fwapgr',
  "newgraphenvironment/fpr",
  "haozhu233/kableExtra@a9c509a" ## 2024 update, this prevents black text in dark mode
)

pkgs_all <- c(pkgs_cran,
              pkgs_gh)

pkgs_ld <- c(pkgs_cran,
             basename(pkgs_gh))

lapply(pkgs_ld,
       require,
       character.only = TRUE)

