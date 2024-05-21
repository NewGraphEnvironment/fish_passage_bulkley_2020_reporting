# install.packages('pak')

pkgs_cran <- c(
  'tidyverse',
  'readwritesqlite',
  'sf',
  'readxl',
  'janitor',
  'leafem',
  'leaflet',
  'plotKML',
  'kableExtra',
  'httr',
  'RPostgres',
  'RPostgreSQL',
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
  'bit64' ##to make integer column type for pg
  # gert  ##to track git moves
  ##leafpop I think
  )

pkgs_gh <- c(
  'poissonconsulting/fwapgr',
  "newgraphenvironment/fpr",
  "haozhu233/kableExtra", ## 2024 update, this prevents black text in dark mode
  "crsh/citr",
  "rstudio/pagedown"
)

pkgs_all <- c(pkgs_cran,
              pkgs_gh)

pkgs_ld <- c(pkgs_cran,
             basename(pkgs_gh))

lapply(pkgs_ld,
       require,
       character.only = TRUE)

