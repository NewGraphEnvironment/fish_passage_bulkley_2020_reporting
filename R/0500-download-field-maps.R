require(RCurl)
library(XML)
library(magrittr)

##we are going to do this a bit sloppy and set a working directory
# setwd("C:/Users/allan/OneDrive/New_Graph/Current/2019-023_Bulkley_fish_passage/field_maps/moe_2018")

##increase your timeout limit to allow download of bigger files
options(timeout=180)


url = "https://www.hillcrestgeo.ca/outgoing/fishpassage/projects/bulkley/"

filenames <- getURL(url,verbose=TRUE,ftp.use.epsv=TRUE, dirlistonly = TRUE)
filenames <- getHTMLLinks(filenames, xpQuery = "//a/@href[contains(., '.pdf')]") #https://stackoverflow.com/questions/32213591/list-files-on-http-ftp-server-in-r

# ##or more precisely
# getHTMLLinks(
#   filenames,
#   xpQuery = "//a/@href['.pdf'=substring(., string-length(.) - 3)]"
# )

for (filename in filenames) {
  download.file(paste(url, filename, sep = ""), paste(getwd(), "/maps/", filename,
                                                      sep = ""), mode = "wb")
}

##get a list of the files and zip them up together into
files_to_zip <- paste0("maps/", list.files(path = "maps/", pattern = "\\.pdf$"))  ##this used to includes the planning file which we don't want to do so watch out
zip::zipr("data/Attachment_2_maps.zip", files = files_to_zip)

