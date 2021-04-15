##need to line up a logo for the report

##resize sern logo
image_read("fig/SERNbc-Logo-HALF.jpg") %>%
  image_scale("1252x439.5") %>%
  image_write(path = "fig/logo_sern.png", format = 'png')


##resize newgraph logo
image_read('fig/logo_html3.png') %>%
  image_scale("626x163.5") %>%
  image_write(path = "fig/logo_newgraph_forcombine.png", format = 'png')

##combine sern and newgraph logos

images_to_combine <- paste0(getwd(), c('/fig/logo_sern.png','/fig/logo_html3.png')) %>%
  image_read() %>%
  image_append(stack = T)  %>%  ##half the original height
  image_write(path = paste0(getwd(), '/fig/logo_combined.png'), format = 'png')


##rotate the porphyr photo
image_read('data/photos/124487/IMG_8245_k_d1_.JPG') %>%
  image_rotate(180) %>%
  image_write(path = "data/photos/124487/IMG_8245_k_d1_rotated.JPG", format = 'jpg')

##resize hctf logo
image_read("fig/hctf_large.jpg") %>%
  image_convert("png") %>%
  image_scale("200") %>%
  image_write(path = "fig/logo_hctf.png", format = 'png')

##combine newgraph and hctf logos
images_to_combine <- paste0(getwd(), c('/fig/logo_hctf.png','/fig/logo_html3.png')) %>%
  image_read() %>%
  image_append(stack = T)  %>%  ##half the original height
  image_write(path = paste0(getwd(), '/fig/logo_hctf_newg.png'), format = 'png')
