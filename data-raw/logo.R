# Create logo for nuts package

rm(list = ls())
require(tidyverse)
require(hexSticker)
require(magick)


euronuts <- image_read("data-raw/squirrel_euronut_cartoon.png")
plot(euronuts)

s= sticker(euronuts, package="nuts", p_size=25, p_color = "#003399",p_y = 1.6,
           s_x=1, s_y=0.8, s_width=1.25, s_height = 1.25,
           h_fill= "#ffcc00", h_color = "#003399",
           filename="nuts_squirrel_eu_nut.png")
plot(s)



usethis::use_logo("nuts_squirrel_eu_nut.png")
